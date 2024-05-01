#' Package Dependencies module's UI.
#'
#' @param id a module id name
#' @keywords internal
#'
packageDependenciesUI <- function(id) {
  uiOutput(NS(id, "package_dependencies_ui"))
}

#' Package Dependencies module's server logic
#'
#' @param id a module id name
#' @param selected_pkg placeholder
#' @param user placeholder
#' @param parent the parent (calling module) session information
#'
#' @import dplyr
#' @importFrom DT formatStyle renderDataTable
#' @importFrom formattable as.datatable csscolor formattable formatter style
#' @importFrom glue glue
#' @importFrom purrr map_df
#' @importFrom rlang warn is_empty
#' @importFrom shiny removeModal showModal tagList 
#' @importFrom shinyjs click 
#' @importFrom stringr str_extract str_replace
#' @importFrom shinyWidgets materialSwitch
#'
#' @keywords internal
#'
packageDependenciesServer <- function(id, selected_pkg, user, credentials, parent) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    tabready <- reactiveVal(value = NULL)
    depends  <- reactiveVal(value = NULL)
    suggests <- reactiveVal(value = NULL)
    revdeps  <- reactiveVal(value = NULL)
    rev_pkg  <- reactiveVal(value = 0L)
    toggled  <- reactiveVal(value = 0L)
    pkg_updates <- reactiveValues()
    cards    <- reactiveVal(value = NULL)

    observeEvent(list(parent$input$tabs, parent$input$metric_type, selected_pkg$name()), {
      req(selected_pkg$name())
      req(selected_pkg$name() != "-")
      
      if (parent$input$tabs == "Package Metrics" & parent$input$metric_type == "dep") {
        tabready(1L)
      } else {
        tabready(0L)
      }
     
    })
    
    pkgref <- eventReactive(list(selected_pkg$name(), tabready()), {
      req(selected_pkg$name())
      req(selected_pkg$name() != "-")
      req(tabready() == 1L)
      
      get_assess_blob(selected_pkg$name(), metric_lst = c("dependencies", "suggests", "reverse_dependencies"))
    })
    
    observeEvent(list(pkgref(), toggled()), {
      req(pkgref())
      tryCatch(
        expr = {
          depends(pkgref()$dependencies[[1]] %>% dplyr::as_tibble() %>% 
                    mutate(package = stringr::str_replace(package, "\n", " ")) %>%
                    mutate(name = stringr::str_extract(package, "^((([[A-z]]|[.][._[A-z]])[._[A-z0-9]]*)|[.])")))
        },
        error = function(e) {
          msg <- paste("Detailed dependency information is not available for package", selected_pkg$name())
          rlang::warn(msg)
          rlang::warn(paste("info:", e))
          depends(dplyr::tibble(package = character(0), type = character(0), name = character(0)))
        }
      )
      tryCatch(
        expr = {
          suggests(pkgref()$suggests[[1]] %>% dplyr::as_tibble()%>% 
                     mutate(package = stringr::str_replace(package, "\n", " ")) %>%
                     mutate(name = stringr::str_extract(package, "^((([[A-z]]|[.][._[A-z]])[._[A-z0-9]]*)|[.])")))
        },
        error = function(e) {
          msg <- paste("Detailed suggests information is not available for package", selected_pkg$name())
          rlang::warn(msg)
          rlang::warn(paste("info:", e))
          suggests(dplyr::tibble(package = character(0), type = character(0), name = character(0)))
        }
      )
      # this is so the dependencies is also a 0x2 tibble like suggests
      if (rlang::is_empty(pkgref()$dependencies[[1]])) depends(dplyr::tibble(package = character(0), type = character(0), name = character(0)))
        
      revdeps(pkgref()$reverse_dependencies[[1]] %>% as.vector())
      
      # send either depends() or both to build_dep_cards(), depending on toggled()
      if (toggled() == 0L) {
        cards(build_dep_cards(data = depends(), loaded = session$userData$loaded2_db()$name, toggled = 0L))
      } else {
        cards(build_dep_cards(data = dplyr::bind_rows(depends(), suggests()), loaded = session$userData$loaded2_db()$name, toggled = 1L))
      }
    })
    
    pkg_df <- eventReactive(list(selected_pkg$name(), tabready(), depends(), toggled()), {
      req(selected_pkg$name())
      req(selected_pkg$name() != "-")
      req(tabready() == 1L)
      req(depends())
      req(suggests())

      if (nrow(depends()) == 0) {
        # packages like curl, magrittr will appear here instead of in tryCatch() above
        msg <- paste("Detailed dependency information is not available for package", selected_pkg$name())
        rlang::warn(msg)
        if (toggled() == 0L || nrow(suggests()) == 0) {
        return(dplyr::tibble(package = character(0), type = character(0), name = character(0)))
          } else {
            pkginfo <- suggests() %>%  as_tibble() 
          } 
      } else {
        pkginfo <- dplyr::bind_rows(depends(), suggests()) %>% as_tibble()
      }
      pkginfo <- pkginfo %>% 
          mutate(package = stringr::str_replace(package, "\n", " ")) %>%
          # a syntactically valid name:
          # consists of letters, numbers and the dot or underline characters 
          # and starts with a letter or the dot not followed by a number. 
          # Names such as '".2way"' are not valid
          mutate(name = stringr::str_extract(package, "^((([[A-z]]|[.][._[A-z]])[._[A-z0-9]]*)|[.])"))
        
      if(toggled() == 0L) 
        pkginfo <- filter(pkginfo, type != "Suggests")
      
      if (!isTruthy(session$userData$repo_pkgs())) {
        if (isTRUE(getOption("shiny.testmode"))) {
          session$userData$repo_pkgs(purrr::map_dfr(test_pkg_refs, ~ as.data.frame(.x, col.names = c("Package", "Version", "Source"))))
        } else {
          session$userData$repo_pkgs(as.data.frame(utils::available.packages()[,1:2]))
        }
      }
      
      purrr::map_df(pkginfo$name, ~get_versnScore(.x, session$userData$loaded2_db(), session$userData$repo_pkgs())) %>% 
        right_join(pkginfo, by = "name") %>% 
        select(package, type, name, version, score) %>%
        arrange(name, type) %>% 
        distinct()
      
    }, ignoreInit = TRUE)
    
    data_table <- eventReactive(pkg_df(), {
      add_buttons_to_table(pkg_df(), ns("select_button")) %>% 
        # remove action button if there is nothing to review
        mutate(Actions = if_else(identical(package, character(0)) | name %in% c(rownames(installed.packages(priority = "base"))), "", Actions)) %>% 
        # if package name not yet loaded, switch the actionbutton to fa-upload
        mutate(Actions = if_else(!name %in% session$userData$loaded2_db()$name, gsub("fas fa-arrow-right fa-regular", "fas fa-upload fa-solid", Actions), Actions))
    })
    
    revdeps_local <- reactive({
      df <- session$userData$loaded2_db()
      req(df, df$name)
      req(selected_pkg$name())

      revdeps <-
        df %>%
        dplyr::filter(name %in% revdeps()) %>%
        dplyr::pull(name) %>%
        get_assess_blob(metric_lst = "suggests")
      
      if (nrow(revdeps) == 0) return(dplyr::left_join(bind_rows(revdeps, list(suggests = logical())), df, by = "name"))
      
      revdeps %>%
        dplyr::mutate(suggests = purrr::map_lgl(suggests, ~ any(grepl(selected_pkg$name(), .x$package)))) %>%
        dplyr::left_join(df, by = "name")
    })
    
    table_revdeps_local <- reactive({
      req(revdeps_local())
      
      revdeps_local() %>%
        dplyr::filter(toggled() == 1L | !suggests) %>%
        dplyr::select(-suggests) %>%
        add_buttons_to_table(ns("go_to_revdep"))
    })
    
    # Create metric grid card.
    metricGridServer(id = 'metricGrid', metrics = cards) 
    
    # Render Output UI for Package Dependencies.
    output$package_dependencies_ui <- renderUI({
      
      # Lets the user know that a package needs to be selected.
      if (identical(selected_pkg$name(), character(0))) {
        showHelperMessage()
      } else {
        req(depends())

        tagList(
          br(), br(),
          div(id = "dep_infoboxes", metricGridUI(NS(id, 'metricGrid'))),
          br(),
          fluidRow(
            column(4, 
                   tags$strong(
                     glue::glue("First-order dependencies for package: ", {selected_pkg$name()})
                   )
            ),
            column(2,
                   shinyWidgets::materialSwitch(
                     inputId =  ns("incl_suggests"),
                     label = "Include Suggests",
                     value = toggled(),
                     inline = TRUE,
                     status = "success"
                   )
            ),
            column(2,
                   if (pkg_updates$render_upload) {
                     actionButton(
                       inputId =  ns("update_all_packages"),
                       label = "Upload all",
                       icon = icon("fas fa-upload", class = "fa-regular", lib = "font-awesome"),
                       size = "xs",
                       style = "height:30px; padding-top:1px;"
                     )
                   } 
            )
          ),
          br(),
          # remove DT "search:" rectangle
          tags$head(
            tags$style(type = "text/css", ".dataTables_filter {display: none;    }")
          ),
          fluidRow(
            column(
              width = 8,
              DT::renderDataTable(server = FALSE, {
                datatable_custom(data_table())                      
              })
            )
          ), 
          br(), br(),
          h3(glue::glue("All reverse Dependencies: {length(revdeps())}"), style = "text-align: left;"),
          br(),
          fluidRow(
            column(
              width = 8,
              h4(glue::glue("Reverse Dependencies available in database: {nrow(table_revdeps_local()) %||% 0}"), style = "text-align: left;"),
              br(), 
              DT::renderDataTable({
                datatable_custom(
                  table_revdeps_local(), 
                  colnames = c("Package", "Version", "Score", "Review Package"),
                  hide_names = NULL
                )
              }),
              br(), br(),
              wellPanel(
                renderText(revdeps() %>% sort()),
                style = "max-height: 500px; overflow: auto"
              )
            )
          ),
          br(), br(),
          fluidRow(div(id = "comments_for_dep",
                       if ("general_comment" %in% unlist(credentials$privileges[user$role], use.names = FALSE)) addCommentUI(NS(id, 'add_comment')),
                       viewCommentsUI(NS(id, 'view_comments')))
          )
        ) # taglist
      }
    }) # renderUI
    
    # the package selected in the table to either browser to or to upload:
    selected_package <- reactiveVal() 
    pkgname <- reactiveVal()
    observeEvent(input$select_button, {
      req(pkg_df())
      selectedRow <- as.numeric(strsplit(input$select_button, "_")[[1]][2])
      selected_package(with(pkg_df(), name[selectedRow]))
      pkgname("-")
    })
    observeEvent(input$go_to_revdep, {
      req(table_revdeps_local())
      selectedRow <- as.numeric(strsplit(input$go_to_revdep, "_")[[1]][2])
      selected_package(with(table_revdeps_local(), name[selectedRow]))
    })
    
    observeEvent(selected_package(),
     {
       rev_pkg(0L)
       pkgname("-")
       # to ensure that if the same package is clicked on, this observeEvent will 
       # run again:
       on.exit(selected_package(NULL)) 
       if (!selected_package() %in% session$userData$loaded2_db()$name) {
         pkgname(selected_package())
         shiny::showModal(modalDialog(
           size = "l",
           easyClose = TRUE,
           h5("Package not Loaded", style = "text-align: center !important"),
           hr(),
           br(),
           fluidRow(
             column(
               width = 12,
               "Please confirm to load this package: ", span(class = "text-info", selected_package()),
             )
           ),
           br(),
           footer = tagList(
             actionButton(NS(id, "confirm"), "Load Package"),
             actionButton(NS(id, "cancel"), "Cancel")
           )
         ))
       } else {
         # update sidebar-select_pkg
         updateSelectizeInput(
           session = parent,
           inputId = "sidebar-select_pkg",
           choices = c("-", session$userData$loaded2_db()$name),
           selected = selected_package()
         )
       }
     },
     ignoreInit = TRUE
    ) # observeEvent
    
    
    observeEvent(input$confirm, {
      shiny::removeModal()
      
      session$userData$trigger_events$upload_pkgs <- pkgname()
      
      session$onFlushed(function() {
        rev_pkg(1L)
      })
    })
    
    # Close modal if user cancels decision submission.
    observeEvent(input$cancel, {
      shiny::removeModal()
    })
    
    names_vect <- eventReactive(rev_pkg(), {
      req(rev_pkg() == 1L)
      dbSelect("SELECT name FROM package")$name
    })
    
    observeEvent(names_vect(), {
      req(names_vect())
      pkg_name <- names_vect()[length(names_vect())]
      updateSelectizeInput(
        session = parent,
        inputId = "sidebar-select_pkg",
        choices = c("-", names_vect()),
        selected = pkg_name
      )
    })
    
    observe({
      if (nrow(pkg_df()) > 0) {
        pkg_updates$pkgs_update <- pkg_df() %>%
          dplyr::filter(is.na(score) | score == "") %>%
          dplyr::filter(!name %in% c(rownames(installed.packages(priority = "base"))))

        pkg_updates$render_upload <- nrow(pkg_updates$pkgs_update) > 0
      } else {
        pkg_updates$render_upload <- FALSE
      }
    })
    
    observeEvent(input$update_all_packages, {
      req(pkg_df(), session$userData$loaded2_db(), pkg_updates)
      rev_pkg(0L)

      pkgname(pkg_updates$pkgs_update$name)
      shiny::showModal(
        modalDialog(
          size = "l",
          easyClose = TRUE,
          title = "Upload all packages?",
          p(glue::glue("Do you want to upload {nrow(pkg_updates$pkgs_update)} package(s)?")),
           footer = tagList(
             actionButton(NS(id, "confirm"), "Load Package(s)"),
             actionButton(NS(id, "cancel"), "Cancel")
           )
        )
      )
    })

    observeEvent(input$incl_suggests, {
      req(pkg_df(), session$userData$loaded2_db())
      if(input$incl_suggests == TRUE | toggled() == 1L) toggled(1L - isolate(toggled()))
    })
    
    # Call module that creates section to add comments.
    comment_added <- addCommentServer(id = "add_comment",
                                      metric_abrv = 'dep',
                                      user = user,
                                      credentials = credentials,
                                      pkg_name = selected_pkg$name)
    
    comments <- eventReactive(list(comment_added(), selected_pkg$name()), {
      get_dep_comments(selected_pkg$name())
    })
    
    # Call module that creates comments view.
    viewCommentsServer(id = "view_comments",
                       comments = comments,
                       pkg_name = selected_pkg$name)
    
    list(
      comments = comments,
      comment_added = comment_added
    )
  }) # moduleServer
}
