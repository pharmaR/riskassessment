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
#' @param changes a reactive value integer count
#' @param parent the parent (calling module) session information
#'
#' @import dplyr
#' @importFrom DT formatStyle renderDataTable
#' @importFrom formattable as.datatable csscolor formattable formatter style
#' @importFrom glue glue
#' @importFrom purrr map_df
#' @importFrom rlang warn
#' @importFrom shiny removeModal showModal tagList
#' @importFrom shinyjs click
#' @importFrom stringr str_extract str_replace
#'
#' @keywords internal
#'
packageDependenciesServer <- function(id, selected_pkg, user, changes, parent) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    cran_pkgs <- as.data.frame(available.packages("https://cran.rstudio.com/src/contrib")[, 1:2])
    
    loaded2_db <- eventReactive(list(selected_pkg$name(), changes()), {
      dbSelect("SELECT name, version, score FROM package")
    })
    
    # used for adding action buttons to data_table
    shinyInput <- function(FUN, len, id, ...) {
      inputs <- character(len)
      for (i in seq_len(len)) {
        inputs[i] <- as.character(FUN(paste0(id, i), ...))
        # change icon from arrow to upload if not loaded into db yet
        pkg_name <- pkg_df()[i, 3] %>% pull()
        if (!pkg_name %in% loaded2_db()$name) {
          inputs[i] <- gsub("fas fa-arrow-right fa-regular", "fas fa-upload fa-solid", inputs[i])
        }
      }
      inputs
    }
    
    tabready <- reactiveVal(value = NULL)
    depends <- reactiveVal(value = NULL)
    revdeps <- reactiveVal(value = NULL)
    rev_pkg <- reactiveVal(value = NULL)
    
    observeEvent(list(parent$input$tabs, parent$input$metric_type, selected_pkg$name()), {
      req(selected_pkg$name())
      req(selected_pkg$name() != "-")
      
      if (parent$input$tabs == "Package Metrics" & parent$input$metric_type == "dep") {
        tabready(1L)
      } else {
        tabready(0L)
      }
    })
    
    pkgref <- eventReactive(list(selected_pkg$name(), changes(), tabready()), {
      req(selected_pkg$name())
      req(selected_pkg$name() != "-")
      req(tabready() == 1L)
      
      get_assess_blob(selected_pkg$name())
    })
    
    observeEvent(pkgref(), {
      req(pkgref())
      
      tryCatch(
        expr = {
          depends(pkgref()$dependencies[[1]] %>% dplyr::as_tibble())
        },
        error = function(e) {
          msg <- paste("Detailed dependency information is not available for package", selected_pkg$name())
          rlang::warn(msg)
          rlang::warn(paste("info:", e))
          depends(dplyr::tibble(package = character(0), type = character(0), name = character(0)))
        }
      )
      revdeps(pkgref()$reverse_dependencies[[1]] %>% as.vector())
    })
    
    pkg_df <- eventReactive(list(selected_pkg$name(), tabready(), depends()), {
      req(selected_pkg$name())
      req(selected_pkg$name() != "-")
      req(tabready() == 1L)
      req(depends())
      
      if (nrow(depends()) == 0) {
        # packages like curl, magrittr will appear here instead of in tryCatch() above
        msg <- paste("Detailed dependency information is not available for package", selected_pkg$name())
        rlang::warn(msg)
        dplyr::tibble(package = character(0), type = character(0), name = character(0))
      } else {
        pkginfo <- depends() %>%
          as_tibble() %>%
          mutate(package = stringr::str_replace(package, "\n", "")) %>%
          mutate(name = stringr::str_extract(package, "\\w+"))
        
      purrr::map_df(pkginfo$name, ~get_versnScore(.x, loaded2_db(), cran_pkgs)) %>% 
        right_join(pkginfo, by = "name") %>% 
        select(package, type, name, version, score) %>%
        distinct()
      }
    })
    
    data_table <- eventReactive(pkg_df(), {
      cbind(
        pkg_df(),
        data.frame(
          Actions = shinyInput(
            actionButton, nrow(pkg_df()),
            "button_",
            size = "xs",
            style = "height:24px; padding-top:1px;",
            label = icon("arrow-right", class = "fa-regular", lib = "font-awesome"),
            onclick = paste0('Shiny.setInputValue(\"', ns("select_button"), '\", this.id, {priority: "event"})')
          )
        )
      ) %>% # remove action button if there is nothing to review
        mutate(Actions = if_else(identical(package, character(0)) | name %in% c(rownames(installed.packages(priority = "base"))), "", Actions))
    })
    
    # Render Output UI for Package Dependencies.
    output$package_dependencies_ui <- renderUI({
      req(depends())
      
      # Lets the user know that a package needs to be selected.
      if (identical(selected_pkg$name(), character(0))) {
        showHelperMessage()
      } else {
        fluidPage(
          shiny::
            tagList(
              br(),
              h4(glue::glue("Package Dependencies: {nrow(depends())}"), style = "text-align: left;"),
              br(),
              fluidRow(
                column(6, 
                 tags$strong(
                  glue::glue("First-order dependencies for package: ", {selected_pkg$name()})
                 )
                ),
                column(3,
                 actionButton(
                   inputId =  ns("update_all_packages"),
                   label = "Upload all",
                   icon = icon("fas fa-upload fa-solid", class = "fa-regular", lib = "font-awesome"),
                   size = "xs",
                   style = "height:30px; padding-top:1px;"
                 )
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
                  if (nrow(depends()) == 0) {
                    renderText("No dependency information is available")
                  } else {
                    DT::renderDataTable(server = FALSE, {
                      # Hiding name from DT table. target contains index for "name"
                      # The - 1 is because js uses 0 index instead of 1 like R
                      target <- which(names(data_table()) %in% c("name")) - 1
                      
                      formattable::as.datatable(
                        formattable::formattable(
                          data_table(),
                          list(
                            score = formattable::formatter(
                              "span",
                              style = x ~ formattable::style(
                                display = "block",
                                "border-radius" = "4px",
                                "padding-right" = "4px",
                                "font-weight" = "bold",
                                "color" = "white",
                                "order" = x,
                                "background-color" = formattable::csscolor(
                                  setColorPalette(100)[round(as.numeric(x)*100)]
                                )
                              )
                            ),
                            decision = formattable::formatter(
                              "span",
                              style = x ~ formattable::style(
                                display = "block",
                                "border-radius" = "4px",
                                "padding-right" = "4px",
                                "font-weight" = "bold",
                                "color" = ifelse(x %in% decision_lst, "white", "inherit"),
                                "background-color" =
                                  ifelse(x %in% decision_lst,
                                         color_lst[x],
                                         "transparent"
                                  )
                              )
                            )
                          )
                        ),
                        selection = "none",
                        colnames = c("Package", "Type", "Name", "Version", "Score", "Review Package"),
                        rownames = FALSE,
                        options = list(
                          lengthMenu = list(c(15, -1), c("15", "All")),
                          columnDefs = list(list(visible = FALSE, targets = target)),
                          searchable = FALSE
                        ),
                        style = "default"
                      ) %>%
                        DT::formatStyle(names(data_table()), textAlign = "center")
                    })
                  } # if_else
                ) # column
              ), # fluidRow
              br(),
              h4(glue::glue("Reverse Dependencies: {length(revdeps())}"), style = "text-align: left;"),
              br(), br(),
              fluidRow(
                column(
                  width = 8,
                  wellPanel(
                    renderText(revdeps() %>% sort()),
                    style = "max-height: 500px; overflow: auto"
                  )
                )
              )
            ) # taglist
        ) # fluidpage
      }
    }) # renderUI
    
    pkgname <- reactiveVal()
    
    observeEvent(input$select_button,
     {
       req(pkg_df())
       rev_pkg(0L)
       
       selectedRow <- as.numeric(strsplit(input$select_button, "_")[[1]][2])
       
       # grab the package name
       pkg_name <- pkg_df()[selectedRow, 3] %>% pull()
       pkgname("-")
       
       if (!pkg_name %in% loaded2_db()$name) {
         pkgname(pkg_name)
         shiny::showModal(modalDialog(
           size = "l",
           easyClose = TRUE,
           h5("Package not Loaded", style = "text-align: center !important"),
           hr(),
           br(),
           fluidRow(
             column(
               width = 12,
               "Please confirm to load this package: ", span(class = "text-info", input$decision),
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
           choices = c("-", loaded2_db()$name),
           selected = pkg_name
         )
       }
     },
     ignoreInit = TRUE
    ) # observeEvent
    
    
    observeEvent(input$confirm, {
      shiny::removeModal()
      
      updateSelectizeInput(
        session = parent, "upload_package-pkg_lst",
        choices = c(pkgname()), selected = pkgname()
      )
      
      session$onFlushed(function() {
        shinyjs::click(id = "upload_package-add_pkgs", asis = TRUE)
        
        rev_pkg(1L)
      })
    })
    
    # Close modal if user cancels decision submission.
    observeEvent(input$cancel, {
      shiny::removeModal()
    })
    
    names_vect <- eventReactive(list(rev_pkg(), changes()), {
      req(rev_pkg() == 1L)
      dbSelect("SELECT name FROM package")$name
    })
    
    observeEvent(names_vect(),
     {
       pkg_name <- names_vect()[length(names_vect())]
       
       updateSelectizeInput(
         session = parent,
         inputId = "sidebar-select_pkg",
         choices = c("-", loaded2_db()$name),
         selected = pkg_name
       )
     },
     ignoreInit = TRUE
    )
    
    
    observeEvent(input$update_all_packages, {
      req(pkg_df(), loaded2_db())
      
      pkgs_update <- pkg_df() %>%
        dplyr::filter(is.na(score) | score == "") %>%
        dplyr::filter(!name %in% c(rownames(installed.packages(priority = "base"))))
      
      n_packages <- nrow(pkgs_update)
      if (n_packages > 0) {
        pkgname(pkgs_update$name)
        shiny::showModal(
          modalDialog(
            size = "l",
            easyClose = TRUE,
            title = "Upload all packages?",
            p(glue::glue("Do you want to upload {n_packages} package(s)?")),
             footer = tagList(
               actionButton(NS(id, "confirm"), "Load Package(s)"),
               actionButton(NS(id, "cancel"), "Cancel")
             )
          )
        )
        
      } else {
        shiny::showModal(
          modalDialog(
            size = "l",
            easyClose = TRUE,
            title = "No packages to update",
            p("There are no new packages to upload.")
          )
        )
        
      }

    })
  }) # moduleServer
}
