#' Package Dependencies module's UI.
#' 
#' @param id a module id name
#' @keywords internal
#' 
packageDependenciesUI <- function(id) {
  uiOutput(NS(id, 'package_dependencies_ui'))
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
#' @importFrom cli cli_progress_bar cli_progress_done cli_progress_message 
#'             cli_progress_update pb_eta pb_percent
#' @importFrom deepdep deepdep plot_dependencies
#' @importFrom DT formatStyle renderDataTable
#' @importFrom formattable as.datatable csscolor formattable formatter style
#' @importFrom glue glue
#' @importFrom purrr map_df
#' @importFrom riskmetric assess_dependencies assess_reverse_dependencies pkg_assess pkg_score
#' @importFrom rlang is_empty
#' @importFrom shiny tagList showModal removeModal
#' @importFrom shinyjs click
#' @importFrom stringr regex str_replace word
#' 
#' @keywords internal
#' 
packageDependenciesServer <- function(id, selected_pkg, user, changes, parent) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    metric_wts_df <- eventReactive(selected_pkg$name(), {
      dbSelect("SELECT id, name, weight FROM metric")
    }) 
    
    metric_weights <- NULL
    observeEvent(metric_wts_df(), {
      # Get the metrics weights to be used during pkg_score.
      metric_weights <<- isolate(metric_wts_df()$weight)
      names(metric_weights) <<- isolate(metric_wts_df()$name)
    })

    loaded2_db <- eventReactive(changes(), {
      dbSelect('SELECT name FROM package')$name
    })
    
    get_versnScore <- function(pkg_name, id) {
      
      riskmetric_assess <-
        riskmetric::pkg_ref(pkg_name,
                            source = "pkg_cran_remote",
                            repos = c("https://cran.rstudio.com")) %>%
        dplyr::as_tibble() %>%
        riskmetric::pkg_assess()
      
      riskmetric_score <-
        riskmetric_assess %>%
        riskmetric::pkg_score(weights = metric_weights)
      
      cli::cli_progress_update(id = id)
      
      return(list(name = riskmetric_assess$package, version = riskmetric_assess$version, score = round(riskmetric_score$pkg_score,2) ))
    }
    
    # used for adding action buttons to data_table
    shinyInput <- function(FUN, len, id, ...) {
      inputs <- character(len)
      for (i in seq_len(len)) {
        inputs[i] <- as.character(FUN(paste0(id, i), ...))
        # change icon from arrow to upload if not loaded into db yet
        pkg_name <- pkg_df()[i, 3] %>% pull()
        if (!pkg_name %in% loaded2_db()) {
          inputs[i] <- gsub("fas fa-arrow-right fa-regular", "fas fa-upload fa-solid", inputs[i])
        }
      }
      inputs
    }
    
    pkgref <- reactive({
      req(selected_pkg$name())
      req(selected_pkg$name() != "-")
      riskmetric::pkg_ref(selected_pkg$name())
    }) 
    
    depends <- reactive({
      req(pkgref())
      pkgref() %>% 
        riskmetric::assess_dependencies() 
    })
    
    revdeps <- reactive({
      req(pkgref())
      pkgref() %>% 
        riskmetric::assess_reverse_dependencies() 
    })
    
    dd <- eventReactive(pkg_df(), {
      x <- deepdep::deepdep(selected_pkg$name(), depth = 2, dependency_type = c("Depends", "Imports", "LinkingTo"))
      if (nrow(x) == 0) {
        x <- data.frame(origin = selected_pkg$name(),
                        name = pull(pkg_df()[1, 3]), version = NA_character_, type = "Imports",
                        origin_level = 0L, dest_level = 1L)
        attributes(x)$class <- c("deepdep", "data.frame")
        attributes(x)$package_name <- selected_pkg$name()
      }
      return(x)
    })
    
    tabready <- reactiveVal()
    add_pkg <- reactiveVal()
    lastpkg <- reactiveVal()

    observeEvent(list(parent$input$tabs, selected_pkg$name()), {
      req(parent$input$tabs, selected_pkg$name())
 
      tabready(0L)
      if(parent$input$tabs == "Package Dependencies") {
        tabready(1L)
        
        if(length(lastpkg()) == 0) lastpkg("$$$$$") # dummy package name

      }
    })
    
    m_id <- reactiveVal()
    # start the progress message as soon as possible
    observeEvent(list(selected_pkg$name(),tabready()), {
      req(selected_pkg$name() != "-")
      req(tabready() == 1L)
      req(lastpkg() != selected_pkg$name())

      # this is really where the progress bar should start
      m_id(cli::cli_progress_message("Compiling dependency info...", .auto_close = FALSE))
    }, ignoreInit = TRUE)
    
    pkg_df <- eventReactive({selected_pkg$name(); tabready()}, {
      req(!rlang::is_empty(selected_pkg$name()))
      req(selected_pkg$name() != "-")
      req(tabready() == 1L)
      req(lastpkg() != selected_pkg$name())

      pkginfo <- depends() %>%  
        as_tibble() %>% 
        mutate(package = stringr::str_replace(package, "\n", "")) %>% 
        mutate(name = stringr::word(.data$package, 1, sep = stringr::regex("[\\s|\\(]"))) 
      
      # drop any Base R packages from list, unless we ony have 1 row
      pkginf2 <- pkginfo %>% 
        filter(!name %in% c(rownames(installed.packages(priority="base")))) 
      if (nrow(pkginf2) > 0) pkginfo <- pkginf2
      
      pkg_name <- ""
      cl_id <- cli::cli_progress_bar("Assessing Package: ", 
                                     type = "iterator",
                                     format = "{pkg_name} {cli::pb_percent} | ETA: {cli::pb_eta}",
                                     total = nrow(pkginfo))
      
      purrr::map_df(pkginfo$name, ~get_versnScore(.x, cl_id), .progress = TRUE) %>% 
        right_join(pkginfo, by = "name") %>% 
        select(package, type, name, version, score)
      
    }, ignoreInit = TRUE) 
    
    observeEvent(pkg_df(), {
      cli::cli_progress_done(id = m_id())
      cli::cli_progress_done()
      
      lastpkg(isolate(selected_pkg$name()))
      
    }) 
    
    # Render Output UI for Package Dependencies.
    output$package_dependencies_ui <- renderUI({
      
      # Lets the user know that a package needs to be selected.
      if(identical(selected_pkg$name(), character(0)))
        showHelperMessage()
      
      else {
        fluidPage(
          
          shiny::
            tagList(
              br(),
              h4(glue::glue("Package Dependencies: {nrow(depends())}"), style = "text-align: left;"),
              br(), 
              tags$strong(glue::glue("First-order dependencies for package: ", {selected_pkg$name()})),
              br(),
              fluidRow(
                column(width = 4,
                       DT::renderDataTable(server = FALSE, {
                         
                         my_data_table <- reactive({
                           cbind(pkg_df(), 
                                 data.frame(
                                   Actions = shinyInput(actionButton, nrow(pkg_df()),
                                                        'button_',
                                                        size = "xs",
                                                        style='height:24px; padding-top:1px;',
                                                        label = icon("arrow-right", class="fa-regular", lib = "font-awesome"),
                                                        onclick = paste0('Shiny.setInputValue(\"' , ns("select_button"), '\", this.id)')
                                   )
                                 )
                           )
                         })
                         
                         formattable::as.datatable(
                           formattable::formattable(
                             isolate(my_data_table()),
                             list(
                               score = formattable::formatter(
                                 "span",
                                 style = x ~ formattable::style(display = "block",
                                                                "border-radius" = "4px",
                                                                "padding-right" = "4px",
                                                                "font-weight" = "bold",
                                                                "color" = "white",
                                                                "order" = x,
                                                                "background-color" = formattable::csscolor(
                                                                  setColorPalette(100)[round(as.numeric(x)*100)]))),
                               decision = formattable::formatter(
                                 "span",
                                 style = x ~ formattable::style(display = "block",
                                                                "border-radius" = "4px",
                                                                "padding-right" = "4px",
                                                                "font-weight" = "bold",
                                                                "color" = ifelse(x %in% decision_lst, "white", "inherit"),
                                                                "background-color" = 
                                                                  ifelse(x %in% decision_lst, 
                                                                         color_lst[x], 
                                                                         "transparent")))
                             )),
                           selection = list(mode = 'multiple'),
                           colnames = c("Package", "Type", "Name", "Version", "Score", "Review Package"),
                           rownames = FALSE,
                           style="default"
                         ) %>%
                           DT::formatStyle(names(my_data_table()), textAlign = 'center')
                       })
                ),
                column(width = 8, 
                       style="position:relative; top: 0px; right: 0px; left: 100px; width: 750px; height: 750px",
                       # renderCachedPlot({
                       #   deepdep::plot_dependencies(dd(), type = "circular", same_level = TRUE, show_version = TRUE, reverse = TRUE, show_stamp = FALSE)
                       # }, 
                       # cacheKeyExpr = { list(selected_pkg$name()) },
                       # cache = cachem::cache_mem(),
                       # sizePolicy = sizeGrowthRatio(width = 1250, height = 1250, growthRate = 1)
                       # )
                       renderPlot(width = 750, height = 750, {
                         deepdep::plot_dependencies(isolate(dd()), type = "circular", same_level = TRUE, show_version = TRUE, reverse = TRUE, show_stamp = FALSE)
                       })
                )
              )
            ),
          br(),
          h4(glue::glue("Reverse Dependencies: {length(revdeps())}"), style = "text-align: left;"),
          br(), br(),
          fluidRow(column(width = 8,
                          renderText(
                            revdeps() %>% sort()
                          )
          )
          )
        )
      }
    }) # renderUI
    
    pkgname <- reactiveVal()
    
    observeEvent(input$select_button, {
      add_pkg(0L)
      
      selectedRow <- as.numeric(strsplit(input$select_button, "_")[[1]][2])
      
      # grab the package name
      pkg_name <- pkg_df()[selectedRow, 3] %>% pull() 
      pkgname("")
      
      if(!pkg_name %in% dbSelect('SELECT name FROM package')$name) {
        pkgname(pkg_name)
        
        shiny::showModal(modalDialog(
          size = "l",
          easyClose = TRUE,
          h5("Package not Loaded", style = 'text-align: center !important'),
          hr(),
          br(),
          fluidRow(
            column(
              width = 12,
              'Please confirm to load this package: ', span(class = 'text-info', input$decision),
            )
          ),
          br(),
          footer = tagList(
            actionButton(NS(id, 'confirm'), 'Load Package'),
            actionButton(NS(id, 'cancel'), 'Cancel')
          )))

      } else {
        # update sidebar-select_pkg
        updateSelectizeInput(
          session = parent,
          inputId = "sidebar-select_pkg",
          choices = c("-", dbSelect('SELECT name FROM package')$name),
          selected = pkg_name
        )}
      
    }, ignoreInit = TRUE) # observeEvent

    observeEvent(input$confirm, {

      shiny::removeModal()
      
      updateSelectizeInput(session = parent, "upload_package-pkg_lst",
                           choices = c(pkgname()), selected = pkgname())

      session$onFlushed(function() {
        shinyjs::click(id = "upload_package-add_pkgs", asis = TRUE)

      add_pkg(1L)
      })
      
    })
    
  # Close modal if user cancels decision submission.
  observeEvent(input$cancel, {
    shiny::removeModal()
  })
  
    names_vect <- eventReactive({add_pkg(); changes()}, {
      req(add_pkg() == 1L)
      
      dbSelect('SELECT name FROM package')$name
    
    })
    
    observeEvent(names_vect(), {

      pkgs <- dbSelect("select name from package")[,1]
      
      pkg_name <- names_vect()[length(names_vect())]
      
      updateSelectizeInput(
        session = parent,
        inputId = "sidebar-select_pkg",
        choices = c("-", dbSelect('SELECT name FROM package')$name),
        selected = pkg_name
        )

    }, ignoreInit = TRUE)
    
  }) # moduleServer
  
}
