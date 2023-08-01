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
#' @importFrom DT formatStyle renderDataTable
#' @importFrom formattable as.datatable csscolor formattable formatter style
#' @importFrom glue glue
#' @importFrom golem get_golem_options
#' @importFrom purrr map_df
#' @importFrom riskmetric pkg_ref pkg_assess pkg_score
#' @importFrom rlang is_empty
#' @importFrom shiny removeModal showModal tagList
#' @importFrom shinyjs click
#' @importFrom shinyWidgets materialSwitch updateMaterialSwitch
#' @importFrom stringr str_extract str_replace
#' 
#' @keywords internal
#' 
packageDependenciesServer <- function(id, selected_pkg, user, changes, parent) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    decision_lst <- if (!is.null(golem::get_golem_options("decision_categories"))) golem::get_golem_options("decision_categories") else c("Low Risk", "Medium Risk", "High Risk")
    color_lst <- get_colors(golem::get_golem_options("assessment_db_name"))
    
    last_toggle <- reactiveVal(value = NULL)
    
    metric_wts_df <- eventReactive(selected_pkg$name(), {
      dbSelect("SELECT id, name, weight FROM metric")
    }) 
    
    metric_weights <- NULL
    observeEvent(metric_wts_df(), {
      # Get the metrics weights to be used during pkg_score.
      metric_weights <<- isolate(metric_wts_df()$weight)
      names(metric_weights) <<- isolate(metric_wts_df()$name)
    })

    loaded2_db <- eventReactive(list(selected_pkg$name(), changes()), {
      dbSelect('SELECT name, score FROM package')
    })
    
    get_versnScore <- function(pkg_name, id, toggle_score) {
      
      riskmetric_assess <-
        riskmetric::pkg_ref(pkg_name,
                            source = "pkg_cran_remote",
                            repos = c("https://cran.rstudio.com")) %>%
        dplyr::as_tibble()  
      
      if (!is.null(toggle_score) && toggle_score == FALSE) {
        return(list(name = riskmetric_assess$package, version = riskmetric_assess$version, score = "" ))
        
      } else {
      
      cli::cli_progress_update(id = id)
      
        if (pkg_name %in% loaded2_db()$name) {
          pkg_score <- loaded2_db() %>% filter(name == pkg_name) %>% pull(score)
          
        } else {
        riskmetric_score <-
          riskmetric_assess %>%
          riskmetric::pkg_assess() %>% 
          riskmetric::pkg_score(weights = metric_weights)
          pkg_score <- round(riskmetric_score$pkg_score,2)
        } 
        cat("pkg_name is", pkg_name, "score is", pkg_score, str(pkg_score), "\n")
        
        return(list(name = riskmetric_assess$package, version = riskmetric_assess$version, score = pkg_score))   
      }
    }
    
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
 
    pkgref <- reactive({
      req(selected_pkg$name())
      req(selected_pkg$name() != "-")
      
      db_table <- dbSelect("SELECT metric.name, package_metrics.encode FROM package 
                       INNER JOIN package_metrics ON package.id = package_metrics.package_id
                       INNER JOIN metric ON package_metrics.metric_id = metric.id
                       WHERE package.name = {selected_pkg$name()}", 
                           db_name = golem::get_golem_options('assessment_db_name'))
      
      purrr::pmap_dfc(db_table, function(name, encode) {dplyr::tibble(unserialize(encode)) %>% purrr::set_names(name)}) 
    }) 
	
    depends <- eventReactive( pkgref(), {
      req(pkgref())
      pkgref()$dependencies[[1]] %>% dplyr::as_tibble()
    })
    
    revdeps <- eventReactive( pkgref(),{
      req(pkgref())
      pkgref()$reverse_dependencies[[1]] %>% as.vector()
    })
    
    tabready <- reactiveVal()
    add_pkg <- reactiveVal()
    lastpkg <- reactiveVal()

    observeEvent(selected_pkg$name(), {
      req(selected_pkg$name())
    # reset MaterialSwitch to FALSE whenever the package name changes
      cat("here is where we would set toggle switch to FALSE \n")
      shinyWidgets::updateMaterialSwitch(session = session, inputId = "toggle_score", value = FALSE)
    })
    
    observeEvent(list(parent$input$tabs, parent$input$metric_type, selected_pkg$name()), {
      req(parent$input$tabs, selected_pkg$name())
      
      tabready(0L)
      if(parent$input$tabs == "Package Metrics" & parent$input$metric_type == "dep") {
        tabready(1L)
        if(length(lastpkg()) == 0) lastpkg("$$$$$") # dummy package name
      }
    }, ignoreInit = TRUE)
    
    m_id <- reactiveVal()
    
    pkg_df <- eventReactive(list(selected_pkg$name(), tabready(), input$toggle_score), {
      
      req(!rlang::is_empty(selected_pkg$name()))
      req(selected_pkg$name() != "-")
      cat("eventReactive for pkg_df(), tabready() is", tabready(), "\n")
      cat("is.null(input$toggle_score)?", is.null(input$toggle_score), "\n")
      req(!is.null(input$toggle_score))
      req(tabready() == 1L)

      cat("+ is.null(last_toggle())? ", is.null(last_toggle()), "last_toggle() is", last_toggle(), "input$toggle_score is", input$toggle_score, "\n")
      cat("+ lastpkg() is", lastpkg(), "selected_pkg$name() is", selected_pkg$name(), "\n")
      
      req(lastpkg() != selected_pkg$name() | (is.null(last_toggle()) || is.null(input$toggle_score) || last_toggle() != input$toggle_score))
      if( lastpkg() != selected_pkg$name()) req(input$toggle_score == FALSE)
      
      # start the progress message as soon as possible.
      m_id(cli::cli_progress_message("Compiling dependency info...", .auto_close = FALSE))
      
      pkginfo <- depends() %>%  
        as_tibble() %>% 
        mutate(package = stringr::str_replace(package, "\n", "")) %>% 
        mutate(name = stringr::str_extract(package, "\\w+"))

      # drop any Base R packages from list, unless we ony have 1 row
      pkginf2 <- pkginfo %>% 
        filter(!name %in% c(rownames(installed.packages(priority="base")))) 
      if (nrow(pkginf2) > 0) pkginfo <- pkginf2
      
      pkg_name <- ""
      cl_id <- cli::cli_progress_bar("Assessing Package: ", 
                                     type = "iterator",
                                     format = "{pkg_name} {cli::pb_percent} | ETA: {cli::pb_eta}",
                                     total = nrow(pkginfo))
      
      cat("just before right_join \n")

      if (nrow(pkginfo) == 0) {
        tibble(package = NA_character_, type = "", name = "", version = "", score = "")
      } else {
      purrr::map_df(pkginfo$name, ~get_versnScore(.x, cl_id, input$toggle_score)) %>% 
        right_join(pkginfo, by = "name") %>% 
        select(package, type, name, version, score)
      }

    }, ignoreInit = TRUE) 
    
    observeEvent(pkg_df(), {
      cli::cli_progress_done(id = m_id())
      cli::cli_progress_done()
      last_toggle(input$toggle_score) # remember last input$toggle_score value
      lastpkg(isolate(selected_pkg$name()))
    }) 
    
    data_table <- eventReactive(pkg_df(), {
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
              shinyWidgets::materialSwitch(
                inputId = ns("toggle_score"),
                label = "Calculate risk scores",
                value = FALSE, 
                inline = TRUE,
                status = "success"
              ),
              br(),
              # remove DT "search:" rectangle
              tags$head(
                tags$style(type="text/css", ".dataTables_filter {display: none;    }"
              )),
              fluidRow(
                column(width = 3,
                       
                       DT::renderDataTable(server = FALSE, {
                         
                         formattable::as.datatable(
                           formattable::formattable(
                             data_table(),
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
                           options = list(
                             lengthMenu = list(c(15, -1), c('15', 'All')),
                             searchable = FALSE),
                           style="default"
                         ) %>%
                           DT::formatStyle(names(data_table()), textAlign = 'center')
                       })  %>% bindCache(selected_pkg$name(), input$toggle_score, loaded2_db())
                )
              ),
          br(),
          h4(glue::glue("Reverse Dependencies: {length(revdeps())}"), style = "text-align: left;"),
          br(), br(),
          fluidRow(
            column(width = 8,
                   renderText(revdeps() %>% sort() )
                   )
           )
          
         ) # taglist
        ) #fluidpage
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
      
      cat("pkg_name just added was", pkg_name, "\n")
      updateSelectizeInput(
        session = parent,
        inputId = "sidebar-select_pkg",
        choices = c("-", dbSelect('SELECT name FROM package')$name),
        selected = pkg_name
        )

    }, ignoreInit = TRUE)
    
  }) # moduleServer
  
}
