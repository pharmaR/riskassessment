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
#' @param maint_metrics placeholder
#' @param user placeholder 
#' @param parent the parent (calling module) session information
#' 
#' @import dplyr
#' @importFrom riskmetric pkg_ref assess_dependencies
#' @importFrom purrr is_empty
#' @importFrom deepdep plot_dependencies
#' 
#' @keywords internal
#' 
packageDependenciesServer <- function(id, selected_pkg, user, parent) {
  moduleServer(id, function(input, output, session) {
       ns <- NS(id)
       
       get_versnScore <- function(pkg_name) {
         
         riskmetric_assess <-
           riskmetric::pkg_ref(pkg_name,
                               source = "pkg_cran_remote",
                               repos = c("https://cran.rstudio.com")) %>%
           dplyr::as_tibble() %>%
           riskmetric::pkg_assess()
         
         riskmetric_score <-
           riskmetric_assess %>%
           riskmetric::pkg_score(weights = metric_weights())
         
         return(list(name = riskmetric_assess$package, version = riskmetric_assess$version, score = round(riskmetric_score$pkg_score,2) ))
       }
       
       metric_weights <- reactive({
         db_name <- "database.sqlite"
         metric_wts_df <- dbSelect("SELECT id, name, weight, is_perc FROM metric", db_name)
         metric_wts <- metric_wts_df$weight
         names(metric_wts) <- metric_wts_df$name
         return(metric_wts)
       })
       
       depends <- reactive({
         req(selected_pkg$name() != "-")
         riskmetric::pkg_ref(selected_pkg$name()) %>% 
           riskmetric::assess_dependencies() 
       })
       
       revdeps <- reactive({
         req(selected_pkg$name() != "-")
         riskmetric::pkg_ref(selected_pkg$name()) %>% 
           riskmetric::assess_reverse_dependencies() 
       })
       
       ready <- reactiveVal()
       observeEvent(parent$input$tabs, {
         cat("parent input$tabs is", parent$input$tabs, "\n")
         ready(0L)
         if(parent$input$tabs == "Package Dependencies") {
         ready(1L)
         }
         
       })
       
       pkg_df <- eventReactive(list(selected_pkg$name(),ready()), {
         req(!rlang::is_empty(selected_pkg$name()))
         req(selected_pkg$name() != "-")
         req(ready() == 1L)

         shiny::showModal(modalDialog(
           title = h3("Update in Progress"),
           tags$strong("Creating data table. Please wait."),
           easyClose = FALSE
         ))
         
         # check remotes first to make sure the packages are available on CRAN
         pkgrem <- remotes::package_deps(selected_pkg$name(), 
                                         dependencies = c("Depends", "Imports", "LinkingTo"))  
         
       pkginfo <- riskmetric::pkg_ref(selected_pkg$name()) %>% 
         riskmetric::assess_dependencies() %>%  
         as_tibble() %>% 
         mutate(package = stringr::str_replace(package, "\n", "")) %>% 
         mutate(name = stringr::word(.data$package, 1, sep = stringr::regex("[\\s|\\(]"))) %>%
         left_join(pkgrem %>% select(package, available), by = c("name" = "package")) %>% 
         filter(!is.na(available))
       
       purrr::map_df(pkginfo$name, ~bind_rows(unlist(get_versnScore(.x)))) %>% 
         right_join(pkginfo, by = "name") %>% 
         select(package, type, name, version, score)

       }, ignoreInit = TRUE)
       
       # used for adding action buttons to data_table
       shinyInput <- function(FUN, len, id, ...) {
         inputs <- character(len)
         for (i in seq_len(len)) {
           inputs[i] <- as.character(FUN(paste0(id, i), ...))
         }
         inputs
       }
       
    observeEvent(pkg_df(), {
      print("in observeEvent for pkg_df()")
      shiny::removeModal()
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
                                                    onclick = paste0('Shiny.onInputChange(' , ns("select_button"), ', this.id)')
                               )
                             )
                       )
                     })
                     
                     formattable::as.datatable(
                       formattable::formattable(
                         my_data_table(),
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
                       colnames = c("Package", "Type", "Name", "Version", "Score", "Explore Metrics"),
                       rownames = FALSE,
                       # extensions = "Buttons",
                       # options = list(
                       #   searching = TRUE,
                       #   lengthChange = FALSE,
                       #   dom = 'Blftpr',
                       #   pageLength = 15,
                       #   lengthMenu = list(c(15, 60, 120, -1), c('15', '60', '120', "All")),
                       #   columnDefs = list(list(className = 'dt-center', targets = "_all"))
                       # )
                       style="default"
                     ) %>%
                       DT::formatStyle(names(my_data_table()), textAlign = 'center')
                   })
            ),
            column(width = 4,
                   renderPlot(
                     deepdep::plot_dependencies(selected_pkg$name(), show_version = TRUE)
                   )
            )
           ),
           br(),
           h4(glue::glue("Reverse Dependencies: {length(revdeps())}"), style = "text-align: left;"),
           br(), br(),
           fluidRow(column(width = 8,
           renderPrint(
               revdeps() %>% sort()
            )
           ),
           column(width = 4,
                  # renderPrint(
                  #   pak::pkg_deps_tree(selected_pkg$name())
                  # )
                  )
          )
         )
        )
      }
    }) # renderUI
    
    observeEvent(input$select_button, {
      cat("in observeEvent for input$select_button", input$select_button, "\n")
      req(pkg_df())
      
      selectedRow <- as.numeric(strsplit(input$select_button, "_")[[1]][2])
      
      # grab the package name
      pkg_name <- pkg_df()[selectedRow, 1]
      print(pkg_name)
      
      # update sidebar-select_pkg
      updateSelectizeInput(
        session = parent,
        inputId = "sidebar-select_pkg",
        choices = c("-", dbSelect('SELECT name FROM package')$name),
        selected = pkg_name
      )
    })
  }) # moduleServer

}
