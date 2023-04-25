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
#' @param parent the parent (calling module) session information
#' 
#' @import dplyr
#' @importFrom riskmetric pkg_ref pkg_assess pkg_score assess_dependencies assess_reverse_dependencies
#' @importFrom purrr map_df
#' @importFrom rlang is_empty
#' @importFrom glue glue
#' @importFrom DT renderDataTable formatStyle
#' @importFrom stringr str_replace word
#' @importFrom shiny tagList showModal removeModal
#' @importFrom shinyjs click
#' @importFrom deepdep deepdep plot_dependencies
#' @importFrom formattable as.datatable formattable formatter style
#' @importFrom cli cli_progress_bar cli_progress_update cli_progress_done
#' 
#' @keywords internal
#' 
packageDependenciesServer <- function(id, selected_pkg, user, parent) {
  moduleServer(id, function(input, output, session) {
       ns <- session$ns
    
       metric_wts_df <- dbSelect("SELECT id, name, weight FROM metric")
       metric_weights <- metric_wts_df$weight
       names(metric_weights) <- metric_wts_df$name
       
       get_versnScore <- function(pkg_name, cl_id) {
         
         riskmetric_assess <-
           riskmetric::pkg_ref(pkg_name,
                               source = "pkg_cran_remote",
                               repos = c("https://cran.rstudio.com")) %>%
           dplyr::as_tibble() %>%
           riskmetric::pkg_assess()
         
         riskmetric_score <-
           riskmetric_assess %>%
           riskmetric::pkg_score(weights = metric_weights)
         
         cli::cli_progress_update(id = cl_id)
         return(list(name = riskmetric_assess$package, version = riskmetric_assess$version, score = round(riskmetric_score$pkg_score,2) ))
       }
       
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
       
       dd <- reactive({
         req(pkg_df())
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
       observeEvent(parent$input$tabs, {
         tabready(0L)
         if(parent$input$tabs == "Package Dependencies") {
         tabready(1L)
         }
         
       })
       
       pkg_df <- eventReactive(list(selected_pkg$name(),tabready()), {
         req(!rlang::is_empty(selected_pkg$name()))
         req(selected_pkg$name() != "-")
         req(tabready() == 1L)

       pkginfo <- riskmetric::pkg_ref(selected_pkg$name()) %>% 
         riskmetric::assess_dependencies() %>%  
         as_tibble() %>% 
         mutate(package = stringr::str_replace(package, "\n", "")) %>% 
         mutate(name = stringr::word(.data$package, 1, sep = stringr::regex("[\\s|\\(]"))) 
       
         # drop any Base R packages from list, unless we ony have 1 row
         pkginf2 <- pkginfo %>% 
         filter(!name %in% c(rownames(installed.packages(priority="base")))) 
         if (nrow(pkginf2) > 0) pkginfo <- pkginf2

         cl_id <- cli::cli_progress_bar("Creating Data Table...", type = "iterator", total = nrow(pkginfo))

         purrr::map_df(pkginfo$name, ~get_versnScore(.x, cl_id), .progress = TRUE) %>% 
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
      cli::cli_progress_done()
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
                                                    onclick = paste0('Shiny.onInputChange(\"' , ns("select_button"), '\", this.id)')
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
                       style="default"
                     ) %>%
                       DT::formatStyle(names(my_data_table()), textAlign = 'center')
                   })
            ),
            column(width = 4, 
                   style="position:relative; top: 0px; right 0px; left: 250px;",
                   renderPlot({
                     deepdep::plot_dependencies(dd(), type = "circular", same_level = TRUE, show_version = TRUE, reverse = TRUE, show_stamp = FALSE)
                   }, height = 900, width = 900)
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
           )
          )
         )
      }
    }) # renderUI
    
    observeEvent(input$select_button, {
      req(pkg_df())
      
      selectedRow <- as.numeric(strsplit(input$select_button, "_")[[1]][2])
      
      # grab the package name
      pkg_name <- pkg_df()[selectedRow, 3] %>% pull() 
      print(pkg_name)
      
      if(pkg_name %in% dbSelect('SELECT name FROM package')$name) {

        # update sidebar-select_pkg
        updateSelectizeInput(
          session = parent,
          inputId = "sidebar-select_pkg",
          choices = c("-", dbSelect('SELECT name FROM package')$name),
          selected = pkg_name
        )
      } else {
        # select maintenance metrics panel
        updateTabsetPanel(session = parent, 
                          inputId = 'tabs', 
                          selected = "Upload Package"
        )
        updateSelectizeInput(session = parent, "upload_package-pkg_lst", 
                             choices = c(pkg_name), selected = pkg_name)
        shinyjs::click(id = "upload_pkg-add_pkgs", asis = TRUE)
        shinyjs::click(id = parent$ns("add_pkgs"), asis = TRUE)
        shinyjs::click(id = "parent$input$add_pkgs")
        shinyjs::click(id = "add_pkgs")

      }
      
    })
  }) # moduleServer

}
