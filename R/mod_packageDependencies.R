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
       
       getDependencyTree <- function(pack, i = -1) {
         if(i == -1) cat(pack, "\n")
         i <- i + 1
         packages <- riskmetric::pkg_ref(pack) %>%  riskmetric::assess_dependencies()  
         if (purrr::is_empty(packages)) {
           return() }
         else {packages <- pull(packages, package)
         }
         for(pkg in packages) { 
           for(n in 0:i) {
             cat(" ")
           }
           cat("|", gsub("\n","",pkg), "\n")
         }
       } 
       
       revDependencyData <- function(pkg_name) {
         data <- riskmetric::pkg_ref(pkg_name) %>%  
           riskmetric::assess_reverse_dependencies() %>% 
           tibble::enframe(., name = "num", value = "name") %>% 
           mutate(version = NA_character_, type = "Suggests",
                  origin = pkg_name, origin_level = 0L, dest_level = 1L
           ) %>% 
           select(origin, name, version, type, origin_level, dest_level)
         
         attributes(data)$class <- c("deepdep", "data.frame")
         attributes(data)$package_name <- pkg_name
         
         return(data)
       }
       
       revdata <- reactive(revDependencyData(selected_pkg$name()))
       
   # Render Output UI for Package Dependencies.
    output$package_dependencies_ui <- renderUI({

      # Lets the user know that a package needs to be selected.
      if(identical(selected_pkg$name(), character(0)))
        showHelperMessage()
      
      else {
        fluidPage(
          
          tagList(
            br(),
            h4("Package Dependencies", style = "text-align: left;"),
            br(), br(),
            fluidRow(
            column(width = 2,
            renderPrint(getDependencyTree(selected_pkg$name())),
            ),
            column(width = 6,
            renderPlot(
              deepdep::plot_dependencies(selected_pkg$name(), show_version = TRUE)
             )
            )
           ),
           br(),
           h4("Reverse Dependencies", style = "text-align: left;"),
           br(), br(),
           fluidRow(column(width = 8,
           renderPlot(
             deepdep::plot_dependencies(revdata())
            )
           )
          )
         )
        )
      }
    }) # renderUI
    
  }) # moduleServer

}
