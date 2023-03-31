#' Package Dependencies module's UI.
#' 
#' @param id a module id name
#' @keywords internal
#' 
packageDependenciesUI <- function(id) {
  uiOutput(NS(id, 'maintenance_metrics_ui'))
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
#' @keywords internal
#' 
packageDependenciesServer <- function(id, selected_pkg, maint_metrics, user, parent) {
  moduleServer(id, function(input, output, session) {
       ns <- NS(id)
   # Render Output UI for Package Dependencies.
    output$package_dependencies_ui <- renderUI({

      # Lets the user know that a package needs to be selected.
      if(identical(selected_pkg$name(), character(0)))
        showHelperMessage()
      
      else {
        fluidPage(
          
          tagList(
            br(),
            h4("Package Dependencies", style = "text-align: center;"),
            br(), br(),
            metricGridUI(NS(id, 'metricGrid')),
          )
         )
      }
    }) # renderUI
  }) # moduleServer

}
