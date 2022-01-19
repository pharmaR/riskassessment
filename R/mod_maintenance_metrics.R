#' maintenance_metrics UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_maintenance_metrics_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' maintenance_metrics Server Functions
#'
#' @noRd 
mod_maintenance_metrics_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_maintenance_metrics_ui("maintenance_metrics_ui_1")
    
## To be copied in the server
# mod_maintenance_metrics_server("maintenance_metrics_ui_1")
