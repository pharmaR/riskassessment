#' testing_metrics UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_testing_metrics_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' testing_metrics Server Functions
#'
#' @noRd 
mod_testing_metrics_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_testing_metrics_ui("testing_metrics_ui_1")
    
## To be copied in the server
# mod_testing_metrics_server("testing_metrics_ui_1")
