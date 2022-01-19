#' assessment_criteria UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_assessment_criteria_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' assessment_criteria Server Functions
#'
#' @noRd 
mod_assessment_criteria_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_assessment_criteria_ui("assessment_criteria_ui_1")
    
## To be copied in the server
# mod_assessment_criteria_server("assessment_criteria_ui_1")
