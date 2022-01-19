#' reportpreview UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_reportpreview_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' reportpreview Server Functions
#'
#' @noRd 
mod_reportpreview_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_reportpreview_ui("reportpreview_ui_1")
    
## To be copied in the server
# mod_reportpreview_server("reportpreview_ui_1")
