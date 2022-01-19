#' db_dash_screen UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_db_dash_screen_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' db_dash_screen Server Functions
#'
#' @noRd 
mod_db_dash_screen_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_db_dash_screen_ui("db_dash_screen_ui_1")
    
## To be copied in the server
# mod_db_dash_screen_server("db_dash_screen_ui_1")
