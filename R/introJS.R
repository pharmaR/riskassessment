
#'UI for Module to display introJS button and functionality.
#' 
#' @param id a module id
#' 
#' @import shiny
introJSUI <- function(id) {
  fluidRow(
    style = "float: right",
    column(
      width = 3,
      shinyWidgets::actionBttn(NS(id, "help"),
                 "",
                 color = "success",
                 icon = icon("circle-question"),
                 block = FALSE,
                 style = "simple",
                 size = "md")
    ))
}

#' Server logic for introJS module
#'
#' @param id a module id
#' @param text a data.frame containing info about each step: element names, text
#'   strings guiding users, and a position of where to place the text in
#'   relationship to the element. Please see `R/introJSText.R` for data.frames
#'   that populate this argument for the app
#'
#' @import shiny
#' @importFrom rintrojs introjs
#'   
introJSServer <- function(id, text) {
  moduleServer(id, function(input, output, session) {
    
    # Start introjs when help button is pressed.
    observeEvent(
      input$help,
      rintrojs::introjs(session,
              options = list(
                steps = 
                  text %>%
                  union(sidebar_steps),
                "nextLabel" = "Next",
                "prevLabel" = "Previous"
              )
      )
    )
  })
}
