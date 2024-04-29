
#'UI for Module to display introJS button and functionality.
#' 
#' @param id a module id
#' @keywords internal
#' 
introJSUI <- function(id) {
  fluidRow(
    style = "position: absolute; top: 1.5rem; right: 0; margin-right: auto;",
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
#' @param user placeholder 
#'
#' 
#' @importFrom rintrojs introjs
#' @keywords internal
introJSServer <- function(id, text, user, credentials) {
  if (missing(credentials))
    credentials <- get_credential_config()
  moduleServer(id, function(input, output, session) {
    
    steps <- reactive({
      if(user$admin || "weight_adjust" %in% unlist(credentials$privileges[user$role], use.names = FALSE)) {
        apptab_steps <- bind_rows(apptab_admn, apptab_steps)
      }
      
      purrr::reduce(
        list(text(), apptab_steps, sidebar_steps),
        dplyr::bind_rows
      )
      })
    
    exportTestValues(
      steps = {
        steps()
      }
    )
    
    # Start introjs when help button is pressed.
    observeEvent(
      input$help,
      rintrojs::introjs(session,
              options = list(
                steps = steps(),
                "nextLabel" = "Next",
                "prevLabel" = "Previous"
              )
      )
    )
  })
}
