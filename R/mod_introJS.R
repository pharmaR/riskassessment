
#'UI for Module to display introJS button and functionality.
#' 
#' @param id a module id
#' @keywords internal
#' 
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
#' @param user placeholder 
#'
#' 
#' @importFrom rintrojs introjs
#' @keywords internal
introJSServer <- function(id, text, user) {
  moduleServer(id, function(input, output, session) {
    approved_roles <- get_golem_config("credentials", file = app_sys("db-config.yml"))[["privileges"]]
    
    steps <- reactive({
      if(user$admin || user$role %in% c(approved_roles[["tier1"]], approved_roles[["weight_adjust"]])) {
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
