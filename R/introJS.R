# Module to display introJS button and functionality.
introJSUI <- function(id) {
  fluidRow(
    style = "float: right",
    column(
      width = 3,
      actionBttn(NS(id, "help"),
                 "",
                 color = "success",
                 icon = icon("question-circle"),
                 block = FALSE,
                 style = "simple",
                 size = "md")
    ))
}

introJSServer <- function(id, text) {
  moduleServer(id, function(input, output, session) {
    
    # Start introjs when help button is pressed.
    observeEvent(
      input$help,
      introjs(session,
              options = list(
                steps = 
                  text %>%
                  union(sidebar_steps),
                "nextLabel" = "Next",
                "prevLabel" = "Previous",
                "skipLabel" = "Close"
              )
      )
    )
  })
}