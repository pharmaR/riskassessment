library(shiny)

ui <- fluidPage(
  riskassessment:::reweightViewUI("reweightInfo")
)

server <- function(input, output, session) {
  shinyOptions(golem_options = list(assessment_db_name = "dplyr.sqlite"))
  
  user <- reactiveValues(
    name = "tester",
    role = "admin"
  )
  
  riskassessment:::reweightViewServer("reweightInfo", user)
}

shinyApp(ui, server)
