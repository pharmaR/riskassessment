library(shiny)

ui <- fluidPage(
  shinyjs::useShinyjs(),
  riskassessment:::reweightViewUI("reweightInfo")
)

server <- function(input, output, session) {
  shinyOptions(golem_options = list(assessment_db_name = "dplyr.sqlite"))
  
  user <- reactiveValues(
    name = "tester",
    role = "admin"
  )
  
  exportTestValues(
    metric_weights = {
      metric_weights()
    }
  )
  
  metric_weights <- riskassessment:::reweightViewServer("reweightInfo", user)
}

shinyApp(ui, server)
