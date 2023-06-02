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
  
  auto_json <- list()
  auto_list <- reactiveVal(auto_json)
  trigger_events <- reactiveValues()
  
  exportTestValues(
    metric_weights = {
      metric_weights()
    }
  )
  
  metric_weights <- riskassessment:::reweightViewServer("reweightInfo", user, auto_list, trigger_events)
}

shinyApp(ui, server)
