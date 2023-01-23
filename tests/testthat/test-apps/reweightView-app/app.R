library(shiny)

ui <- fluidPage(
  riskassessment:::reweightViewUI("reweightInfo")
)

server <- function(input, output, session) {
  user <- reactiveValues(
    name = "tester",
    role = "admin"
  )
  
  riskassessment:::reweightViewServer("reweightInfo", user)
}

shinyApp(ui, server)
