library(shiny)

shinyOptions(golem_options = list(assessment_db_name = "database.sqlite",
                                  decision_categories = paste(c("Insignificant", "Minor", "Moderate", "Major", "Severe"), "Risk")))

ui <- fluidPage(
  shinyjs::useShinyjs(),
  riskassessment:::mod_decision_automation_ui("automate")
)

server <- function(input, output, session) {
  
  exportTestValues(
    auto_decision_output = {
      auto_decision()
    }
  )
  
  user <- reactiveValues(
    name = "tester",
    role = "admin"
  )
  
  auto_decision <- riskassessment:::mod_decision_automation_server("automate", user)
}

shinyApp(ui, server)
