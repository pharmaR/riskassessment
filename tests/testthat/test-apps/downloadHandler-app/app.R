library(shiny)

ui <- fluidPage(
  tabsetPanel(
    id = "tabs",
    tabPanel(
      "single",
      riskassessment:::mod_downloadHandler_button_ui("downloadHandler_1", multiple = FALSE),
      riskassessment:::mod_downloadHandler_filetype_ui("downloadHandler_1")
    ),
    tabPanel(
      "multiple",
      riskassessment:::mod_downloadHandler_button_ui("downloadHandler_2", multiple = TRUE),
      riskassessment:::mod_downloadHandler_filetype_ui("downloadHandler_2")
    )
  )
)

server <- function(input, output, session) {
  shinyOptions(golem_options = list(assessment_db_name = "dplyr_tidyr.sqlite"))
  
  user <- reactiveValues(
    name = "tester",
    role = "tester"
  )
  
  pkg <- reactiveVal("dplyr")
  pkgs <- reactiveVal(c("dplyr", "tidyr"))
  
  metric_weights <- reactiveVal(structure(list(name = c("has_vignettes", "has_news", "news_current", 
                                                        "has_bug_reports_url", "has_website", "has_maintainer", 
                                                        "has_source_control", "export_help", "bugs_status", 
                                                        "license", "covr_coverage", "downloads_1yr"), 
                                               weight = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1)), 
                                          class = "data.frame", 
                                          row.names = c(NA, -12L)))
  

  session$userData$repo_pkgs <- reactiveVal()
  session$userData$suggests <- reactiveVal(FALSE)
  
  session$userData$loaded2_db <- reactive({
    riskassessment:::dbSelect("select name, version, score from package")
  })
  
  riskassessment:::mod_downloadHandler_server("downloadHandler_1", pkg, user, metric_weights)
  riskassessment:::mod_downloadHandler_server("downloadHandler_2", pkgs, user, metric_weights)
}

shinyApp(ui, server)
