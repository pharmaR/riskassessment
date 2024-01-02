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
  
  loaded2_db <- eventReactive(pkg(), {

    riskassessment:::dbSelect("SELECT name, version, score FROM package")
  })
  
  # Get Package Dependency metrics.
  dep_metrics  <- reactiveVal()
  
  pkgref <- eventReactive(pkg(), {

    riskassessment:::get_assess_blob(pkg())
  })
  
  observeEvent(pkgref(), {
    req(pkgref())
    tryCatch(
      expr = {
        dep_metrics(pkgref()$dependencies[[1]] |> dplyr::as_tibble())
      },
      error = function(e) {
        msg <- paste("Detailed dependency information is not available for package", pkg())
        rlang::warn(msg)
        rlang::warn(paste("info:", e))
        dep_metrics(dplyr::tibble(package = character(0), type = character(0), name = character(0)))
      }
    )
  })
  
  riskassessment:::mod_downloadHandler_server("downloadHandler_1", pkg, user, metric_weights, dep_metrics, loaded2_db)
  riskassessment:::mod_downloadHandler_server("downloadHandler_2", pkgs, user, metric_weights, dep_metrics, loaded2_db)
}

shinyApp(ui, server)
