test_that("downloadHanlder works", {
  
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
    shinyOptions(golem_options = list(assessment_db_name = system.file("testdata", "dplyr_tidyr.sqlite", package = "riskassessment")))
    
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
    
    riskassessment:::mod_downloadHandler_server("downloadHandler_1", pkg, user, metric_weights)
    riskassessment:::mod_downloadHandler_server("downloadHandler_2", pkgs, user, metric_weights)
  }
  
  app <- AppDriver$new(shinyApp(ui, server))
  
  expect_equal(app$get_value(input = "tabs"), "single")
  expect_equal(app$get_value(input = "downloadHandler_1-report_format"), "html")
  report <- app$get_download("downloadHandler_1-download_reports")
  expect_equal(tools::file_ext(report), "html")
  
  app$set_inputs(`downloadHandler_1-report_format` = "docx")
  report <- app$get_download("downloadHandler_1-download_reports")
  expect_equal(tools::file_ext(report), "docx")
  
  app$set_inputs(`downloadHandler_1-report_format` = "pdf")
  report <- app$get_download("downloadHandler_1-download_reports")
  expect_equal(tools::file_ext(report), "pdf")
  
  app$set_inputs(tabs = "multiple")
  expect_equal(app$get_value(input = "downloadHandler_2-report_format"), "html")
  report <- app$get_download("downloadHandler_2-download_reports")
  expect_equal(tools::file_ext(report), "zip")
})