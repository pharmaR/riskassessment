user <- reactiveValues(name = "tester",
                       role = "tester")

uploaded_pkgs <- reactiveVal(structure(list(package = c("dplyr", "shiny", "DT"), 
                                            version = c("1.0.10", "1.7.2", "0.25"), 
                                            status = c("new", "new", "new")), 
                                       row.names = c(NA, -3L), 
                                       class = c("tbl_df", "tbl", "data.frame")))

metric_weights <- reactiveVal(structure(list(name = c("has_vignettes", "has_news", "news_current", 
                                                      "has_bug_reports_url", "has_website", "has_maintainer", 
                                                      "has_source_control", "export_help", "bugs_status", 
                                                      "license", "covr_coverage", "downloads_1yr"), 
                                             weight = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1)), 
                                        class = "data.frame", 
                                        row.names = c(NA, -12L)))

changes <- reactiveVal(0)

test_that("table_data updates",{
  testServer(databaseViewServer, args = list(user = user, 
                                             uploaded_pkgs = uploaded_pkgs, 
                                             metric_weights = metric_weights, 
                                             changes = changes), {
                                               
    shinyOptions(golem_options = list(assessment_db_name = system.file("testdata", "test_dat1.sqlite", package = "riskassessment")))
    session$flushReact()
                                               
   expect_equal(table_data(), structure(list(name = c("shiny", "dplyr", "DT"),
                                             version = c("1.7.3", "1.0.10", "0.26"),
                                             score = c(0.33, 0.11, 0.23),
                                             was_decision_made = c(FALSE,  FALSE, FALSE),
                                             decision = c("-", "-", "-"),
                                             last_comment = c("-", "-", "-")),
                                        class = "data.frame",
                                        row.names = c(NA, -3L)))
   
   output$packages_table
  })
})
