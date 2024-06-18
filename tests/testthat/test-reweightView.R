test_that("reweightView works", {
  # delete app DB if exists to ensure clean test
  app_db_loc <- test_path("test-apps", "reweightView-app", "dplyr.sqlite")
  if (file.exists(app_db_loc)) {
    file.remove(app_db_loc)
  }
  
  # copy in already instantiated database to avoid need to rebuild
  # this is a database that has been built via inst/testdata/upload_format.csv
  test_db_loc <- system.file("testdata", "upload_format.database", package = "riskassessment")
  file.copy(
    test_db_loc,
    app_db_loc
  )
  
  app <- shinytest2::AppDriver$new(test_path("test-apps", "reweightView-app"))

  metric_weights <- app$get_value(export = "metric_weights")
  curr_new_wts <- app$get_value(export = "reweightInfo-curr_new_wts")
  expect_equal(metric_weights, curr_new_wts[,1:2])
  expect_equal(app$get_value(input = "reweightInfo-metric_weight"), 1)
  
  db_backup <- app$get_download("reweightInfo-download_database_btn")
  app$wait_for_idle()
  app$click(selector = "#confirmation_id button")
  
  con1 <- DBI::dbConnect(RSQLite::SQLite(), app_db_loc)
  con2 <- DBI::dbConnect(RSQLite::SQLite(), db_backup)
  
  expect_equal(DBI::dbListTables(con2), DBI::dbListTables(con1))
  
  db1 <- purrr::map(DBI::dbListTables(con1), ~ DBI::dbGetQuery(con1, glue::glue("SELECT * FROM {.x}")))
  db2 <- purrr::map(DBI::dbListTables(con2), ~ DBI::dbGetQuery(con2, glue::glue("SELECT * FROM {.x}")))
  expect_equal(db2, db1)
  DBI::dbDisconnect(con1)
  DBI::dbDisconnect(con2)

  expect_equal(app$get_value(input = "reweightInfo-metric_name"), curr_new_wts[1,1])
  app$set_inputs(`reweightInfo-metric_weight` = -30)
  app$wait_for_idle()
  expect_equal(app$get_value(input = "reweightInfo-metric_weight"), 0)
  
  app$set_inputs(`reweightInfo-metric_weight` = 2)
  curr_new_wts2 <- app$get_value(export = "reweightInfo-curr_new_wts")
  expect_equal(curr_new_wts2, curr_new_wts)
  app$click("reweightInfo-update_weight")
  curr_new_wts2 <- app$get_value(export = "reweightInfo-curr_new_wts")
  expect_equal(curr_new_wts2[1,3], 2)
  expect_equal(curr_new_wts2[-1,], curr_new_wts[-1,])
  
  app$set_inputs(`reweightInfo-metric_name` = curr_new_wts[3,1])
  app$wait_for_idle()
  app$set_inputs(`reweightInfo-metric_weight` = 3.5)
  app$wait_for_idle()
  app$click("reweightInfo-update_weight")
  curr_new_wts2 <- app$get_value(export = "reweightInfo-curr_new_wts")
  
  expect_equal(nrow(dbSelect("select * from comments", db_backup)), 0)
  expect_equal(dbSelect("select * from package", db_backup)[["decision"]], NULL)
  
  # Set overall comment
  dbUpdate(
    "INSERT INTO comments
    VALUES ('dplyr', 'tester', 'admin', 'This is an overall comment', 'o', 'TODAY'),
    ('dplyr', 'tester', 'admin', 'This is a maintenance metric comment', 'mm', 'TODAY')",
    app_db_loc
  )
  # Set decision
  dbUpdate(
    "UPDATE package
          SET decision_id = 1, decision_by = 'tester', decision_date = 'TODAY'
          WHERE name = 'dplyr'",
    app_db_loc
  )
  
  db_backup <- app$get_download("reweightInfo-download_database_btn")
  app$wait_for_idle()
  app$click(selector = "#confirmation_id button")
  
  expect_equal(nrow(dbSelect("select * from comments", db_backup)), 2)
  expect_equal(dbSelect("select decision_id, decision_by, decision_date from package", db_backup) |> unlist(use.names = FALSE), c("1", "tester", "TODAY"))
  
  app$click("reweightInfo-update_pkg_risk")
  app$wait_for_idle()
  app$set_inputs(`reweightInfo-reset_decisions` = TRUE)
  app$click("reweightInfo-confirm_update_risk")
  
  db_backup <- app$get_download("reweightInfo-download_database_btn")
  app$wait_for_idle()
  app$click(selector = "#confirmation_id button")
  
  expect_equal(nrow(dbSelect("select * from comments where comment_type = 'o'", db_backup)), 0)
  expect_equal(nrow(dbSelect("select * from comments", db_backup)), 3)
  expect_equal(dbSelect("select decision_id, decision_by, decision_date from package", db_backup) |> unlist(use.names = FALSE), rep(NA_character_, 3))
  
  metric_weights <- app$get_value(export = "metric_weights")
  curr_new_wts <- app$get_value(export = "reweightInfo-curr_new_wts")
  expect_equal(metric_weights, curr_new_wts[,1:2])
  expect_equal(metric_weights[1:3,2], c(2,1,3.5))
  
  # Verify that app will not crash and metrics update if database is empty
  dbUpdate("DELETE FROM package", app_db_loc)
  
  expect_equal(nrow(dbSelect("SELECT DISTINCT name FROM package", app_db_loc)), 0)
  expect_equal(app$get_value(input = "reweightInfo-metric_name"), curr_new_wts[1,1])
  app$set_inputs(`reweightInfo-metric_weight` = 1)
  app$click("reweightInfo-update_weight")
  expect_equal(app$get_value(export = "metric_weights")[1,2], 2)
  
  app$click("reweightInfo-update_pkg_risk")
  app$wait_for_idle()
  app$click("reweightInfo-confirm_update_risk")
  expect_equal(app$get_value(export = "metric_weights")[1,2], 1)
})
