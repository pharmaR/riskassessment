test_that("decision_automation works", {
  # delete app DB if exists to ensure clean test
  app_db_loc <- test_path("test-apps", "decision_automation-app", "database.sqlite")
  if (file.exists(app_db_loc)) {
    file.remove(app_db_loc)
  }
  
  # copy in already instantiated database to avoid need to rebuild
  # this is a database that has been built via inst/testdata/upload_format.csv
  test_db_loc <- app_sys("testdata", "decision_automation_ex1.sqlite")
  file.copy(
    test_db_loc,
    app_db_loc
  )
  
  app <- shinytest2::AppDriver$new(test_path("test-apps", "decision_automation-app"))
  
  # Check that reactive values are loaded correctly
  # Check datatable table
  expected <- structure(list(decision = c("Insignificant Risk", "Severe Risk"), 
                                       ll = c(0, 0.7), ul = c(0.1, 1)), 
                                  class = c("tbl_df", "tbl", "data.frame"), 
                                  row.names = c(NA, -2L))
  actual <- app$get_value(export = "automate-datatable")
  expect_equal(actual, expected)
  app$wait_for_idle()
  
  # Check module automate decision reactive
  expected <- list(`Insignificant Risk` = c(0, 0.1), 
                   `Severe Risk` = c(0.7, 1))
  actual <- app$get_value(export = "automate-auto_decision")
  expect_equal(actual, expected)
  
  # Check automate decision module output matches as well
  actual <- app$get_value(export = "auto_decision_output")
  expect_equal(actual, expected)
  
  # Check that inputs got set correctly on initialization
  # Check group checkbox
  expected <- c("Insignificant Risk", "Severe Risk")
  actual <- app$get_value(input = "automate-auto_include")
  expect_equal(actual, expected)
  
  # Check "Insignificant Risk" input
  expected <- 0.1
  actual <- app$get_value(input = "automate-insignificant_risk_attr")
  expect_equal(actual, expected)
  
  # Check "Severe Risk" input
  expected <- 0.7
  actual <- app$get_value(input = "automate-severe_risk_attr")
  expect_equal(actual, expected)
  
  
  # Set inputs for new decision rules
  app$set_inputs(`automate-auto_include` = c("Insignificant Risk", "Moderate Risk"),
                 `automate-moderate_risk_attr` = c(.3, .45))
  # Submit new decision rules
  app$click(input = "automate-submit_auto")
  app$wait_for_idle()
  app$click(input = "automate-confirm_submit_auto")
  app$wait_for_idle()
  
  # Verify that module output has updated
  # Check datatable table
  expected <- structure(list(decision = c("Insignificant Risk", "Moderate Risk"), 
                             ll = c(0, 0.3), ul = c(0.1, 0.45)), 
                        class = c("tbl_df", "tbl", "data.frame"), 
                        row.names = c(NA, -2L))
  actual <- app$get_value(export = "automate-datatable")
  expect_equal(actual, expected)
  
  # Check automate decision module output
  expected <- list(`Insignificant Risk` = c(0, 0.1), 
                   `Moderate Risk` = c(0.3, 0.45))
  actual <- app$get_value(export = "auto_decision_output")
  expect_equal(actual, expected)
  
  # Check that sliders are working correctly
  app$set_inputs(`automate-auto_include` = c("Insignificant Risk", "Moderate Risk", "Severe Risk"),
                 `automate-severe_risk_attr` = .4)
  app$wait_for_idle()
  
  # Check that Moderate Risk got adjusted
  expected <- list(`Insignificant Risk` = c(0, 0.1), `Severe Risk` = c(0.4, 1), `Moderate Risk` = c(0.3, 0.4))
  actual <- app$get_value(export = "automate-auto_decision")
  expect_equal(actual, expected)
  
  # Check that reset works correctly
  app$click(input = "automate-auto_reset")
  app$wait_for_idle()
  expected <- list(`Insignificant Risk` = c(0, 0.1), `Severe Risk` = NULL, `Moderate Risk` = c(0.3, 0.45))
  actual <- app$get_value(export = "automate-auto_decision")
  expect_equal(actual, expected)
  
})
