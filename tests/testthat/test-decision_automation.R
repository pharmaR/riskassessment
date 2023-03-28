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
  
  
})