
test_that("A package can be reviewed", {
  
  # delete app DB if exists to ensure clean test
  if (file.exists(test_path("test-apps", "database.sqlite"))) {
    file.remove(test_path("test-apps", "database.sqlite"))
  }
  
  # set up new app driver object
  app <- AppDriver$new(app_dir = test_path("test-apps"))
  
  # test package data to upload
  test_csv <- system.file("extdata", "upload_format.csv", package = "riskassessment")
  
  # upload file to application
  app$upload_file(
    `upload_package-uploaded_file` = test_csv
  )

  # wait for table to be shown
  app$wait_for_value(
    output = "upload_package-upload_pkgs_table",
    ignore = list(NULL),
    timeout = 30 * 1000 # CI keeps failing here...
  )
  app$wait_for_idle(1000)


  # choose a package and a version
  app$set_inputs(`sidebar-select_pkg` = "dplyr")
  app$click("sidebar-submit_decision")
  app$wait_for_idle(1000)
  app$click("sidebar-submit_confirmed_decision")
  app$wait_for_idle(1000)


  # TODO: also add add negative proof that this value is not set before
  #       the above logic is run



  # show database tab
  app$click(selector = "#database-tab")
  app$wait_for_idle(1000)

  # read in database table
  db_table <- app$get_html("#databaseView-packages_table") %>%
    rvest::minimal_html() %>%
    rvest::html_table() %>%
    .[[1]]

  # confirm dplyr is now shown as "Low Risk"
  testthat::expect_equal(
    db_table[["Decision"]][db_table[["Package"]] == "dplyr"],
    "Low Risk"
  )

  # TODO: also confirm value is in database
  
})
