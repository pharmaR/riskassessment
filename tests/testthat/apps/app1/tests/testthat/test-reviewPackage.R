
test_that("A package can be reviewed", {
  # set up new app driver object
  app <- AppDriver$new()

  # test package data to upload
  test_csv <- "upload_format.csv"

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
