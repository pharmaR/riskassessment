test_that("Reactivity of database view table", {
  # delete app DB if exists to ensure clean test
  app_db_loc <- test_path("test-apps", "database.sqlite")
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
  
  # set up new app driver object
  app <- shinytest2::AppDriver$new(app_dir = test_path("test-apps"))
  
  app$set_inputs(apptabs = "database-tab")
  
  app$expect_values(export = "databaseView-table_data", screenshot_args = FALSE)
  
  #### Test that`table_data` updates in response to `changes` ####
  app$set_inputs(`sidebar-select_pkg` = "dplyr", timeout_ = 60 * 1000)
  app$click("sidebar-submit_decision")
  app$wait_for_idle()
  app$click("sidebar-submit_confirmed_decision")
  
  app$expect_values(export = "databaseView-table_data", screenshot_args = FALSE)
  
  #### Test that`table_data` updates in response to `uploaded_pkgs` ####
  tbl_actual <-
    app$get_value(export = "databaseView-table_data")
  
  app$run_js("Shiny.setInputValue('upload_package-load_cran', 'load')")
  app$wait_for_idle()
  app$set_inputs(`upload_package-pkg_lst` = "tidyr")
  app$click("upload_package-add_pkgs", wait_ = FALSE)
  app$wait_for_value(export = "databaseView-table_data", 
                     ignore = tbl_actual, timeout = 30 * 1000 )
  
  app$expect_values(export = "databaseView-table_data", screenshot_args = FALSE)
  
  #### Test that `packages_table` is loaded correctly ####
  tbl_actual <-
    app$get_value(export = "databaseView-table_data") %>% # has tidyr first, then dplyr
    mutate(across(everything(), as.character))
  
  packages_table <-
    app$get_html("#databaseView-packages_table") %>%
    rvest::minimal_html() %>%
    rvest::html_table() %>%
    `[[`(1) %>% 
    select(-'Explore Metrics') %>% # added only to packages_table
    filter(Package != "") %>%
    mutate(across(everything(), ~ na_if(as.character(.x), "")))
  
  expect_equal(packages_table, tbl_actual, 
               ignore_attr = TRUE)
  
  #### Test that the selected packages are being passed to the download handler ####
  expect_equal(app$get_value(export = "databaseView-pkgs"), character(0))
  app$run_js("Shiny.setInputValue('databaseView-packages_table_rows_selected', 1)")
  app$wait_for_idle()
  expect_equal(app$get_value(export = "databaseView-pkgs"), "tidyr")
  app$run_js("Shiny.setInputValue('databaseView-packages_table_rows_selected', [1,2])")
  app$wait_for_idle()
  expect_equal(app$get_value(export = "databaseView-pkgs"), c("tidyr", "dplyr"))
  app$run_js("Shiny.setInputValue('databaseView-packages_table_rows_selected', null)")
  app$wait_for_idle()
  expect_equal(app$get_value(export = "databaseView-pkgs"), character(0))
  
  app$stop()
  
})
