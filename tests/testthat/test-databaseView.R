test_that("Reactivity of database view table", {
  skip_on_ci()
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
  
  #### Test that the `table_data` loads correctly ####
  tbl_expect <-
    structure(list(name = "dplyr", version = "1.1.0", score = 0.29, 
                   decision = "-", decision_by = "-", decision_date = "-",
                   last_comment = "-"), 
              class = "data.frame", row.names = c(NA, -1L))
  tbl_actual <-
    app$get_value(export = "databaseView-table_data")
  
  expect_equal(tbl_actual, tbl_expect)
  
  #### Test that`table_data` updates in response to `changes` ####
  app$set_inputs(`sidebar-select_pkg` = "dplyr", timeout_ = 60 * 1000)
  app$click("sidebar-submit_decision")
  app$wait_for_idle()
  app$click("sidebar-submit_confirmed_decision")
  
  tbl_expect <-
    structure(list(name = "dplyr", version = "1.1.0", score = 0.29, 
                   decision = "Low Risk", decision_by = "test_user", decision_date = as.character(Sys.Date()),
                   last_comment = "-"), 
              class = "data.frame", row.names = c(NA, -1L))
  tbl_actual <-
    app$get_value(export = "databaseView-table_data")
  
  expect_equal(tbl_actual, tbl_expect)
  
  #### Test that`table_data` updates in response to `uploaded_pkgs` ####
  tbl_actual <-
    app$get_value(export = "databaseView-table_data")
  
  app$run_js("Shiny.setInputValue('upload_package-load_cran', 'load')")
  app$wait_for_idle()
  app$set_inputs(`upload_package-pkg_lst` = "tidyr")
  app$click("upload_package-add_pkgs", wait_ = FALSE)
  app$wait_for_value(export = "databaseView-table_data", 
                     ignore = tbl_actual, timeout = 30 * 1000 )
  
  tbl_expect <- structure(list(name = c("tidyr", "dplyr"), 
                               decision = c("-", "Low Risk"),
                               decision_by = c('-', "test_user"),
                               decision_date = c("-", as.character(Sys.Date())),
                               last_comment = c("-", "-")), 
                          class = "data.frame", row.names = c(NA, -2L))
  tbl_actual <-
    app$get_value(export = "databaseView-table_data")
  
  expect_equal(tbl_actual %>% dplyr::select(1,4,5,6,7) %>% dplyr::arrange(1), tbl_expect)
  
  #### Test that `packages_table` is loaded correctly ####
  tbl_actual <-
    app$get_value(export = "databaseView-table_data") # has tidyr first, then dplyr
  
  packages_table <-
    app$get_html("#databaseView-packages_table") %>%
    rvest::minimal_html() %>%
    rvest::html_table() %>%
    `[[`(1) %>% 
    select_if(!names(.) %in% c('Explore Metrics')) # added only to packages_table
  
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
