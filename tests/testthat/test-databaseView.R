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
  app <- AppDriver$new(app_dir = test_path("test-apps"))
  
  app$set_inputs(apptabs = "database-tab")
  
  #### Test that the `table_data` loads correctly ####
  tbl_expect <-
    structure(list(name = "dplyr", version = "1.0.10", score = 0.1, 
                   was_decision_made = FALSE, decision = "-", 
                   last_comment = "-"), 
              class = "data.frame", row.names = c(NA, -1L))
  tbl_actual <-
    app$get_value(export = "databaseView-table_data")
  
  expect_equal(tbl_actual, tbl_expect)
  
  #### Test that`table_data` updates in response to `changes` ####
  app$set_inputs(`sidebar-select_pkg` = "dplyr")
  app$click("sidebar-submit_decision")
  app$wait_for_idle()
  app$click("sidebar-submit_confirmed_decision")
  
  tbl_expect <-
    structure(list(name = "dplyr", version = "1.0.10", score = 0.1, 
                   was_decision_made = TRUE, decision = "Low Risk", 
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
                               was_decision_made = c(FALSE, TRUE), 
                               decision = c("-", "Low Risk"), 
                               last_comment = c("-", "-")), 
                          class = "data.frame", row.names = c(NA, -2L))
  tbl_actual <-
    app$get_value(export = "databaseView-table_data")
  
  expect_equal(tbl_actual %>% dplyr::select(1,4,5,6) %>% dplyr::arrange(1), tbl_expect)
  
  #### Test that `packages_table` is loaded correctly ####
  tbl_actual <-
    app$get_value(export = "databaseView-table_data") %>% 
    dplyr::mutate(was_decision_made = dplyr::if_else(was_decision_made, "Yes", "No"))
  
  packages_table <-
    app$get_html("#databaseView-packages_table") %>%
    rvest::minimal_html() %>%
    rvest::html_table() %>%
    `[[`(1)
  
  expect_equal(packages_table, tbl_actual, 
               ignore_attr = TRUE)
  
  #### Test that the download button is disabled if no packages selected ####
  expect_equal(app$get_js("$('#databaseView-download_reports').attr('disabled')"), "disabled")
  app$run_js("Shiny.setInputValue('databaseView-packages_table_rows_selected', 1)")
  app$wait_for_idle()
  expect_equal(app$get_js("$('#databaseView-download_reports').attr('disabled')"), NULL)
  app$run_js("Shiny.setInputValue('databaseView-packages_table_rows_selected', null)")
  app$wait_for_idle()
  expect_equal(app$get_js("$('#databaseView-download_reports').attr('disabled')"), "disabled")
  
  #### Test that the download file works as expected ####
  expect_equal(app$get_value(input = "databaseView-report_formats"), "html")
  app$run_js("Shiny.setInputValue('databaseView-packages_table_rows_selected', 1)")
  app$wait_for_idle()
  report <- app$get_download("databaseView-download_reports")
  expect_equal(tools::file_ext(report), "html")
  
  # TODO: Add DOCX and PDF tests back in when issues are resolved with compiling the Rmd file
  # app$set_inputs(`databaseView-report_formats` = "docx")
  # report <- app$get_download("databaseView-download_reports")
  # expect_equal(tools::file_ext(report), "docx")
  # 
  # # app$set_inputs(`databaseView-report_formats` = "pdf")
  # # report <- app$get_download("databaseView-download_reports")
  # # expect_equal(tools::file_ext(report), "pdf")
  # # 
  # app$set_inputs(`databaseView-report_formats` = "html")
  app$run_js("Shiny.setInputValue('databaseView-packages_table_rows_selected', [1,2])")
  app$wait_for_idle()
  report <- app$get_download("databaseView-download_reports")
  expect_equal(tools::file_ext(report), "zip")
  
  app$stop()
})
