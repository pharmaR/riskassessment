test_that("downloadHandler works", {
  # delete app DB if exists to ensure clean test
  app_db_loc <- test_path("test-apps", "database.sqlite")
  if (file.exists(app_db_loc)) {
    file.remove(app_db_loc)
  }
  
  # copy in already instantiated database to avoid need to rebuild
  # this is a database that has been built via inst/testdata/upload_format.csv
  test_db_loc <- system.file("testdata", "dplyr_tidyr.sqlite", package = "riskassessment")
  file.copy(
    test_db_loc,
    app_db_loc
  )

  app <- AppDriver$new(test_path("test-apps"))
  
  app$set_inputs(`sidebar-select_pkg` = "dplyr")
  app$set_inputs("tabs" = "Report Preview")
  app$wait_for_idle()
  
  expect_equal(app$get_value(input = "reportPreview-downloadHandler-report_format"), "html")
  report <- app$get_download("reportPreview-downloadHandler-download_reports")
  expect_equal(tools::file_ext(report), "html")
  
  app$set_inputs(`reportPreview-downloadHandler-report_format` = "docx")
  report <- app$get_download("reportPreview-downloadHandler-download_reports")
  expect_equal(tools::file_ext(report), "docx")
  
  app$set_inputs(`reportPreview-downloadHandler-report_format` = "pdf")
  report <- app$get_download("reportPreview-downloadHandler-download_reports")
  expect_equal(tools::file_ext(report), "pdf")
  
  app$set_inputs(apptabs = "database-tab")
  app$wait_for_idle()  
  expect_equal(app$get_js("$('#databaseView-downloadHandler-download_reports').attr('disabled')"), "disabled")
  app$run_js("Shiny.setInputValue('databaseView-packages_table_rows_selected', 1)")
  app$wait_for_idle()
  expect_equal(app$get_js("$('#databaseView-downloadHandler-download_reports').attr('disabled')"), NULL)
  app$run_js("Shiny.setInputValue('databaseView-packages_table_rows_selected', null)")
  app$wait_for_idle()
  expect_equal(app$get_js("$('#databaseView-downloadHandler-download_reports').attr('disabled')"), "disabled")
  
  app$run_js("Shiny.setInputValue('databaseView-packages_table_rows_selected', [1,2])")
  app$wait_for_idle()
  report <- app$get_download("databaseView-downloadHandler-download_reports")
  expect_equal(tools::file_ext(report), "zip")
})