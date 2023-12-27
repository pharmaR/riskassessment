test_that("downloadHandler works", {

  skip_on_ci()
  
  app <- shinytest2::AppDriver$new(test_path("test-apps", "downloadHandler-app"))
  
  expect_equal(app$get_value(input = "tabs"), "single")
  expect_equal(app$get_value(input = "downloadHandler_1-report_format"), "html")
  report <- app$get_download("downloadHandler_1-download_reports")
  expect_equal(tools::file_ext(report), "html")
  
  app$set_inputs(`downloadHandler_1-report_format` = "docx")
  report <- app$get_download("downloadHandler_1-download_reports")
  expect_equal(tools::file_ext(report), "docx")

  app$set_inputs(`downloadHandler_1-report_format` = "pdf")
  report <- app$get_download("downloadHandler_1-download_reports")
  expect_equal(tools::file_ext(report), "pdf")
  
  app$set_inputs(tabs = "multiple")
  expect_equal(app$get_value(input = "downloadHandler_2-report_format"), "html")
  report <- app$get_download("downloadHandler_2-download_reports")
  expect_equal(tools::file_ext(report), "zip")
})
