test_that("downloadHandler works", {

  app <- shinytest2::AppDriver$new(test_path("test-apps", "downloadHandler-app"))
  
  expect_equal(app$get_value(input = "tabs"), "single")
  expect_equal(app$get_value(input = "downloadHandler_1-report_format"), "html")
  app$click("downloadHandler_1-create_reports", timeout_ = 30*1000)
  app$wait_for_idle()
  report <- app$get_download("downloadHandler_1-download_reports")
  expect_equal(tools::file_ext(report), "html")
  
  app$set_inputs(`downloadHandler_1-report_format` = "docx")
  app$click("downloadHandler_1-create_reports", timeout_ = 30*1000)
  app$wait_for_idle()
  report <- app$get_download("downloadHandler_1-download_reports")
  expect_equal(tools::file_ext(report), "docx")

  app$set_inputs(`downloadHandler_1-report_format` = "pdf")
  app$click("downloadHandler_1-create_reports", timeout_ = 30*1000)
  app$wait_for_idle()
  report <- app$get_download("downloadHandler_1-download_reports")
  expect_equal(tools::file_ext(report), "pdf")
  
  app$set_inputs(tabs = "multiple")
  expect_equal(app$get_value(input = "downloadHandler_2-report_format"), "html")
  app$click("downloadHandler_2-create_reports", timeout_ = 30*1000)
  app$wait_for_idle()
  report <- app$get_download("downloadHandler_2-download_reports")
  expect_equal(tools::file_ext(report), "zip")
})
