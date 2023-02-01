test_that("Reactivity of reportPreview", {
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
  
  # set pkg_name to dplyr
  app$set_inputs(`sidebar-select_pkg` = "dplyr")
  # get to the Report Preview tab
  app$set_inputs(tabs = "Report Preview")
  
  vals <- app$get_values()$input
   
 # set file type to pdf
  app$set_inputs(`reportPreview-downloadHandler-report_format` = "pdf")
  app$wait_for_idle()
  
  # verify it changed
  expect_equal(app$get_values()$input$`reportPreview-downloadHandler-report_format`, "pdf")
  
  # set file type back to default html
  app$set_inputs(`reportPreview-downloadHandler-report_format` = "html")
  app$wait_for_idle()
  
  expect_equal(app$get_values()$input$`reportPreview-downloadHandler-report_format`, "html")
  
  # download and check file type
  report <- app$get_download("reportPreview-downloadHandler-download_reports")
  app$wait_for_idle()

  # scrape output pkg_overview
  out_val <- app$get_values()$output$`reportPreview-pkg_overview`$html
  
  html <- rvest::read_html(out_val)
  pkg_rev <- rvest::html_nodes(html, "h5") %>% 
    rvest::html_text()
  
  # scrape similar section in html report
  html <- rvest::read_html(report)
  about <- html %>% 
    rvest::html_nodes(xpath="//h5[contains(., 'General Information')]/following-sibling::h6") %>% 
    rvest::html_nodes(xpath="//h5/preceding-sibling::h6") %>% 
    rvest::html_text()
  
  # first 8 should match
  expect_equal(about[1:8], pkg_rev)
  
  # scrape maintenance info headers
  maint_info <- rvest::html_elements(html,".card-header") %>% 
    rvest::html_text() %>% 
    paste(collapse = ", ")
  
  str_expect <- "Vignettes, NEWS file, NEWS current, Report Bugs, Website, Maintainer, Source Control, Documentation, Bugs Closure Rate, License, First Version Release, Latest Version Release, Package Downloads"
  
  expect_equal(maint_info, str_expect)
  
  app$stop()
  unlink("app_db_loc")
  rm(app, vals, html, about, maint_info, out_val, pkg_rev, report, str_expect, app_db_loc)
})