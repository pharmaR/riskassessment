test_that("Reactivity of maintenanceMetrics", {
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
  app$wait_for_idle()
  
  # set pkg_name to dplyr
  app$set_inputs(`sidebar-select_pkg` = "dplyr")
  
  # get to the Maintenance Metrics tab
  app$set_inputs(tabs = "Package Metrics",
                 metric_type = "mm")
  app$wait_for_idle()
  
  # read the current comment -- set to "No comments"
  out_cmt <- app$get_value(output = "maintenanceMetrics-view_comments-view_comments")$html
  
  cmt_txt <- rvest::read_html(out_cmt) %>%  
    rvest::html_nodes(xpath = '//div[@class="well"]/text()') %>% 
    purrr::map_chr(., ~as.character(.x))
  
  expect_equal(cmt_txt, "No comments")
  
  # add a comment
  add_comment <- "This is a well-maintained package."
  app$set_inputs(`maintenanceMetrics-add_comment-add_comment` = add_comment)
  
  app$click("maintenanceMetrics-add_comment-submit_comment")
  app$wait_for_idle()
  
  # check some of the card footers
  out_val <- app$get_value(output = "maintenanceMetrics-metricGrid-has_source_control-metricBox_ui")$html
  pkg_url <- rvest::read_html(out_val) %>% 
    rvest::html_elements(.,".card-footer") %>% 
    rvest::html_text() 
  expect_equal(trimws(pkg_url), "Package source control url")
  
  out_val <- app$get_value(output = "maintenanceMetrics-metricGrid-has_vignettes-metricBox_ui")$html
  vignettes <- rvest::read_html(out_val) %>% 
    rvest::html_elements(.,".card-footer") %>% 
    rvest::html_text() 
  expect_equal(trimws(vignettes), "Number of vignettes")
  
  out_val <- app$get_value(output = "maintenanceMetrics-metricGrid-news_current-metricBox_ui")$html
  news_curr <- rvest::read_html(out_val) %>% 
    rvest::html_elements(.,".card-footer") %>% 
    rvest::html_text() 
  expect_equal(trimws(news_curr), "NEWS contains current version")
  
  # read the comment back in
  out_cmt <- app$get_value(output = "maintenanceMetrics-view_comments-view_comments")$html
  
  cmt_txt <- rvest::read_html(out_cmt) %>%  
    rvest::html_nodes(xpath = '//div[@class="well"]/text()') %>% 
    purrr::map_chr(., ~as.character(.x))
  
  # is the last text entry the comment we entered?
  expect_equal(cmt_txt[length(cmt_txt)], add_comment)
  
  app$stop()
  unlink("app_db_loc")
  rm(app, add_comment, out_val, pkg_url, vignettes, news_curr, out_cmt, cmt_txt, app_db_loc)
  
})
