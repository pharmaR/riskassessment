test_that("Reactivity of communityMetrics", {
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
  
  # set pkg_name to dplyr
  app$set_inputs(`sidebar-select_pkg` = "dplyr")
  
  # get to the Maintenance Metrics tab
  app$set_inputs(tabs = "Community Usage Metrics")
  
  # read the current comment
  out_cmt <- app$get_values()$output$`communityMetrics-view_comments-view_comments`$html
  
  cmt_txt <- rvest::read_html(out_cmt) %>%  
    rvest::html_nodes(xpath = '//div[@class="well"]/text()') %>% 
    purrr::map_chr(., ~as.character(.x))
  
  expect_equal(cmt_txt, "No comments")
  
  # add a comment
  add_comment <- "This package gets lots of downloads."
  app$set_inputs(`communityMetrics-add_comment-add_comment` = add_comment)
  
  app$click("communityMetrics-add_comment-submit_comment")
  app$wait_for_idle()
  
  # read the comment back in
  out_cmt <- app$get_values()$output$`communityMetrics-view_comments-view_comments`$html
  
  cmt_txt <- rvest::read_html(out_cmt) %>%  
    rvest::html_nodes(xpath = '//div[@class="well"]/text()') %>% 
    purrr::map_chr(., ~as.character(.x))
  
  # is the last text entry the comment we entered?
  expect_equal(cmt_txt[length(cmt_txt)], add_comment)
  
  app$stop()
  unlink("app_db_loc")
  rm(app, add_comment, out_cmt, cmt_txt, app_db_loc)
})