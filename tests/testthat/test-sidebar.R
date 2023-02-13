test_that("Reactivity of sidebar", {
  library(shinytest2, quietly = TRUE)
  
  app_db_loc <- test_path("test-apps", "database.sqlite")
  if (file.exists(app_db_loc)) {
    file.remove(app_db_loc)
  }
  
  # copy in already instantiated database to avoid need to rebuild
  # this is a database created for test-downloadHandler
  test_db_loc <- test_path("test-apps", "downloadHandler-app", "dplyr_tidyr.sqlite")
  
  file.copy(
    test_db_loc,
    app_db_loc
  )
  
  # set up new app driver object
  app <- AppDriver$new(app_dir = test_path("test-apps"), load_timeout = 600*1000)
  
  # select_pkg is "-"
  expect_equal(app$get_value(input = "sidebar-select_pkg"), "-")
  
  # set select_pkg to "dplyr"
  app$set_inputs(`sidebar-select_pkg` = "dplyr")
  
  pkg_ver <- app$get_value(input = "sidebar-select_ver") %>% gsub(" - latest version","",.)
  # select_ver for "dplyr" should be >= "1.0.10"
  expect_true(pkg_ver >= "1.0.10")
  
  # status and risk messages should appear
  out_htm <- app$get_values()$output$`sidebar-status`$html
  status_txt <- rvest::read_html(out_htm) %>% rvest::html_text()
  # status is "Under Review"
  expect_equal(status_txt, "Under Review")
  
  out_htm <- app$get_values()$output$`sidebar-score`$html
  score_txt <- rvest::read_html(out_htm) %>% rvest::html_text()
  # numeric score is between 0 and 1, inclusive
  expect_true(dplyr::between(as.numeric(score_txt), 0, 1))
  
  # test slider...
  app$set_inputs(`sidebar-decision` = "1") # 0 = Low, 1 = Medium, 2 = High
  expect_equal(app$get_values()$input$`sidebar-decision`, "Medium")
  
  app$set_inputs(`sidebar-decision` = "2") # 0 = Low, 1 = Medium, 2 = High
  expect_equal(app$get_values()$input$`sidebar-decision`, "High")
  
  app$set_inputs(`sidebar-decision` = "0") # 0 = Low, 1 = Medium, 2 = High
  expect_equal(app$get_values()$input$`sidebar-decision`, "Low")
  
  # add a comment
  add_comment <- "This is a really useful package."
  app$set_inputs(`sidebar-overall_comment` = add_comment)
  
  app$click("sidebar-submit_overall_comment")
  app$wait_for_idle()
  
  # clear the modal
  app$run_js('document.getElementById(`shiny-modal`).click();')
  
  # button was pressed once
  expect_equal(app$get_value(input = "sidebar-submit_overall_comment")[1], 1L)
  
  # comment is hidden
  expect_equal(app$get_value(input = "sidebar-overall_comment"), "")
  
  # change the comment
  add_comment <- "OK maybe this is a great package."
  app$set_inputs(`sidebar-overall_comment` = add_comment)
  
  expect_equal(app$get_value(input = "sidebar-overall_comment"), add_comment)
  
  app$click("sidebar-submit_overall_comment")
  app$wait_for_idle()
  
  app$run_js('document.getElementById(`sidebar-submit_overall_comment_yes`).click();')
  
  # button was pressed twice
  expect_equal(app$get_value(input = "sidebar-submit_overall_comment")[1], 2L)
  
  # click on submit_decision and submit_confirmed_decision
  app$click("sidebar-submit_decision")
  app$wait_for_idle()
  app$click("sidebar-submit_confirmed_decision")
  
  # and status and risk messages should appear
  out_htm <- app$get_values()$output$`sidebar-status`$html
  status_txt <- rvest::read_html(out_htm) %>% rvest::html_text()
  # sidebar status set to Reviewed
  expect_equal(status_txt, "Reviewed")
  
  ##### this section only appears with user$role = "admin"
  out_htm <- app$get_values()$output$`sidebar-reset_decision_ui`$html
  if (!is.null(out_htm)) {
  score_txt <- rvest::read_html(out_htm) %>% rvest::html_text()
  } else {
    score_txt <- "Nothing here"
  }
  
  # do this if the Reset Decision button appeared
  if (score_txt == "Reset Decision") {
    # reset decision and confirm
    app$click("sidebar-reset_decision")
    app$wait_for_idle()
    app$click("sidebar-reset_confirmed_decision")
    # button pressed once
    expect_equal(app$get_value(input = "sidebar-reset_decision")[1], 1L)
    
    out_htm <- app$get_values()$output$`sidebar-status`$html
    status_txt <- rvest::read_html(out_htm) %>% rvest::html_text()
    # sidebar status is reset to "Under Review"
    expect_equal(status_txt, "Under Review")
  }
  #####

  # set select_pkg back to "-"
  app$set_inputs(`sidebar-select_pkg` = "-")
  # expect version to be set to "-" as well
  expect_equal(app$get_value(input = "sidebar-select_ver"), "-")
  
  # status and score have been reset
  expect_equal(app$get_value(output = "sidebar-status")$message, "Please select a package")
  expect_equal(app$get_value(output = "sidebar-score")$message, "Please select a package")
  
  app$stop()
  unlink("app_db_loc")
  rm(app, add_comment, out_htm, pkg_ver, score_txt, status_txt, app_db_loc)
})