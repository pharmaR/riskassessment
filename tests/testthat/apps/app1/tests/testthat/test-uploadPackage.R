library(shinytest2)
library(testthat)

test_that("Uploaded packages show up in sidebar selector", {
  
  ## TODO:
  #   - confirm covr picks up coverage appropriately
  app <- AppDriver$new()

  # test_csv <- app_sys("data-raw/upload_format.csv")
  test_csv <- "upload_format.csv"
  
  # upload file to 
  app$upload_file(
    `upload_package-uploaded_file` = test_csv
  )

  # choose a package to force input update
  app$set_inputs(`sidebar-select_pkg` = c("DT"))
  
  # wait for app to stabilize
  app$wait_for_idle(500)
    
  # check all uploaded packages show up in selectize input
  vals <- app$get_values()
  
  side_select <- rvest::minimal_html(vals$output$`sidebar-select_pkg_ui`$html)
  select_choices <- side_select %>% 
    rvest::html_elements("option") %>% 
    rvest::html_attr(name = "value")
  
  # "-" is added by default
  select_choices <- setdiff(select_choices, "-")
  
  # confirm two match
  test_data <- read.csv(test_csv)
  
  expect_identical(
    sort(select_choices),
    sort(test_data$package)
  )
})
