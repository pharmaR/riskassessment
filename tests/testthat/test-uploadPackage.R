library(shinytest2)

test_that("Uploaded packages show up in sidebar selector", {
  
  ## TODO:
  #   - add mechanism to always start with clean db (e.g. delete)
  #   - confirm covr picks up coverage appropriately
  
  app <- AppDriver$new(app_dir = test_path("../../app.R"))
  
  test_csv <- test_path("../../data-raw/upload_format.csv")
  
  app$upload_file(
    `upload_package-uploaded_file` = test_csv
  )
  
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
