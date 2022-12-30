
test_that("Uploaded packages show up in sidebar selector", {
  
  # set up new app driver object
  app <- AppDriver$new()

  # test package data to upload
  test_csv <- "upload_format.csv"
  
  # upload file to 
  app$upload_file(
    `upload_package-uploaded_file` = test_csv
  )

  # choose a package to force input update
  app$set_inputs(`sidebar-select_pkg` = c("DT"))
  
  # wait for app to stabilize
  app$wait_for_idle(500)
    
  # get current app state
  vals <- app$get_values()
  
  # check all uploaded packages show up in selectize input
  side_select <- rvest::minimal_html(vals$output$`sidebar-select_pkg_ui`$html)
  select_choices <- side_select %>% 
    rvest::html_elements("option") %>% 
    rvest::html_attr(name = "value")
  
  # "-" is added by default
  select_choices <- setdiff(select_choices, "-")
  
  # read in raw data
  test_data <- read.csv(test_csv)

  # confirm values from the two match
  expect_identical(
    sort(select_choices),
    sort(test_data$package)
  )
})
