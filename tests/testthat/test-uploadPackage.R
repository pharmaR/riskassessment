library(shinytest2)

test_that("Uploaded packages show up in sidebar selector", {
  
  ## TODO:
  #   - confirm covr picks up coverage appropriately
  
  # reset DB to ensure clean test
  if (file.exists(test_path("testdata/database.sqlite"))) {
    file.remove(test_path("testdata/database.sqlite"))
  }
  
  # this is a slightly modified version of app.R in the package root. the 
  # modifications and location are needed to enable devtools::check to pass
  app <- AppDriver$new(app_dir = test_path("testdata/app.R"))
  
  # the test data being used -- currently duplicated from data-raw/ for similar
  # devtools::check-related reasons
  test_csv <- test_path("testdata/upload_format.csv")
  
  # upload the file to the app to load some test data
  app$upload_file(
    `upload_package-uploaded_file` = test_csv
  )
  
  # choose a package
  app$set_inputs(`sidebar-select_pkg` = c("DT"))
  
  # get the current app sate
  vals <- app$get_values()

  # parse out what packages are available as choices in the dropdown
  side_select <- rvest::minimal_html(vals$output$`sidebar-select_pkg_ui`$html)
  select_choices <- side_select %>% 
    rvest::html_elements("option") %>% 
    rvest::html_attr(name = "value")
  
  # "-" is added by default for UI; remove it for comparison
  select_choices <- setdiff(select_choices, "-")
  
  # read the uploaded file
  test_data <- read.csv(test_csv)
  
  # confirm two match
  expect_identical(
    sort(select_choices),
    sort(test_data$package)
  )
  
  app$stop()
})

