
test_that("Uploaded packages show up in summary table", {
  # set up new app driver object
  app <- AppDriver$new()

  # test package data to upload
  test_csv <- "upload_format.csv"

  # upload file to application
  app$upload_file(
    `upload_package-uploaded_file` = test_csv
  )

  # wait for table to be shown
  app$wait_for_value(output = "upload_package-upload_pkgs_table", ignore = list(NULL))
  app$wait_for_idle(1000)
  
  # parse the package name from the upload summary table
  uploaded_packages <- app$get_html("#upload_package-upload_pkgs_table") %>%
    rvest::minimal_html() %>%
    rvest::html_table() %>%
    .[[2]] %>% 
    .[["package"]]

  # read in raw data
  test_data <- read.csv(test_csv)
  
  # confirm packages from the two match
  expect_identical(
    sort(uploaded_packages),
    sort(test_data$package)
  )
})
