
test_that("Uploaded packages show up in summary table", {
  # delete app DB if exists to ensure clean test
  if (file.exists(test_path("test-apps", "database.sqlite"))) {
    file.remove(test_path("test-apps", "database.sqlite"))
  }

  # set up new app driver object
  app <- AppDriver$new(app_dir = test_path("test-apps"))

  # test package data to upload
  test_csv <- system.file("extdata", "upload_format.csv", package = "riskassessment")

  # upload file to application
  app$upload_file(
    `upload_package-uploaded_file` = test_csv
  )

  # wait for table to be shown
  app$wait_for_value(
    output = "upload_package-upload_pkgs_table",
    ignore = list(NULL),
    timeout = 30 * 1000 # CI keeps failing here...
  )
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
