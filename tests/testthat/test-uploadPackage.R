
test_that("Uploaded packages show up in summary table", {
  # delete app DB if exists to ensure clean test
  db_loc <- test_path("test-apps", "database.sqlite")
  if (file.exists(db_loc)) {
    file.remove(db_loc)
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
    .[[2]]

  # read in raw data
  test_data <- read.csv(test_csv)

  # confirm packages from the two match
  expect_identical(
    sort(uploaded_packages$package),
    sort(test_data$package)
  )

  # confirm status for all is "new"
  expect_true(all(test_data$uploaded_packages == "new"))
})


test_that("Sample upload file can be shown and downloaded", {
  # delete app DB if exists to ensure clean test
  db_loc <- test_path("test-apps", "database.sqlite")
  if (file.exists(db_loc)) {
    file.remove(db_loc)
  }

  # set up new app driver object
  app <- AppDriver$new(app_dir = test_path("test-apps"))

  app$click(selector = "#upload_package-upload_format")
  app$wait_for_idle(1500)

  # table shown matches
  display_table <- app$get_html(".modal-body table") %>%
    .[[2]] %>%
    rvest::minimal_html() %>%
    rvest::html_table() %>%
    .[[1]] %>%
    .[, -1]

  # confirm match
  expect_identical(
    display_table,
    riskassessment:::template
  )

  # download file from application and read
  sample_file <- app$get_download("upload_package-download_sample")
  dl_data <- readr::read_csv(sample_file)

  # confirm match
  expect_identical(
    dl_data,
    riskassessment:::template
  )
})