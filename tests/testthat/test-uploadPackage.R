

test_that("Uploaded packages show up in summary table", {
  
  # delete app DB if exists to ensure clean test
  app_db_loc <- test_path("test-apps", "database.sqlite")
  if (file.exists(app_db_loc)) {
    file.remove(app_db_loc)
  }

  # copy in already instantiated database to avoid need to rebuild
  # this is a database that has been built via inst/testdata/upload_format.csv
  test_db_loc <- system.file("testdata", "skeleton.sqlite", package = "riskassessment")
  file.copy(
    test_db_loc,
    app_db_loc
  )
  
  db <- dbSelect("select * from package;", app_db_loc)
  cat("\n", "in test-uploadPackage (1). ncols(db) should = 15 and is:", ncol(db), "\n")
  
  # set up new app driver object
  app <- shinytest2::AppDriver$new(app_dir = test_path("test-apps"))

  # test package data to upload
  test_csv <- system.file("testdata", "upload_format.csv", package = "riskassessment")

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
  app_db_loc <- test_path("test-apps", "database.sqlite")
  if (file.exists(app_db_loc)) {
    file.remove(app_db_loc)
  }

  # copy in already instantiated database to avoid need to rebuild
  # this is a database that has been built via inst/testdata/upload_format.csv
  test_db_loc <- system.file("testdata", "skeleton.sqlite", package = "riskassessment")
  file.copy(
    test_db_loc,
    app_db_loc
  )
  
  db <- dbSelect("select * from package;", app_db_loc)
  cat("\n", "in test-uploadPackage (2). ncols(db) should = 15 and is:", ncol(db), "\n")
  
  # set up new app driver object
  app <- shinytest2::AppDriver$new(app_dir = test_path("test-apps"), load_timeout = 600 * 1000)

  # click to show example upload table
  app$click(selector = "#upload_package-upload_format")
  app$wait_for_idle(1500)

  # parse displayed table for comparison
  display_table <- app$get_html(".modal-body table") %>%
    .[[2]] %>%
    rvest::minimal_html() %>%
    rvest::html_table() %>%
    .[[1]] %>%
    .[, -1]

  # remove the "spec" and "problem" attributes that exist on internal 
  # represenatation to allow for comparison; these are appended by readr
  template_tbl <- riskassessment:::template
  attr(template_tbl, "spec") <- NULL
  attr(template_tbl, "problems") <- NULL
  template_tbl <- as.data.frame(template_tbl)
  
  # confirm match
  expect_identical(
    as.data.frame(display_table),
    template_tbl
  )

  # download file from application and read
  sample_file <- app$get_download("upload_package-download_sample")
  dl_data <- read.csv(sample_file)

  # confirm match
  expect_identical(
    dl_data,
    template_tbl
  )
})

test_that("Removed packages show up in summary table", {
  
  # delete app DB if exists to ensure clean test
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

  db <- dbSelect("select * from package;", app_db_loc)
  cat("\n", "in test-uploadPackage (3). ncols(db) should = 15 and is:", ncol(db), "\n")
  
  pkgs <- dbSelect("select name from package", app_db_loc)[,1]
  expect_equal(length(pkgs), 2L)
  
  # set up new app driver object
  app <- shinytest2::AppDriver$new(app_dir = test_path("test-apps"), load_timeout = 600 * 1000)
  
  expect_equal(app$get_value(input = "tabs"), "Upload Package")
  
  # set focus
  app$run_js('Shiny.setInputValue("upload_package-curr_pkgs", "load", {priority: "event"})')
  app$wait_for_idle(1000)
  
  # set dplyr as package to remove
  app$set_inputs(`upload_package-rem_pkg_lst` = "dplyr")
  
  app$click(selector = "#upload_package-rem_pkg_btn")
  
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
  
  # expect status is removed
  expect_true(all(uploaded_packages$status == "removed"))
  expect_identical(uploaded_packages$package[1], "dplyr")
  
  # There should be just one package left in the db: stringr
  pkgs_left <- app$get_value(export = "databaseView-table_data")$name
  expect_equal(length(pkgs_left), 1L)
  expect_identical(pkgs_left[1], "tidyr")

  app$stop()
})
