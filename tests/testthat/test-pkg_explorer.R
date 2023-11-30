test_that("pkg_explorer works", {
  
  # delete app DB if exists to ensure clean test
  app_db_loc <- test_path("test-apps", "explorer-app", "dplyr.sqlite")
  if (file.exists(app_db_loc)) {
    file.remove(app_db_loc)
  }
  
  # copy in already instantiated database to avoid need to rebuild
  # this is a database that has been built via inst/testdata/upload_format.csv
  test_db_loc <- app_sys("testdata", "upload_format.database")
  file.copy(
    test_db_loc,
    app_db_loc
  )
  
  app_tar_loc <- test_path("test-apps", "explorer-app", "tarballs", "dplyr_1.1.2.tar.gz")
  if (!dir.exists(dirname(app_tar_loc))) {
    dir.create(dirname(app_tar_loc))
  }
  if (!file.exists(app_tar_loc)) {
    download.file(
      "https://cran.r-project.org/src/contrib/Archive/dplyr/dplyr_1.1.2.tar.gz",
      app_tar_loc,
      mode = "wb"
    )
  }
  
  app_src_loc <- test_path("test-apps", "explorer-app", "source", "magrittr")
  if (!dir.exists(app_src_loc)) {
    untar(app_tar_loc, exdir = dirname(app_src_loc))
  }
  
  app <- shinytest2::AppDriver$new(test_path("test-apps", "explorer-app"))

  expect_equal(
    app$get_value(output = "src_explorer-is_file"),
    TRUE
  )
  
  app$expect_values(input = "src_explorer-dirtree", screenshot_args = FALSE)

  expect_equal(
    app$get_value(output = "src_explorer-filepath")$html,
    structure("<h5>DESCRIPTION</h5>", html = TRUE, class = c("html", "character"))
  )
  app$expect_values(input = "src_explorer-editor", screenshot_args = FALSE)
  
  app$run_js("$('#300 .jstree-ocl').click()")
  app$run_js("$('#301_anchor').click()")
  app$wait_for_idle()
  
  expect_equal(
    app$get_value(output = "src_explorer-filepath")$html,
    structure("<h5>tests/testthat.R</h5>", html = TRUE, class = c("html", "character"))
  )
  app$expect_values(input = "src_explorer-editor", screenshot_args = FALSE)
})
