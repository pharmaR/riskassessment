test_that("pkg_explorer works", {
  
  # delete app DB if exists to ensure clean test
  app_db_loc <- test_path("test-apps", "explorer-app", "database.sqlite")
  if (file.exists(app_db_loc)) {
    file.remove(app_db_loc)
  }
  
  # copy in already instantiated database to avoid need to rebuild
  # this is a database that has been built via inst/testdata/upload_format.csv
  test_db_loc <- app_sys("testdata", "skeleton.sqlite")
  file.copy(
    test_db_loc,
    app_db_loc
  )
  
  app_tar_loc <- test_path("test-apps", "explorer-app", "tarballs", "magrittr_2.0.3.tar.gz")
  test_tar_loc <- app_sys("testdata", "magrittr_2.0.3.tar.gz")
  if (!file.exists(app_tar_loc)) {
    file.copy(
      test_tar_loc,
      app_tar_loc
    )
  }
  
  app_src_loc <- test_path("test-apps", "explorer-app", "source", "magrittr")
  if (!dir.exists(app_src_loc)) {
    untar(app_tar_loc, exdir = dirname(app_src_loc))
  }
  
  app <- shinytest2::AppDriver$new(test_path("test-apps", "explorer-app"))
  
  expect_true(TRUE)
  
})
