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
  
  app <- shinytest2::AppDriver$new(test_path("test-apps", "explorer-app"))
  
  app$set_inputs(tabs = "fn_expl_tab")
  app$wait_for_value(input = "fn_explorer-test_files")

  expect_equal(
    app$get_values(input = paste("fn_explorer", c("exported_function", "file_type"), sep = "-"))$input,
    list(`fn_explorer-exported_function` = ".data", `fn_explorer-file_type` = "test")
  )
  
  expect_equal(
    app$get_value(input = "fn_explorer-test_files"),
    "test-across.R"
  )
  
  expect_equal(
    app$get_text("#fn_explorer-file_output td.code pre.language-r")[1:10],
    c("# across ------------------------------------------------------------------", 
      "", "test_that(\"across() works on one column data.frame\", {", 
      "  df <- data.frame(x = 1)", "", "  out <- df %>% mutate(across(everything(), identity))", 
      "  expect_equal(out, df)", "})", "", "test_that(\"across() does not select grouping variables\", {"
    )
  )

  app$set_inputs(`fn_explorer-exported_function` = "arrange")
  app$wait_for_idle()
  
  expect_equal(
    app$get_value(input = "fn_explorer-test_files"),
    "test-arrange.R"
  )
  
  app$set_inputs(`fn_explorer-file_type` = "man")
  app$wait_for_idle()
  
  expect_equal(
    app$get_value(input = "fn_explorer-man_files"),
    "arrange.Rd"
  )
  
  app$expect_text("#fn_explorer-file_output div.container")
  
})
