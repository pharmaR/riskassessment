
test_that("Uploaded packages show up in sidebar selector", {
  
  # reset DB to ensure clean test
  if (file.exists(test_path("apps/app1/database.sqlite"))) {
    file.remove(test_path("apps/app1/database.sqlite"))
  }
  
  # mimic test folder structure and execution mechanism  
  # https://github.com/rstudio/shinytest2/blob/b47a0db2749dfd781ea92c4697784e9c69b9ebc0/tests/testthat/test-apps.R#L43
  shinytest2::test_app(fs::dir_ls(testthat::test_path("apps"), type = "directory"))
  expect_equal(1, 1) # needed to not skip empty test
})

