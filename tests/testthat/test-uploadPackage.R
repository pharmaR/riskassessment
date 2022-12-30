
test_that("Uploaded packages show up in sidebar selector", {

  # https://github.com/rstudio/shinytest2/blob/b47a0db2749dfd781ea92c4697784e9c69b9ebc0/tests/testthat/test-apps.R#L43

  shinytest2::test_app(fs::dir_ls(testthat::test_path("apps"), type = "directory"))
      
})
