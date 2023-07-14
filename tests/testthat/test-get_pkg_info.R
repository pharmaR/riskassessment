test_that("get_latest_pkg_info() in utils.R", {
      test <- get_latest_pkg_info("rpact")
      expect_error(get_latest_pkg_info("r_pact123"))
      
      expect_type(test, "list")
      expect_equal(colnames(test), c("Version", "Maintainer", "Author",
                                     "License", "Published", "Title",
                                     "Description"))
      expect_true(length(test) > 0)
      expect_true(nrow(test) > 0)
})

test_that("get_desc_pkg_info() in utils.R", {
  test <- get_desc_pkg_info("magrittr", "2.0.3", app_sys("testdata"))
  expect_error(get_desc_pkg_info("magittr", "2.0.3", app_sys("testdata")))
  
  expect_type(test, "list")
  expect_equal(colnames(test), c("Package", "Version", "Maintainer", 
                                 "Author", "License", "Published", 
                                 "Title", "Description"))
  expect_true(length(test) > 0)
  expect_true(nrow(test) > 0)
})
