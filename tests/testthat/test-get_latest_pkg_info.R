test_that("get_latest_pkg_info() in utils.R", {
      test <- get_latest_pkg_info("rpact")
      
      expect_type(test, "list")
      expect_equal(colnames(test), c("Version", "Maintainer", "Author",
                                     "License", "Published", "Title",
                                     "Description"))
      expect_true(length(test) > 0)
      expect_true(nrow(test) > 0)
})
