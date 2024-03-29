test_that("utils.R", {
      expect_type(getTimeStamp(), "character")
      expect_equal(object = getTimeStamp(),
                   expected = paste(gsub(x = Sys.time(), pattern = " ", replacement = "; "),
                                    Sys.timezone()), tolerance = 1e-02
                   )
      str = ";"
      expect_true(grepl(str, getTimeStamp(), fixed = TRUE))
      expect_true(grepl(Sys.timezone(), getTimeStamp(), fixed = TRUE))
})
