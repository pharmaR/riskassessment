
### Create tests for autofont


test_that("no extra args - font is proportional and hits floor at .75", {
  long_str <- "https://github.com/tidyverse/dplyr/issues"
  expect_equal(auto_font(long_str,), .817)
  expect_equal(auto_font(paste0(long_str, "+")), .800)
  expect_equal(auto_font(paste0(long_str, "pl")), .783)
  expect_equal(auto_font(paste0(long_str, "plu")), .767)
  expect_equal(auto_font(paste0(long_str, "plus")), .75)
})

test_that("adjusting 'txt_max' changes proportion for long strings", {
  long_str <- "https://github.com/tidyverse/dplyr/issues"
  expect_equal(auto_font(long_str, txt_max = 55), .941)
  expect_equal(auto_font(paste0(long_str, "+"), txt_max = 55), .927)
})

test_that("adjusting 'size_min' changes proportion for long strings", {
  long_str <- "https://github.com/tidyverse/dplyr/issues"
  expect_equal(auto_font(long_str, size_min = .5), .589)
  expect_equal(auto_font(paste0(long_str, "+"), size_min = .5), .567)
})

test_that("Adding `bins` argument returns categorical options", {
  short_str <- ""
  med_str <- "https://github.com/"
  long_str <- "https://github.com/tidyverse/dplyr/issues"
  
  expect_equal(auto_font(short_str, num_bins = 3), 1.5)
  expect_equal(auto_font(paste0(short_str, "+"), num_bins = 3), 1.5)
  
  expect_equal(auto_font(med_str, num_bins = 3), 1.125)
  expect_equal(auto_font(paste0(med_str, "+"), num_bins = 3), 1.125)
  
  expect_equal(auto_font(long_str, num_bins = 3), .75)
  expect_equal(auto_font(paste0(long_str, "+"), num_bins = 3), .75)
})



test_that("no extra args - font is proportional and hits ceiling at 1.5", {
  str <- ""
  expect_equal(auto_font(str), 1.5)
  expect_equal(auto_font(paste0(str, "+")), 1.483)
  expect_equal(auto_font(paste0(str, "pl")), 1.467)
  expect_equal(auto_font(paste0(str, "plu")), 1.450)
  expect_equal(auto_font(paste0(str, "plus")), 1.433)
})

test_that("adjusting 'txt_max' changes proportion for short strings", {
  str <- ""
  expect_equal(auto_font(str, txt_max = 10), 1.5)
  expect_equal(auto_font(paste0(str, "+"), txt_max = 10), 1.425)
})

test_that("adjusting 'size_max' changes proportion for short strings", {
  str <- ""
  expect_equal(auto_font(str, size_max = 2.0), 2)
  expect_equal(auto_font(paste0(str, "+"), size_max = 2.0), 1.972)
})






