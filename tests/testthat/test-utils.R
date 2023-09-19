test_that("Test showHelperMessage() works", {
  msg <- showHelperMessage(message = "This is a test message!")
  testthat::expect_equal(object = msg$children[[1]][1], expected = "This is a test message!")
  testthat::expect_equal(object = msg$name, expected = "h6")
  
  ## check HTML attributes of the message
  attribs <- stringr::str_split(msg$attribs$style, pattern = "[;\n]")[[1]]  ## split at either semi-colon OR newline
  testthat::expect_equal(object = attribs[1], expected = "text-align: center")
  testthat::expect_equal(object = stringr::str_split(string = attribs[3], pattern = "^\\s+")[[1]][2], 
                         expected = "color: gray")  ## split at one or more white spaces in the beginning
  testthat::expect_equal(object = stringr::str_split(string = attribs[5], pattern = "^\\s+")[[1]][2], 
                         expected = "padding-top: 50px")
})

test_that("Test that get_latest_pkg_info() works", {
  test <- get_latest_pkg_info("rpact")
  expect_error(get_latest_pkg_info("r_pact123"))
  
  expect_type(test, "list")
  expect_equal(colnames(test), c("Version", "Maintainer", "Author",
                                 "License", "Published", "Title",
                                 "Description"))
  expect_true(length(test) > 0)
  expect_true(nrow(test) > 0)
})

test_that("Test that get_desc_pkg_info() works", {
  test <- get_desc_pkg_info("magrittr", "2.0.3", app_sys("testdata"))
  expect_error(get_desc_pkg_info("magittr", "2.0.3", app_sys("testdata")))
  
  expect_type(test, "list")
  expect_equal(colnames(test), c("Package", "Version", "Maintainer", 
                                 "Author", "License", "Published", 
                                 "Title", "Description"))
  expect_true(length(test) > 0)
  expect_true(nrow(test) > 0)
})

test_that("Test that generate_comm_data() works", {
  dat <- generate_comm_data("ggplot2")
  
  expect_equal(dat %>%
                 group_by(year, month) %>%
                 summarize(n = n()) %>%
                 filter(n > 1) %>% nrow
               , 0)
  
  expect_equal(pull(dat[1, "version"]), "0.5 - 0.5.2")
  
  mid_years <- (min(dat$year)+1):(max(dat$year) - 1)
  year1_mnths <- dat$month[1]:12
  yearn_mnths <- 1:dat$month[nrow(dat)]
  all_combos <- expand.grid(year1_mnths, min(dat$year)) %>%
    union(expand.grid(1:12, mid_years))%>%
    union(expand.grid(yearn_mnths, max(dat$year)))
  colnames(all_combos) <- c("month", "year")
  expect_equal(dat %>% distinct(year, month) %>% as.data.frame, all_combos)
})

test_that("Test that showComments() works", {
  skip("Placeholder for showComments()")
})

test_that("Test that getTimeStamp() works", {
  expect_type(getTimeStamp(), "character")
  expect_equal(object = getTimeStamp(),
               expected = paste(gsub(x = Sys.time(), pattern = " ", replacement = "; "),
                                Sys.timezone()), tolerance = 1e-02
  )
  str = ";"
  expect_true(grepl(str, getTimeStamp(), fixed = TRUE))
  expect_true(grepl(Sys.timezone(), getTimeStamp(), fixed = TRUE))
})

test_that("Test that get_date_span() works", {
  testthat::expect_equal(object = get_date_span(start = "2021-07-03", end = "2022-12-05")$value, expected = 1)
  testthat::expect_equal(object = get_date_span(start = "2021-07-03", end = "2022-12-05")$label, expected = "Year")
  testthat::expect_equal(object = get_date_span(start = "2020-02-26", end = "2020-05-04")$value, expected = 2)
  testthat::expect_equal(object = get_date_span(start = "2020-02-26", end = "2020-05-04")$label, expected = "Months")
  testthat::expect_equal(object = get_date_span(start = "2020-03-16", end = "2020-03-21")$value, expected = 0)
  testthat::expect_equal(object = get_date_span(start = "2020-03-16", end = "2020-03-21")$label, expected = "Months")
  
  ## check error message when function is called without any argument
  testthat::expect_error(object = get_date_span(), regexp = "argument \"start\" is missing, with no default")
})

test_that("Test that build_comm_cards() works", {
  skip("Placeholder for build_comm_cards()")
})

test_that("Test that build_db_cards() works", {
  skip("Placeholder for build_db_cards()")
})

test_that("Test that auto_font() works", {
  # No extra args - font is proportional and hits floor at .75
    long_str <- "https://github.com/tidyverse/dplyr/issues"
    expect_equal(auto_font(long_str,), .817)
    expect_equal(auto_font(paste0(long_str, "+")), .800)
    expect_equal(auto_font(paste0(long_str, "pl")), .783)
    expect_equal(auto_font(paste0(long_str, "plu")), .767)
    expect_equal(auto_font(paste0(long_str, "plus")), .75)

  # Adjusting 'txt_max' changes proportion for long strings
    long_str <- "https://github.com/tidyverse/dplyr/issues"
    expect_equal(auto_font(long_str, txt_max = 55), .941)
    expect_equal(auto_font(paste0(long_str, "+"), txt_max = 55), .927)

  # Adjusting 'size_min' changes proportion for long strings
    long_str <- "https://github.com/tidyverse/dplyr/issues"
    expect_equal(auto_font(long_str, size_min = .5), .589)
    expect_equal(auto_font(paste0(long_str, "+"), size_min = .5), .567)

  # Adding `bins` argument returns categorical options
    short_str <- ""
    med_str <- "https://github.com/"
    long_str <- "https://github.com/tidyverse/dplyr/issues"
    
    expect_equal(auto_font(short_str, num_bins = 3), 1.5)
    expect_equal(auto_font(paste0(short_str, "+"), num_bins = 3), 1.5)
    
    expect_equal(auto_font(med_str, num_bins = 3), 1.125)
    expect_equal(auto_font(paste0(med_str, "+"), num_bins = 3), 1.125)
    
    expect_equal(auto_font(long_str, num_bins = 3), .75)
    expect_equal(auto_font(paste0(long_str, "+"), num_bins = 3), .75)

  # No extra args - font is proportional and hits ceiling at 1.5
    str <- ""
    expect_equal(auto_font(str), 1.5)
    expect_equal(auto_font(paste0(str, "+")), 1.483)
    expect_equal(auto_font(paste0(str, "pl")), 1.467)
    expect_equal(auto_font(paste0(str, "plu")), 1.450)
    expect_equal(auto_font(paste0(str, "plus")), 1.433)

  # Adjusting 'txt_max' changes proportion for short strings
    str <- ""
    expect_equal(auto_font(str, txt_max = 10), 1.5)
    expect_equal(auto_font(paste0(str, "+"), txt_max = 10), 1.425)

  # Adjusting 'size_max' changes proportion for short strings
    str <- ""
    expect_equal(auto_font(str, size_max = 2.0), 2)
    expect_equal(auto_font(paste0(str, "+"), size_max = 2.0), 1.972)
})

test_that("Test that build_comm_plotly() works", {
  skip("Placeholder for build_comm_plotly()")
})

test_that("Test that get_Date() works", {
  expect_equal(get_Date(), Sys.Date())
  oo <- options(shiny.testmode = TRUE)
  expect_equal(get_Date(), as.Date("2023-07-20"))
  options(oo)
})

test_that("Test that get_time() works", {
  expect_equal(get_time(), Sys.time())
  oo <- options(shiny.testmode = TRUE)
  expect_equal(get_time(), as.POSIXct("2023-07-20 08:00:00 EDT"))
  options(oo)
})

test_that("Test that build_dep_cards() works", {
  skip("Placeholder for build_dep_cards()")
})
