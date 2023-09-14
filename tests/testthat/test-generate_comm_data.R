
### Create tests for generate_comm_data

dat <- generate_comm_data("ggplot2")
test_that("func should return one row per month and year", {
  expect_equal(dat %>%
                 group_by(year, month) %>%
                 summarize(n = n()) %>%
                 filter(n > 1) %>% nrow
                 , 0)
})
test_that("Versions should aggregate when more than 1 version exists per year and month", {
  expect_equal(pull(dat[1, "version"]), "0.5 - 0.5.2")
})
test_that("data should contain every possible year month combo from month of first release until present month - 1", {
  mid_years <- seq(from = min(dat$year)+1, to = max(dat$year) - 1)
  year1_mnths <- dat$month[1]:12
  yearn_mnths <- 1:dat$month[nrow(dat)]
  all_combos <- expand.grid(year1_mnths, min(dat$year)) %>%
    union(expand.grid(1:12, mid_years))%>%
    union(expand.grid(yearn_mnths, max(dat$year)))
  colnames(all_combos) <- c("month", "year")
  expect_equal(dat %>% distinct(year, month) %>% as.data.frame, all_combos)
})
