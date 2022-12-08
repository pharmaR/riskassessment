
## test cases for functions in utils.R


testthat::test_that(desc = "Check output of get_date_span()", 
                    code = {
                      testthat::expect_equal(get_date_span(start = "2021-07-03", end = "2022-12-05")$value, 1)
                      testthat::expect_equal(get_date_span(start = "2021-07-03", end = "2022-12-05")$label, "Year")
                      testthat::expect_equal(get_date_span(start = "2020-02-26", end = "2020-05-04")$value, 2)
                      testthat::expect_equal(get_date_span(start = "2020-02-26", end = "2020-05-04")$label, "Months")
                      testthat::expect_equal(get_date_span(start = "2020-03-16", end = "2020-03-21")$value, 0)
                      testthat::expect_equal(get_date_span(start = "2020-03-16", end = "2020-03-21")$label, "Months")
                    })

