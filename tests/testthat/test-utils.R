
## test cases for functions in utils.R
testthat::test_that(desc = "Check output of get_date_span()", 
                    code = {
                      testthat::expect_equal(object = get_date_span(start = "2021-07-03", end = "2022-12-05")$value, expected = 1)
                      testthat::expect_equal(object = get_date_span(start = "2021-07-03", end = "2022-12-05")$label, expected = "Year")
                      testthat::expect_equal(object = get_date_span(start = "2020-02-26", end = "2020-05-04")$value, expected = 2)
                      testthat::expect_equal(object = get_date_span(start = "2020-02-26", end = "2020-05-04")$label, expected = "Months")
                      testthat::expect_equal(object = get_date_span(start = "2020-03-16", end = "2020-03-21")$value, expected = 0)
                      testthat::expect_equal(object = get_date_span(start = "2020-03-16", end = "2020-03-21")$label, expected = "Months")
                      
                      ## check error message when function is called without any argument
                      testthat::expect_error(object = get_date_span(), regexp = 'argument "start" is missing, with no default')
                    })
