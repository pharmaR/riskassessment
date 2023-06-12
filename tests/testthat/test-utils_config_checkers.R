test_that("check_dec_cat works", {
  expect_error(
    check_dec_cat("Low"),
    "The number of decision categories must be at least 2"
  )
  
  expect_error(
    check_dec_cat(c("Low", "Medium", "Medium", "High")),
    "The decision categories must be unique"
  )
  
  expect_equal(check_dec_cat(c("Low", "Medium", "High")), NULL)
})

test_that("check_dec_rules works", {
  dec_cat <- c("Low", "Medium", "High")
  expect_error(
    check_dec_rules(dec_cat, list("Low" = list(0, .1), "Very High" = list(.7, 1))),
    "All decision rule categories should be included in the list of decision categories"
  )
  
  expect_error(
    check_dec_rules(dec_cat, list("Low" = list(0, .1), "Low" = list(0, .1))),
    "The decision categories must be unique for the decision rules"
  )
  
  expect_error(
    check_dec_rules(dec_cat, list("Low" = list(0, .1), "High" = list(.7, 'l'))),
    "The rules must be numeric values"
  )
  
  expect_error(
    check_dec_rules(dec_cat, list("Low" = list(0, .1), "High" = list(.7, Inf))),
    "All rules must be between 0 and 1"
  )
  
  expect_error(
    check_dec_rules(dec_cat, list("Low" = list(.7, 1), "High" = list(0, .1))),
    "The rules should be ascending in order of the categories"
  )
  
  expect_error(
    check_dec_rules(dec_cat, list("Low" = list(.1, .2), "High" = list(.7, 1))),
    "Rules for the first decision category must have a lower bound of 0"
  )
  
  expect_error(
    check_dec_rules(dec_cat, list("Low" = list(0, .1), "High" = list(.7, .9))),
    "Rules for the last decision category must have an upper bound of 1"
  )
  
  expect_equal(
    check_dec_rules(dec_cat, list("Low" = list(0, .1), "High" = list(.7, 1))),
    NULL
  )
})

test_that("check_metric_weights works", {
  
  allowed_lst <- c('has_vignettes', 'has_news', 'news_current', 'has_bug_reports_url', 'has_website', 'has_maintainer', 'has_source_control', 'export_help', 'bugs_status', 'license', 'covr_coverage', 'downloads_1yr')
  expect_error(
    check_metric_weights(list(has_vignette = -2, has_vignette = 0)),
    glue::glue("The metric weights must be a subset of the following: {paste(allowed_lst, collapse = ', ')}")
  )
  
  expect_error(
    check_metric_weights(list(has_vignettes = -2, has_vignettes = 0)),
    "The metric weights must be unique"
  )
  
  expect_error(
    check_metric_weights(list(has_vignettes = -2, covr_coverage = 0)),
    "The weights must be single, non-negative, numeric values"
  )
  
  expect_error(
    check_metric_weights(list(has_vignettes = c(0,1), covr_coverage = 0)),
    "The weights must be single, non-negative, numeric values"
  )
  
  expect_error(
    check_metric_weights(list(has_vignettes = "YES", covr_coverage = 0)),
    "The weights must be single, non-negative, numeric values"
  )
  
  expect_equal(
    check_metric_weights(list(has_vignettes = NULL, covr_coverage = 0)),
    NULL
  )
})

test_that("check_credentials works", {
  
  expect_error(
    check_credentials(list(roles = c("admin", "lead", "reviewer"))),
    "Both 'roles' and 'privileges' must be present in credentials configuration"
  )
  
  expect_error(
    check_credentials(list(roles = c("admin", "lead", "lead"), privileges = list())),
    "The roles must be unique"
  )
  
  expect_error(
    check_credentials(list(roles = c("admin", "lead", "reviewer"), privileges = list())),
    "The roles corresponding to 'admin' privileges must be specified"
  )
  
  expect_error(
    check_credentials(list(roles = c("admin", "reviewer"), privileges = list(admin = "admin", lead = "admin"))),
    "The following role(s) designated under privileges is(are) not present in the 'roles' configuration: lead",
    fixed = TRUE
  )
  
  expect_equal(
    check_credentials(list(roles = c("admin", "lead", "reviewer"), privileges = list(admin = "admin"))),
    NULL
  )
})
