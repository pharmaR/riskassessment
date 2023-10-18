test_that("set_colors works", {
  expect_equal(
    set_colors(1:3),
    c(`1` = "#9CFF94", `2` = "#F8D95D", `3` = "#FF765B")
  )
  
  expect_equal(
    set_colors(1:20), # achere
    c(`1` = "#9CFF94", `2` = "#2FBC06", `3` = "#67BA04", `4` = "#81B50A", 
      `5` = "#96AB0A", `6` = "#A99D04", `7` = "#A99D04", `8` = "#B78D07", 
      `9` = "#BE7900", `10` = "#BE6200", `11` = "#B24F22", `12` = "#A63E24", 
      `13` = "#A63E24", `14` = "#A63E24", `15` = "#A63E24", `16` = "#A63E24", 
      `17` = "#A63E24", `18` = "#A63E24", `19` = "#A63E24", `20` = "#A63E24"
    )
  )
})

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
    "All decision rules should be either named after a decision category or following the convention `rule_\\{d\\}`"
  )
  
  expect_error(
    check_dec_rules(dec_cat, list("Low" = list(0, .1), "Low" = list(0, .1))),
    "The rule names must be unique"
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
  expect_error(
    check_metric_weights(list(has_vignette = -2, has_vignette = 0)),
    glue::glue("The metric weights must be a subset of the following: {paste(metric_lst, collapse = ', ')}")
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
  
  expect_warning(
    check_credentials(list(roles = c("admin", "reviewer"), privileges = list(admin = used_privileges, lead = "admin"))),
    "The following role(s) designated under privileges is(are) not present in the 'roles' configuration: lead",
    fixed = TRUE
  )
  
  expect_warning(
    check_credentials(list(roles = c("admin", "lead", "reviewer"), privileges = list(admin = "admin"))),
    "The following privilege(s) is(are) not assigned to any 'role' in the credentials configuration: weight_adjust, auto_decision_adjust, final_decision, revert_decision, add_package, delete_package, overall_comment, general_comment",
    fixed = TRUE
  )
  
  expect_equal(
    check_credentials(list(roles = c("admin", "lead", "reviewer"), privileges = list(admin = used_privileges))),
    NULL
  )
})

test_that("parse_rules works", {
  expect_equal(
    parse_rules(get_db_config("decisions")),
    list()
  )
  
  expect_equal(
    parse_rules(get_db_config("decisions", "example")),
    list(`Severe Risk` = list(0.7, 1L), 
         rule_2 = list(metric = "has_vignettes", 
                       condition = "~ .x == 0", 
                       decision = "Major Risk", 
                       decision_id = 4L, 
                       metric_id = 1L), 
         `Insignificant Risk` = list(0L, 0.1), 
         rule_else = list(decision = "Insignificant Risk", 
                          decision_id = 1L, 
                          metric_id = integer(0)))
  )
})
