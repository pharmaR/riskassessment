test_that("assign_decisions works", {
  
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
    "All decision rule categories should be included in the list of decisions"
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

test_that("get_colors works", {
  expect_equal(
    get_colors(1:3),
    c(`1` = "#06B756FF", `2` = "#A99D04FF", `3` = "#A63E24FF")
  )
  
  expect_equal(
    get_colors(1:20),
    c(`1` = "#06B756FF", `2` = "#2FBC06FF", `3` = "#67BA04FF", `4` = "#81B50AFF", 
      `5` = "#96AB0AFF", `6` = "#A99D04FF", `7` = "#A99D04FF", `8` = "#B78D07FF", 
      `9` = "#BE7900FF", `10` = "#BE6200FF", `11` = "#B24F22FF", `12` = "#A63E24FF", 
      `13` = "#A63E24FF", `14` = "#A63E24FF", `15` = "#A63E24FF", `16` = "#A63E24FF", 
      `17` = "#A63E24FF", `18` = "#A63E24FF", `19` = "#A63E24FF", `20` = "#A63E24FF"
    )
  )
})

test_that("risk_lbl works", {
  expect_equal(
    risk_lbl("Low"),
    "low_attr"
  )
  
  expect_equal(
    risk_lbl("Low Risk"),
    "low_risk_attr"
  )
  
  expect_equal(
    risk_lbl("Low     Risk"),
    "low_risk_attr"
  )
  
  expect_equal(
    risk_lbl("Low Risk", FALSE),
    "low_risk"
  )
})

test_that("process_dec_tbl works", {
  expect_equal(
    process_dec_tbl(app_sys("testdata", "skeleton.sqlite")),
    structure(list(), names = character(0))
  )
  
  expect_equal(
    process_dec_tbl(app_sys("testdata", "decision_automation_ex1.sqlite")),
    list(`Insignificant Risk` = list(0, 0.1), `Severe Risk` = list(0.7, 1))
  )
})
