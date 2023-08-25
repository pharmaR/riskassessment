test_that("assign_decisions works", {
  expect_true("Placeholder" != "")
})

test_that("set_colors works", {
  expect_equal(
    set_colors(1:3),
    c(`1` = "#06B756", `2` = "#A99D04", `3` = "#A63E24")
  )
  
  expect_equal(
    set_colors(1:20),
    c(`1` = "#06B756", `2` = "#2FBC06", `3` = "#67BA04", `4` = "#81B50A", 
      `5` = "#96AB0A", `6` = "#A99D04", `7` = "#A99D04", `8` = "#B78D07", 
      `9` = "#BE7900", `10` = "#BE6200", `11` = "#B24F22", `12` = "#A63E24", 
      `13` = "#A63E24", `14` = "#A63E24", `15` = "#A63E24", `16` = "#A63E24", 
      `17` = "#A63E24", `18` = "#A63E24", `19` = "#A63E24", `20` = "#A63E24"
    )
  )
})

test_that("risk_lbl works", {
  expect_equal(
    risk_lbl("Low"),
    "cat_low"
  )
  
  expect_equal(
    risk_lbl("Low Risk"),
    "cat_low_risk"
  )
  
  expect_equal(
    risk_lbl("It's Low     Risk"),
    "cat_its_low_risk"
  )
  
  expect_equal(
    risk_lbl("Low Risk", type = "attribute"),
    "cat_low_risk_attr"
  )
  
  expect_equal(
    risk_lbl("Low Risk", type = "module"),
    "cat_low_risk_mod"
  )
})

test_that("process_dec_tbl works", {
  expect_equal(
    process_dec_tbl(),
    list()
  )
  
  expect_equal(
    process_dec_tbl(app_sys("testdata", "skeleton.sqlite")),
    structure(list(), names = character(0))
  )
  
  expect_equal(
    process_dec_tbl(app_sys("testdata", "decision_automation_ex1.sqlite")),
    list(`Insignificant Risk` = c(0, 0.1), `Severe Risk` = c(0.7, 1))
  )
})
