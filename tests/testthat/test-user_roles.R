test_that("Privileges table matchs used privileges",{
  expect_equal(
    privileges_tbl[["Privilege"]],
    used_privileges
  )
})
