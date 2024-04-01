test_that("upload_pkg_lst works", {
  
  assess_db <- "tmp_assess.sqlite"
  on.exit(unlink(assess_db))
  if (file.exists(assess_db))
    file.remove(assess_db)
  
  test_db <- app_sys("testdata", "skeleton.sqlite")
  file.copy(
    test_db,
    assess_db
  )

  expect_warning(out <- upload_pkg_lst("dplyrr", "tmp_assess.sqlite"),
                 "not found on CRAN") |>
    suppressWarnings()
  expect_equal(
    out,
    data.frame(package = "dplyrr", version = "0.0.0", status = "not found", 
               score = NA_real_),
    ignore_attr = TRUE
  )
  
  options("shiny.testmode" = TRUE)
  on.exit(options("shiny.testmode" = NULL))
  
  expect_warning(out <- upload_pkg_lst("dplyr", "tmp_assess.sqlite"),
                 "No value supplied for `repos`")
  expect_equal(
    out,
    data.frame(package = "dplyr", version = "1.1.2", status = "new", 
               score = 0.27),
    ignore_attr = TRUE
  )
  
  expect_no_warning(out <- upload_pkg_lst(c("purrr", "dplyr"), "tmp_assess.sqlite", "https://cran.rstudio.com"))
  expect_equal(
    out,
    data.frame(package = c("purrr", "dplyr"), version = c("1.0.1", "1.1.2"), 
               status = c("new", "duplicate"), score = c(0.27, NA)),
    ignore_attr = TRUE
  )
  
  expect_equal(
    dbSelect("SELECT * FROM package", "tmp_assess.sqlite"),
    data.frame(
      id = 1:2,
      name = c("dplyr", "purrr"),
      version = c("1.1.2", "1.0.1"),
      title = c(
        "dplyr: A Grammar of Data Manipulation",
        "purrr: Functional Programming Tools"
      ),
      description = c(
        "A fast, consistent tool for working with data frame like   objects, both in memory and out of memory.",
        "A complete and consistent functional programming toolkit for   R."
      ),
      maintainer = c(
        "Hadley Wickham  <hadley at posit.co>",
        "Hadley Wickham  <hadley at rstudio.com>"
      ),
      author = c(
        "Hadley Wickham    [aut, cre], Romain François    [aut], Lionel Henry [aut], Kirill Müller    [aut], Davis Vaughan    [aut], Posit Software, PBC [cph, fnd]",
        "Hadley Wickham    [aut, cre], Lionel Henry [aut], RStudio [cph, fnd]"
      ),
      license = c("MIT + file LICENSE", "MIT + file LICENSE"),
      published_on = c("2023-04-20",
                       "2023-01-10"),
      score = c(0.27, 0.27),
      weighted_score = c(NA_real_,
                         NA_real_),
      decision_id = c(NA_integer_, NA_integer_),
      decision_by = c("",
                      ""),
      decision_date = c(NA_real_, NA_real_),
      date_added = c("2023-07-20",
                     "2023-07-20")
    ),
    ignore_attr = TRUE
  )
})