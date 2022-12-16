test_that("database has been defined and dbSelect function works", {

  # this is a copy of the empty database.sqlite db which is in ./inst/testdata
  db_name <- "skeleton.sqlite"

  base_path <- app_sys("testdata")
  
  # 1. file exists?
  testthat::expect_true(file.exists(file.path(base_path, db_name)))
  
  # 2. valid db? test this is a valid sqlite database
  testthat::expect_equal(readLines(file.path(base_path, db_name), n =1, warn = FALSE), "SQLite format 3")  

  # 3. can we connect?
  testthat::expect_true(DBI::dbCanConnect(RSQLite::SQLite(), file.path(base_path, db_name)))

  query <- "SELECT [name] FROM sqlite_master WHERE type='table' order by [rootpage]"
  tbls <- dbSelect(query, file.path(base_path, db_name))

  # 4. test s3 class is data.frame
  testthat::expect_s3_class(tbls, "data.frame")

  tbl_names <- c("package", "sqlite_sequence", "metric", "package_metrics", "community_usage_metrics", "comments")
  # 5. test all tables have been created
  testthat::expect_equal(tbls |> dplyr::pull(), tbl_names)

  query <- "select * FROM [metric] WHERE [class] = 'maintenance' order by [id]"
  metric <- dbSelect(query, file.path(base_path, db_name))

  # 6. test that we have at least 10 maintenance metrics
  testthat::expect_gte(nrow(metric), 10)

  query <- "select name FROM [thispackage] limit 1"
  
  # 7. expect message about "no such table"
  testthat::expect_message(dbSelect(query, file.path(base_path, db_name)), regexp = "Error: no such table:")
  
  # NULL is returned when Trycatch is invoked
  ret <- suppressMessages(dbSelect(query, file.path(base_path, db_name)))
  # 8. test NULL is returned
  testthat::expect_null(ret)
  
  
  rm(db_name, base_path, query, tbls, tbl_names, metric)
  
})

