test_that("database has been defined and dbSelect function works", {
  db_name = golem::get_golem_options('assessment_db_name')
  if(is.null(db_name)) db_name <- "database.sqlite"
  
  # 1. file exists?
  testthat::expect_true(file.exists(db_name))
  
  # 2. valid db? test this is a valid sqlite database
  testthat::expect_equal(readLines(file.path("../..",db_name), n =1, warn = FALSE), "SQLite format 3")  
  
  # 3. can we connect?
  testthat::expect_true(DBI::dbCanConnect(RSQLite::SQLite(), db_name))

  query <- "SELECT [name] FROM sqlite_master WHERE type='table' order by [rootpage]"
  tbls <- dbSelect(query, file.path("../..",db_name))
  
  # 4. test s3 class is data.frame
  testthat::expect_s3_class(tbls, "data.frame")
  
  tbl_names <- c("package", "sqlite_sequence", "metric", "package_metrics", "community_usage_metrics", "comments")
  # 5. test all tables have been created
  testthat::expect_equal(tbls |> dplyr::pull(), tbl_names)
  
  query <- "select * FROM [metric] WHERE [class] = 'maintenance' order by [id]"
  metric <- dbSelect(query, file.path("../..",db_name))
  
  # 6. test that we have at least 10 maintenance metrics
  testthat::expect_gte(nrow(metric), 10)

  rm(query, tbl_names, metric)
  
})

