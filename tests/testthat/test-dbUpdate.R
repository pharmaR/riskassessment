test_that("both dbUpdate and dbSelect work", {
  app_db_loc <- test_path("test-apps", "database.sqlite")
  if (file.exists(app_db_loc)) {
    file.remove(app_db_loc)
  }
  
  # copy in already instantiated database to avoid need to rebuild
  # this is a database that has been built via inst/testdata/upload_format.csv
  test_db_loc <- system.file("testdata", "skeleton.sqlite", package = "riskassessment")
  file.copy(
    test_db_loc,
    app_db_loc
  )
  
  con <- dbConnect(RSQLite::SQLite(), app_db_loc)
  
  # 1. table 'package' exists?
  testthat::expect_true(DBI::dbExistsTable(con, "package"))
  
  # executing each query
  #col_list <- purrr::map(.x = tbls, .f =DBI::dbListFields, conn = con)
  cols <- DBI::dbListFields(con, "package")
  
  # 2. description is the 5th column in table 'package'
  testthat::expect_equal(5, which(cols == "description"))
  
  DBI::dbDisconnect(con)
  
  query <- "select * FROM [package] limit 1"
  tbl1 <- dbSelect(query, app_db_loc)
  
  # 4. expect zero rows have been returned.
  testthat::expect_equal(nrow(tbl1), 0L)
  
  pkg_name <- "stringr"
  
  command <- glue::glue("DELETE from [package] WHERE ( name = '{pkg_name}')")
  
  # 5. expect zero rows were affected, referring to existing table 
  testthat::expect_message(dbUpdate(command, app_db_loc), regexp = "zero rows were affected by the command:" )
  
  command <- glue::glue("DELETE from [thispkg] WHERE (name = '{pkg_name}')")
  # 6. expect message about "no such table"
  testthat::expect_message(dbUpdate(command, app_db_loc), regexp = "Error: no such table:" )
  
  pkg_info <- get_latest_pkg_info(pkg_name)
  
  command <-(glue::glue(
    "INSERT or REPLACE INTO package
        (name, version, title, description, maintainer, author,
        license, published_on, decision, decision_by, decision_date, date_added)
        VALUES('{pkg_name}', '{pkg_info$Version}', '{pkg_info$Title}', '{pkg_info$Description}',
        '{pkg_info$Maintainer}', '{pkg_info$Author}', '{pkg_info$License}', '{pkg_info$Published}',
        '', '', null, '{Sys.Date()}')"))
  
  dbUpdate(command, app_db_loc)
  
  tbl1 <- dbSelect(query, app_db_loc)
  
  # 7. expect one row has been returned.
  testthat::expect_equal(nrow(tbl1), 1L)
  
  # 8. query result matches what we wrote into the table
  testthat::expect_equal(pkg_info$Title, tbl1$title)
  
  # clean up after ourselves
  command <- glue::glue("DELETE from [package] WHERE ( name = '{pkg_name}')")
  dbUpdate(command, app_db_loc)
  
  tbl1 <- dbSelect(query, app_db_loc)
  
  # 9. expect zero rows have been returned.
  testthat::expect_equal(nrow(tbl1), 0L)
  
  unlink(app_db_loc)
  rm(app_db_loc, con, query, command, tbl1, cols, pkg_name, pkg_info)
})

