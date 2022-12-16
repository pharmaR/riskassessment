test_that("both dbUpdate and dbSelect work", {

  base_path <- app_sys("testdata")
  # this is a copy of the empty database.sqlite db which is in ./inst/testdata
  db_name <- "skeleton.sqlite"
  # and this is a temporary datbase
  db_temp <- "datatest.sqlite"
  
  confr <- dbConnect(RSQLite::SQLite(), file.path(base_path, db_name))
  conto <- dbConnect(RSQLite::SQLite(), file.path(base_path, db_temp))
  
  # copy into temp db
  RSQLite::sqliteCopyDatabase(confr, conto)
  
  DBI::dbDisconnect(confr)
  DBI::dbDisconnect(conto)
  
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(base_path, db_temp))
  
  # 1. table 'package' exists?
  testthat::expect_true(DBI::dbExistsTable(con, "package"))
  
  # executing each query
  #col_list <- purrr::map(.x = tbls, .f =DBI::dbListFields, conn = con)
  cols <- DBI::dbListFields(con, "package")
  
  # 2. description is the 5th column in table 'package'
  testthat::expect_equal(5, which(cols == "description"))
  
  DBI::dbDisconnect(con)
  
  query <- "select * FROM [package] limit 1"
  tbl1 <- dbSelect(query, file.path(base_path, db_temp))
  
  # 4. expect zero rows have been returned.
  testthat::expect_equal(nrow(tbl1), 0L)
  
  pkg_name <- "stringr"
  
  command <- glue::glue("DELETE from [package] WHERE ( name = '{pkg_name}')")

  # 5. expect zero rows were affected, referring to existing table 
  testthat::expect_message(dbUpdate(command, file.path(base_path, db_temp)), regexp = "zero rows were affected by the command:" )
  
  command <- glue::glue("DELETE from [thispkg] WHERE (name = '{pkg_name}')")
  # 6. expect message about "no such table"
  testthat::expect_message(dbUpdate(command, file.path(base_path, db_temp)), regexp = "Error: no such table:" )
  
  pkg_info <- get_latest_pkg_info(pkg_name)
  
  command <-(glue::glue(
    "INSERT or REPLACE INTO package
        (name, version, title, description, maintainer, author,
        license, published_on, decision, date_added)
        VALUES('{pkg_name}', '{pkg_info$Version}', '{pkg_info$Title}', '{pkg_info$Description}',
        '{pkg_info$Maintainer}', '{pkg_info$Author}', '{pkg_info$License}', '{pkg_info$Published}',
        '', '{Sys.Date()}')"))
  
  dbUpdate(command, file.path(base_path, db_temp))
  
  tbl1 <- dbSelect(query, file.path(base_path, db_temp))
  
  # 7. expect one row has been returned.
  testthat::expect_equal(nrow(tbl1), 1L)
  
  # 8. query result matches what we wrote into the table
  testthat::expect_equal(pkg_info$Title, tbl1$title)
  
  # clean up after ourselves
  command <- glue::glue("DELETE from [package] WHERE ( name = '{pkg_name}')")
  dbUpdate(command, file.path(base_path, db_temp))
  
  tbl1 <- dbSelect(query, file.path(base_path, db_temp))
  
  # 9. expect zero rows have been returned.
  testthat::expect_equal(nrow(tbl1), 0L)
  
  unlink(file.path(base_path, db_temp))
  rm(base_path, db_name, db_temp, confr, conto, con, query, command, tbl1, cols, pkg_name, pkg_info)
  
})

