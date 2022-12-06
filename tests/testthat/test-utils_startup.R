#### create_db tests ####

test_that("invalid arguments", {
  expect_error(create_db())
  expect_error(create_db(1))
  expect_error(create_db("tmp"))
  expect_error(create_db(c("tmp.sqlite", "tmp2.sqlite")))
  expect_error(create_db(), "db_name must follow SQLite naming conventions.*")
})

test_that("database creation", {
  db <- create_db("tmp.sqlite")
  
  expect_equal(db, "tmp.sqlite")
  
  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  on.exit({
    DBI::dbDisconnect(con)
    unlink(db)
    })
  
  expect_equal(DBI::dbListTables(con),
               c("comments", "community_usage_metrics", "metric", "package", "package_metrics", "sqlite_sequence"))
  pkg <- DBI::dbGetQuery(con, "SELECT * FROM package")
  expect_equal(nrow(pkg), 0)
  expect_equal(names(pkg), c("id", "name", "version", "title", "description", "maintainer", "author", "license", "published_on", "score", "weighted_score", "decision", "date_added"))
  metric <- DBI::dbGetQuery(con, "SELECT * FROM metric")
  expect_equal(nrow(metric), 12)
  expect_equal(names(metric), c("id", "name", "long_name", "is_url", "is_perc", "description", "class", "weight"))
  pkg_metric <- DBI::dbGetQuery(con, "SELECT * FROM package_metrics")
  expect_equal(nrow(pkg_metric), 0)
  expect_equal(names(pkg_metric), c("id", "package_id", "metric_id", "value", "weight"))
  com_metric <- DBI::dbGetQuery(con, "SELECT * FROM community_usage_metrics")
  expect_equal(nrow(com_metric), 0)
  expect_equal(names(com_metric), c("id", "month", "year", "downloads", "version"))
  comments <- DBI::dbGetQuery(con, "SELECT * FROM comments")
  expect_equal(nrow(comments), 0)
  expect_equal(names(comments), c("id", "user_name", "user_role", "comment", "comment_type", "added_on"))
})

#### create_credentials_db ####

test_that("invalid arguments", {
  expect_error(create_credentials_db())
  expect_error(create_credentials_db(1))
  expect_error(create_credentials_db("tmp"))
  expect_error(create_credentials_db(c("tmp.sqlite", "tmp2.sqlite")))
  expect_error(create_credentials_db(), "db_name must follow SQLite naming conventions.*")
})

test_that("database creation", {
  db <- create_credentials_db("tmp.sqlite")
  
  expect_equal(db, "tmp.sqlite")
  
  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  on.exit({
    DBI::dbDisconnect(con)
    unlink(db)
  })
  
  expect_equal(DBI::dbListTables(con),
               c("credentials", "logs", "pwd_mngt"))
  creds <- shinymanager::read_db_decrypt(con, name = "credentials", passphrase = passphrase)
  expect_equal(creds$user, "ADMIN")
  expect_equal(creds$admin, 'TRUE')
  expect_equal(creds$expire, as.character(Sys.Date() + 365))
  pwd <- shinymanager::read_db_decrypt(con, name = "pwd_mngt", passphrase = passphrase)
  expect_equal(pwd$must_change, 'TRUE')
})
