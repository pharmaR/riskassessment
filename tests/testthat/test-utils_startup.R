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
               c("comments", "community_usage_metrics", "decision_categories", "metric", "package", "package_metrics", "sqlite_sequence"))
  pkg <- DBI::dbGetQuery(con, "SELECT * FROM package")
  expect_equal(nrow(pkg), 0)
  expect_equal(names(pkg), c("id", "name", "version", "title", "description", "maintainer", "author", "license", "published_on", 
                             "score", "weighted_score", "decision_id", "decision_by", "decision_date", "date_added"))
  metric <- DBI::dbGetQuery(con, "SELECT * FROM metric")
  expect_equal(nrow(metric), 12)
  expect_equal(names(metric), c("id", "name", "long_name", "is_url", "is_perc", "description", "class", "weight"))
  pkg_metric <- DBI::dbGetQuery(con, "SELECT * FROM package_metrics")
  expect_equal(nrow(pkg_metric), 0)
  expect_equal(names(pkg_metric), c("id", "package_id", "metric_id", "value", "encode"))
  com_metric <- DBI::dbGetQuery(con, "SELECT * FROM community_usage_metrics")
  expect_equal(nrow(com_metric), 0)
  expect_equal(names(com_metric), c("id", "month", "year", "downloads", "version"))
  comments <- DBI::dbGetQuery(con, "SELECT * FROM comments")
  expect_equal(nrow(comments), 0)
  expect_equal(names(comments), c("id", "user_name", "user_role", "comment", "comment_type", "added_on"))
  decisions <- DBI::dbGetQuery(con, "SELECT * FROM decision_categories")
  expect_equal(nrow(decisions), 0)
  expect_equal(names(decisions), c("id", "decision", "color", "lower_limit", "upper_limit"))
})

#### create_credentials_db  tests ####

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

#### create_credentials_dev_db tests ####

test_that("invalid arguments", {
  expect_error(create_credentials_dev_db())
  expect_error(create_credentials_dev_db(1))
  expect_error(create_credentials_dev_db("tmp"))
  expect_error(create_credentials_dev_db(c("tmp.sqlite", "tmp2.sqlite")))
  expect_error(create_credentials_dev_db(), "db_name must follow SQLite naming conventions.*")
})

test_that("database creation", {
  db <- create_credentials_dev_db("tmp.sqlite")
  
  expect_equal(db, "tmp.sqlite")
  
  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  on.exit({
    DBI::dbDisconnect(con)
    unlink(db)
  })
  
  expect_equal(DBI::dbListTables(con),
               c("credentials", "logs", "pwd_mngt"))
  creds <- shinymanager::read_db_decrypt(con, name = "credentials", passphrase = passphrase)
  expect_equal(creds$user, c("admin", "lead", "reviewer"))
  expect_equal(creds$admin, c('TRUE', 'FALSE', 'FALSE'))
  expect_equal(creds$expire, c(NA_character_, NA_character_, NA_character_))
  expect_equal(creds$role, c("admin", "lead", "reviewer"))
  pwd <- shinymanager::read_db_decrypt(con, name = "pwd_mngt", passphrase = passphrase)
  expect_equal(pwd$must_change, c('FALSE', 'FALSE', 'FALSE'))
})

#### initialize_raa tests ####

test_that("database initialization", {
  expect_error(initialize_raa())
  expect_error(initialize_raa(assess_db = "tmp_assess.sqlite"),
               "cred_db must follow SQLite naming conventions.*")
  expect_error(initialize_raa(cred_db = "tmp_cred.sqlite"),
               "assess_db must follow SQLite naming conventions.*")
  
  db_lst <- initialize_raa("tmp_assess.sqlite", "tmp_cred.sqlite", c("Low Risk", "Medium Risk", "High Risk"))
  on.exit(unlink(db_lst))
  expect_true(file.exists(db_lst[1]))
  expect_true(file.exists(db_lst[2]))
})

#### add_tags tests ####

test_that("add_tags works", {
  at <- add_tags(fluidPage())
  
  expect_type(at, "closure")
})

#### app_theme tests ####

test_that("app_theme runs", {
  app_thm <- app_theme()
  expect_equal(app_thm, 
               bslib::bs_theme(
                 bootswatch = "lux",
                 version = 5,
                 primary = "#24305E",
                 secondary = "#F76C6C",
               ))
})
