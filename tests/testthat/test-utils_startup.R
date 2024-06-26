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
               c("comments", "community_usage_metrics", "decision_categories", "metric", "package", "package_metrics", "roles", "rules", "sqlite_sequence"))
  pkg <- DBI::dbGetQuery(con, "SELECT * FROM package")
  expect_equal(nrow(pkg), 0)
  expect_equal(names(pkg), c("id", "name", "version", "title", "description", "maintainer", "author", "license", "published_on", 
                             "score", "weighted_score", "decision_id", "decision_by", "decision_date", "date_added"))
  metric <- DBI::dbGetQuery(con, "SELECT * FROM metric")
  expect_equal(nrow(metric), 15) 
  expect_equal(names(metric), c("id", "name", "long_name", "is_perc", "is_url", "is_riskmetric", "description", "class", "weight"))
  #This expectation is to ensure that the internal data element metric_lst is
  #maintaining uniformity with the metric table
  expect_equal(metric_lst, metric[metric$is_riskmetric == 1, ] |> with(purrr::set_names(name, id)))
  pkg_metric <- DBI::dbGetQuery(con, "SELECT * FROM package_metrics")
  expect_equal(nrow(pkg_metric), 0)
  expect_equal(names(pkg_metric), c("id", "package_id", "metric_id", "value", "metric_score", "encode"))
  com_metric <- DBI::dbGetQuery(con, "SELECT * FROM community_usage_metrics")
  expect_equal(nrow(com_metric), 0)
  expect_equal(names(com_metric), c("id", "month", "year", "downloads", "version"))
  comments <- DBI::dbGetQuery(con, "SELECT * FROM comments")
  expect_equal(nrow(comments), 0)
  expect_equal(names(comments), c("id", "user_name", "user_role", "comment", "comment_type", "added_on"))
  decisions <- DBI::dbGetQuery(con, "SELECT * FROM decision_categories")
  expect_equal(nrow(decisions), 0)
  expect_equal(names(decisions), c("id", "decision", "color", "lower_limit", "upper_limit"))
  roles <- DBI::dbGetQuery(con, "SELECT * FROM roles")
  expect_equal(nrow(roles), 0)
  expect_equal(names(roles), c("id", "user_role", used_privileges))
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
  expect_equal(creds$user, c("admin", "lead", "reviewer", "viewer"))
  expect_equal(creds$admin, c('TRUE', 'FALSE', 'FALSE', 'FALSE'))
  expect_equal(creds$expire, c(NA_character_, NA_character_, NA_character_, NA_character_))
  expect_equal(creds$role, c("admin", "lead", "reviewer", "viewer"))
  pwd <- shinymanager::read_db_decrypt(con, name = "pwd_mngt", passphrase = passphrase)
  expect_equal(pwd$must_change, c('FALSE', 'FALSE', 'FALSE', 'FALSE'))
})

#### initialize_raa tests ####

test_that("database initialization", {
  expect_error(initialize_raa(cred_db = "tmp_assess.txt"),
               "cred_db must follow SQLite naming conventions.*")
  expect_error(initialize_raa(assess_db = "tmp_cred.txt"),
               "assess_db must follow SQLite naming conventions.*")
  
  db_lst <- initialize_raa("tmp_assess.sqlite", "tmp_cred.sqlite")
  on.exit(unlink(db_lst))
  expect_true(file.exists(db_lst[1]))
  expect_true(file.exists(db_lst[2]))
})

test_that("check_repo works", {
  
  repos <- c("https://packagemanager.posit.co/cran/latest", "https://cran.rstudio.com")
  expect_equal(
    check_repos(repos),
    repos
  )
  
  repos <- c("https://packagemanager.posit.co/cran/latest", "https://cran.studio.com")
  expect_error(
    check_repos(repos),
    "The following URL was not reachable: https://cran.studio.com/src/contrib. Please check that the repo is valid and pointing to external sources."
  )
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
