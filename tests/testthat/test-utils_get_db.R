test_that("utils_get_db functions other than dbSelect", {
  
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
  
  # valid db? test this is a valid sqlite database
  testthat::expect_equal(readLines(file.path(base_path, db_temp), n =1, warn = FALSE), "SQLite format 3")
  
  # load pkg info for stringr into the database
  pkg_name <- "stringr"
  
  pkg_info <- get_latest_pkg_info(pkg_name)
  
  command <- glue::glue(
    "INSERT or REPLACE INTO package
        (name, version, title, description, maintainer, author,
        license, published_on, decision, date_added)
        VALUES('{pkg_name}', '{pkg_info$Version}', '{pkg_info$Title}', '{pkg_info$Description}',
        '{pkg_info$Maintainer}', '{pkg_info$Author}', '{pkg_info$License}', '{pkg_info$Published}',
        '', '{Sys.Date()}')")
  
  dbUpdate(command, file.path(base_path, db_temp))
  
  comment <- "this is a pretty good package"
  user_name <- Sys.info()["user"]
  user_role <- "admin"
  abrv <- c('o', 'mm', 'cum')
  
  for(i in seq_along(abrv)) { 
    metric_abrv <- abrv[i]
    command <- glue::glue(
      "INSERT INTO comments values('{pkg_name}', '{user_name}', 
        '{user_role}', '{comment}', '{metric_abrv}',
        '{getTimeStamp()}')")
    dbUpdate(command, file.path(base_path, db_temp))
  }

  insert_riskmetric_to_db(pkg_name, file.path(base_path, db_temp))
  pkg_id <- dbSelect(glue::glue("SELECT id FROM package WHERE name = '{pkg_name}'"), file.path(base_path, db_temp))
  
  insert_community_metrics_to_db(pkg_name, file.path(base_path, db_temp))
  
  test_that("get_overall_comments works", {
    ocmt <- get_overall_comments(pkg_name, db_name = file.path(base_path, db_temp))
    expect_equal(names(ocmt), c("id", "user_name", "user_role", "comment", "comment_type", "added_on"))
    expect_equal(ocmt$id[1], pkg_name)
    expect_equal(ocmt$user_name[1], unname(user_name))
    expect_equal(ocmt$comment[1], comment)
    expect_equal(ocmt$comment_type, "o")
  })
  
  test_that("get_mm_comments works", {
    mcmt <- get_mm_comments(pkg_name, db_name = file.path(base_path, db_temp))
    expect_equal(mcmt$user_name, unname(user_name))
    expect_equal(names(mcmt), c("user_name", "user_role", "comment", "added_on"))
    expect_equal(mcmt$comment, comment)
  })
  
  test_that("get_cm_comments works", {
    ccmt <- get_cm_comments(pkg_name, db_name = file.path(base_path, db_temp))
    expect_equal(ccmt$user_name, unname(user_name))
    expect_equal(names(ccmt), c("user_name", "user_role", "comment", "added_on"))
    expect_equal(ccmt$comment,  comment)
  })
  
  test_that("get_mm_data works", {
    mmdata <- get_mm_data(pkg_id, file.path(base_path, db_temp))
    expect_s3_class(mmdata, "data.frame")
    expect_equal(names(mmdata), c("name", "is_perc", "is_url", "value", "title", "desc", "succ_icon", "unsucc_icon", "icon_class"))
    expect_equal(mmdata$name[1], "has_vignettes")
  })
  
  test_that("get_comm_data works", {
    cmdata <- get_comm_data(pkg_name, file.path(base_path, db_temp))
    expect_s3_class(cmdata, "data.frame")
    expect_equal(colnames(cmdata), c("id", "month", "year", "downloads", "version"))
    expect_equal(cmdata$id[1], pkg_name)
  })
  
  test_that("get_pkg_info works", {
    pkg <- get_pkg_info(pkg_name, file.path(base_path, db_temp))
    expect_s3_class(pkg, "data.frame")
    expect_equal(nrow(pkg), 1) 
    expect_equal(names(pkg), c("id", "name", "version", "title", "description", "maintainer", "author", "license", "published_on", "score", "weighted_score", "decision", "decision_by", "decision_date", "date_added"))
  })
  
  test_that("get_metric_weights works", {
  mtwt <-  get_metric_weights(file.path(base_path, db_temp))
  testthat::expect_equal(mtwt$name[1], "has_vignettes")
  })
  
  test_that("weight_risk_comment works", {
    wcmt <- weight_risk_comment(pkg_name, file.path(base_path, db_temp))
    testthat::expect_s3_class(wcmt, "character")
    testthat::expect_match(wcmt, regexp = "Metric re-weighting has occurred.")
  })

  unlink(file.path(base_path, db_temp))
  rm(base_path, db_temp, pkg_name, pkg_id, pkg_info, command, comment, user_name, user_role, abrv)
  
})
  