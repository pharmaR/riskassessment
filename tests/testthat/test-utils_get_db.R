test_that("utils_get_db functions other than dbSelect", {
  
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
  
  # valid db? test this is a valid sqlite database
  testthat::expect_equal(readLines(app_db_loc, n =1, warn = FALSE), "SQLite format 3")
  
  # load pkg info for stringr into the database
  pkg_name <- "stringr"
  
  pkg_info <- get_latest_pkg_info(pkg_name)
  
  command <- "INSERT or REPLACE INTO package
        (name, version, title, description, maintainer, author,
        license, published_on, decision_by, decision_date, date_added)
        VALUES({pkg_name}, {pkg_info$Version}, {pkg_info$Title}, {pkg_info$Description},
        {pkg_info$Maintainer}, {pkg_info$Author}, {pkg_info$License}, {pkg_info$Published},
        '', null, {Sys.Date()})"
  
  dbUpdate(command, app_db_loc)
  
  comment <- "this is a pretty good package"
  user_name <- Sys.info()["user"]
  user_role <- "admin"
  abrv <- c('o', 'mm', 'cum')
  
  for(i in seq_along(abrv)) { 
    metric_abrv <- abrv[i]
    command <- "INSERT INTO comments values({pkg_name}, {user_name}, 
        {user_role}, {comment}, {metric_abrv},
        {getTimeStamp()})"
    dbUpdate(command, app_db_loc)
  }

  insert_riskmetric_to_db(pkg_name, app_db_loc)
  insert_community_metrics_to_db(pkg_name, app_db_loc)
  
  pkg_id <- dbSelect("SELECT id FROM package WHERE name = {pkg_name}", app_db_loc)
  
  pkgs_cum_metrics <- generate_comm_data(pkg_name)
  
  pkgs_cum_values <- glue::glue(
    "('{pkg_name}', {pkgs_cum_metrics$month}, {pkgs_cum_metrics$year}, 
  {pkgs_cum_metrics$downloads}, '{pkgs_cum_metrics$version}')") %>%
    glue::glue_collapse(sep = ", ")
  
  test_that("get_overall_comments works", {
    ocmt <- get_overall_comments(pkg_name, db_name = app_db_loc)
    expect_equal(names(ocmt), c("id", "user_name", "user_role", "comment", "comment_type", "added_on"))
    expect_equal(ocmt$id[1], pkg_name)
    expect_equal(ocmt$user_name[1], unname(user_name))
    expect_equal(ocmt$comment[1], comment)
    expect_equal(ocmt$comment_type, "o")
  })
  
  test_that("get_mm_comments works", {
    mcmt <- get_mm_comments(pkg_name, db_name = app_db_loc)
    expect_equal(mcmt$user_name, unname(user_name))
    expect_equal(names(mcmt), c("user_name", "user_role", "comment", "added_on"))
    expect_equal(mcmt$comment, comment)
  })
  
  test_that("get_cm_comments works", {
    ccmt <- get_cm_comments(pkg_name, db_name = app_db_loc)
    expect_equal(ccmt$user_name, unname(user_name))
    expect_equal(names(ccmt), c("user_name", "user_role", "comment", "added_on"))
    expect_equal(ccmt$comment,  comment)
  })
  
  test_that("get_mm_data works", {
    mmdata <- get_mm_data(pkg_id, db_name = app_db_loc)
    expect_s3_class(mmdata, "data.frame")
    expect_equal(names(mmdata), c("name", "is_perc", "is_url", "value", "title", "desc", "succ_icon", "unsucc_icon", "icon_class"))
    expect_equal(mmdata$name[1], "has_vignettes")
  })
  
  test_that("get_comm_data works", {
    cmdata <- get_comm_data(pkg_name, app_db_loc)
    expect_s3_class(cmdata, "data.frame")
    expect_equal(colnames(cmdata), c("id", "month", "year", "downloads", "version"))
    expect_equal(cmdata$id[1], pkg_name) # look at insert_community_metrics_to_db()
  })
  
  test_that("get_pkg_info works", {
    pkg <- get_pkg_info(pkg_name, app_db_loc)
    expect_s3_class(pkg, "data.frame")
    expect_equal(nrow(pkg), 1) 
    expect_equal(names(pkg), c("id", "name", "version", "title", "description", "maintainer", "author", "license", "published_on", 
                               "score", "weighted_score", "decision_id", "decision_by", "decision_date", "date_added", "decision"))
  })
  
  test_that("get_metric_weights works", {
  mtwt <-  get_metric_weights(app_db_loc)
  testthat::expect_equal(mtwt$name[1], "has_vignettes")
  })
  
  test_that("weight_risk_comment works", {
    wcmt <- weight_risk_comment(pkg_name, app_db_loc)
    testthat::expect_s3_class(wcmt, "character")
    testthat::expect_match(wcmt, regexp = "Metric re-weighting has occurred.")
  })

  unlink(app_db_loc)
  rm(app_db_loc, pkg_name, pkg_id, pkg_info, command, comment, user_name, user_role, abrv)
  
})
  