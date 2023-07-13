test_that("utils_insert_db functions other than dbUpdate", {
  
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
  pkg_info <- test_pkg_info[[pkg_name]]
  
  insert_pkg_info_to_db(pkg_name, pkg_info$Version, db_name = app_db_loc)

  test_that("insert_pkg_info_to_db works", {
    pkg <- dbSelect(
      "SELECT *
     FROM package
     WHERE name = {pkg_name}", app_db_loc
    )
    expect_s3_class(pkg, "data.frame")
    expect_equal(nrow(pkg), 1) 
    expect_equal(names(pkg), c("id", "name", "version", "title", "description", "maintainer", "author", "license", "published_on", 
                               "score", "weighted_score", "decision_id", "decision_by", "decision_date", "date_added"))
  })
  
  insert_riskmetric_to_db(pkg_name, app_db_loc)
  
  pkg_id <- dbSelect("SELECT id FROM package WHERE name = {pkg_name}", app_db_loc)
  
  test_that("insert_riskmetric_to_db", {
    mmdata <-   dbSelect(
      "SELECT metric.name, metric.long_name, metric.description, metric.is_perc,
                    metric.is_url, package_metrics.value
                    FROM metric
                    INNER JOIN package_metrics ON metric.id = package_metrics.id
                    WHERE package_metrics.package_id = {pkg_id} AND 
                    metric.class = 'maintenance' ;", app_db_loc)
    expect_s3_class(mmdata, "data.frame")
    expect_equal(names(mmdata), c("name", "long_name", "description", "is_perc", "is_url", "value"))
    expect_equal(mmdata$name[1], "has_vignettes")
  })
  
  insert_community_metrics_to_db(pkg_name, app_db_loc)
  
  test_that("insert_community_metrics_to_db works", {
    cmdata <- dbSelect(
      "SELECT *
     FROM community_usage_metrics
     WHERE id = {pkg_name}", app_db_loc
    )
    expect_s3_class(cmdata, "data.frame")
    expect_equal(colnames(cmdata), c("id", "month", "year", "downloads", "version"))
    expect_equal(cmdata$id[1], pkg_name)
  })
  
  update_metric_weight(metric_name = 'has_vignettes', metric_weight = 2, app_db_loc)
  
  test_that("update_metric_weight works", {
    mtwt <-  dbSelect(
      "SELECT name, weight 
     FROM metric where name = 'has_vignettes'", db_name = app_db_loc
    )
    testthat::expect_equal(mtwt$weight, 2)
  })
  
  old_package <- dbSelect("SELECT * FROM package", app_db_loc)
  rescore_package(pkg_name, app_db_loc)
  
  test_that("rescore_package works", {
    new_package <- dbSelect("SELECT * FROM package", app_db_loc)
    testthat::expect(old_package$score != new_package$score, "Updated risk score equals old risk score")
    testthat::expect_equal(dplyr::select(new_package, - score), dplyr::select(old_package, - score))
  })
  
  test_that("db_trash_collection works", {
    cmdata1 <- dbSelect(
      "SELECT *
     FROM community_usage_metrics
     WHERE id = {pkg_name}", app_db_loc
    )
    dbUpdate("delete from package where name = {pkg_name}", db_name = app_db_loc)
    db_trash_collection(db_name = app_db_loc)
    cmdata2 <- dbSelect(
      "SELECT *
     FROM community_usage_metrics
     WHERE id = {pkg_name}", app_db_loc
    )
    testthat::expect_true(nrow(cmdata2) == 0)
    testthat::expect_lt(nrow(cmdata2), nrow(cmdata1))
  })
  
  unlink(app_db_loc)
  rm(app_db_loc, pkg_name, pkg_id)
  
})
