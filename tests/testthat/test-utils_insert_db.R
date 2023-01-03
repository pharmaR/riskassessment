test_that("utils_insert_db functions other than dbUpdate", {
  
  # insert_pkg_info_to_db
  # insert_maintenance_metrics_to_db
  # insert_community_metrics_to_db
  # update_metric_weight
  
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
  
  insert_pkg_info_to_db(pkg_name, file.path(base_path, db_temp))

  test_that("get_pkg_info works", {
    pkg <- get_pkg_info(pkg_name, file.path(base_path, db_temp))
    expect_s3_class(pkg, "data.frame")
    expect_equal(nrow(pkg), 1) 
    expect_equal(names(pkg), c("id", "name", "version", "title", "description", "maintainer", "author", "license", "published_on", "score", "weighted_score", "decision", "date_added"))
  })
  
  insert_maintenance_metrics_to_db(pkg_name, file.path(base_path, db_temp))
  
  pkg_id <- dbSelect(glue::glue("SELECT id FROM package WHERE name = '{pkg_name}'"), file.path(base_path, db_temp))

  test_that("get_mm_data works", {
    mmdata <- get_mm_data(pkg_id, file.path(base_path, db_temp))
    expect_s3_class(mmdata, "data.frame")
    expect_equal(names(mmdata), c("name", "is_perc", "is_url", "value", "title", "desc", "succ_icon", "unsucc_icon", "icon_class"))
    expect_equal(mmdata$name[1], "has_vignettes")
  })

  insert_community_metrics_to_db(pkg_name, file.path(base_path, db_temp))
  
  test_that("get_comm_data works", {
    cmdata <- get_comm_data(pkg_name, file.path(base_path, db_temp))
    expect_s3_class(cmdata, "data.frame")
    expect_equal(colnames(cmdata), c("id", "month", "year", "downloads", "version"))
    expect_equal(cmdata$id[1], pkg_name)
  })
  
  update_metric_weight(metric_name = 'has_vignettes', metric_weight = 2, file.path(base_path, db_temp))
                                   
  test_that("update_metric_weight works", {
  mtwt <-  dbSelect(
      "SELECT name, weight 
     FROM metric where name = 'has_vignettes'", db_name = file.path(base_path, db_temp)
    )
  testthat::expect_equal(mtwt$weight, 2)
  })
  
  unlink(file.path(base_path, db_temp))
  rm(base_path, db_temp, pkg_name, pkg_id, pkg_info, command, comment, user_name, user_role, abrv)
  
})
  