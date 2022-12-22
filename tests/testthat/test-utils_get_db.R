test_that("utils_get_db functions other than dbSelect", {
  
  base_path <- app_sys("testdata")
  db_temp <- "datatest.sqlite"
  # create temporary database in inst/testdata
  create_db(file.path(base_path, db_temp))
  
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

})