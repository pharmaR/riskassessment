test_that("module can produce a table of package dependencies", {
  
  app_db_loc <- test_path("test-apps", "database.sqlite")
  if (file.exists(app_db_loc)) {
    file.remove(app_db_loc)
  }
  
  # copy in already instantiated database to avoid need to rebuild
  # this is a database that has been built via inst/testdata/upload_format.csv
  test_db_loc <- system.file("testdata", "upload_format.database", package = "riskassessment")
  file.copy(
    test_db_loc,
    app_db_loc
  )

  # set up new app driver object
  app <- shinytest2::AppDriver$new(app_dir = test_path("test-apps"), load_timeout = 1E6)

  app$set_inputs(`sidebar-select_pkg` = "dplyr")
  
  app$set_inputs(tabs = "Package Dependencies", wait_ = FALSE)
  app$wait_for_idle(timeout = 1E5)
  
  out_htm <- app$get_value(output = "packageDependencies-package_dependencies_ui")$html
  
  status_txt <- rvest::read_html(out_htm) %>% rvest::html_text()
  actual <- trimws(stringr::word(status_txt, sep = "\n", 3))
  expected <- "First-order dependencies for package: dplyr"
  expect_equal(actual, expected)
  
  # pull the datatable id out of the html and read it
  id_strng <- stringr::str_extract(out_htm, pattern = "(?<=id\\=).?(\\w+\\-?\\w+)") %>% substr(2,nchar(.))
  json <- app$get_value(output = id_strng) %>% rjson::fromJSON()
  actual <- json$x$data[[3]] %>% trimws() 
  expected <- c("generics", "glue", "lifecycle", "magrittr", "R6", "rlang", "tibble", "tidyselect", "vctrs", "pillar")
  
  expect_equal(sort(actual), sort(expected))
  
  unlink("app_db_loc")
  rm(out_htm, id_strng, json, actual, expected, app_db_loc)

})