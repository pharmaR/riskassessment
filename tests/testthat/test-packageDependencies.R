test_that("module can produce a table of package dependencies", {
  
  skip("Check back soon!")
  
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

test_that(
  "Feature 1. module packageDependencies selects all reverse dependencies 
  in the database.
    Scenario 1. Given the selected package is 'dplyr',  
    and the packages names in the package database are 'dplyr' and 'dbplyr', 
    I expect that the package names 'plotly', 'admiral', 'dbplyr' and 'glue' are found in [revdeps],
    and that [revdeps_local] is equal to 'dbplyr'.",
  {
    testargs <- list(
      selected_pkg =  list(
        name = reactiveVal("dplyr")
      ),
      user = "test_user",
      parent = reactiveValues(
        input = reactiveValues(
          tabs = "Package Metrics",
          metric_type = "dep"
        )
      )
    )
    
    test_db_loc <- system.file("testdata", "upload_format.database", 
                               package = "riskassessment")
    temp_db_loc <- withr::local_tempfile(fileext = ".database")
    file.copy(test_db_loc, temp_db_loc)
    
    # because only the dplyr package is in the test dataset, we add one of its 
    # known reverse dependencies:
    withr::with_db_connection(
      list(con = DBI::dbConnect(RSQLite::SQLite(), temp_db_loc)),
      {
        DBI::dbAppendTable(
          con, 
          "package", 
          data.frame(
            name = "dbplyr",
            version = "1.0.0",
            score = "0.32"
          )
        )
      }
    )
    # add test db location to the app session:
    app_session <- MockShinySession$new()
    app_session$options$golem_options <- list(
      assessment_db_name = temp_db_loc
    )
    app_session$userData$loaded2_db <- reactiveVal(dbSelect("SELECT name, version, score FROM package", temp_db_loc))
    
    testServer(packageDependenciesServer, args = testargs,  {
      session$flushReact()
      expect_true(all(c("plotly", "admiral", "dbplyr", "glue") %in% revdeps()))
      expect_equal(revdeps_local(), "dbplyr")
    },
    session = app_session)
  }
)

