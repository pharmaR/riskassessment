test_that("The introJS module works as expected", {
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
  
  # set up new app driver object
  app <- shinytest2::AppDriver$new(app_dir = test_path("test-apps"))
  
  expect_equal(app$get_value(input = "tabs"), "Upload Package")
  
  # note upload_pkgs 
  upload_pkgs <- bind_rows(list(upload_pkg, upload_adm, apptab_admn, apptab_steps))
  
  app$click("upload_package-introJS-help")
  
  # Verify that all elements exist and are visible
  el_pos <-
    purrr::map(upload_pkgs$element, ~ app$get_js(glue::glue('$("{.x}").position()'))[["top"]]) %>%
    unlist()
  expect(length(el_pos) == length(upload_pkgs$element), "One or more Upload Package introJS elements are missing.")
  #expect(all(el_pos != 0), "Not all Upload Package introJS elements are visible.") #last 3 not visible.
  
  tt_txt <- app$get_text(".introjs-tooltiptext")
  expect_equal(tt_txt, upload_pkgs$intro[1])
  
  app$click(selector = ".introjs-nextbutton")
  app$wait_for_idle()
  
  tt_txt <- app$get_text(".introjs-tooltiptext")
  expect_equal(tt_txt, upload_pkgs$intro[2])
  
  app$click(selector = glue::glue("[data-stepnumber='{nrow(upload_pkgs)+1}']"))
  app$wait_for_idle()
  
  tt_txt <- app$get_text(".introjs-tooltiptext")
  expect_equal(tt_txt, sidebar_steps$intro[1])
  
  app$click(selector = ".introjs-prevbutton")
  app$wait_for_idle()
  
  tt_txt <- app$get_text(".introjs-tooltiptext")
  expect_equal(tt_txt, upload_pkgs$intro[nrow(upload_pkgs)])
  
  app$click(selector = glue::glue("[data-stepnumber='{nrow(upload_pkgs)+nrow(sidebar_steps)}']"))
  app$wait_for_idle()
  app$click(selector = ".introjs-donebutton")
  
  app$run_js("Shiny.setInputValue('upload_package-load_cran', 'load')")
  app$wait_for_idle()
  app$set_inputs(`upload_package-pkg_lst` = "tidyr")
  app$click("upload_package-add_pkgs", wait_ = FALSE)
  app$wait_for_value(
    output = "upload_package-upload_pkgs_table",
    ignore = list(NULL),
    timeout = 30 * 1000
  )
  
  upload_pkg_complete <- dplyr::bind_rows(list(upload_pkg, upload_adm, upload_pkg_comp, apptab_admn, apptab_steps))
  
  # Verify that all elements exist and are visible
  el_pos <-
    purrr::map(upload_pkg_complete$element, ~ app$get_js(glue::glue('$("{.x}").position()'))[["top"]]) %>%
    unlist()
  expect(length(el_pos) == length(upload_pkg_complete$element), "One or more Upload Complete introJS elements are missing.")
  #expect(all(el_pos != 0), "Not all Upload Complete introJS elements are visible.")
  
  app$click("upload_package-introJS-help")
  app$click(selector = glue::glue("[data-stepnumber='{nrow(upload_pkg_complete)+1}']"))
  app$wait_for_idle()
  
  # we should be at sidebar_steps #1 here
  tt_txt <- app$get_text(".introjs-tooltiptext")
  expect_equal(tt_txt, sidebar_steps$intro[1])
  
  app$click(selector = ".introjs-skipbutton")
  app$set_inputs(tabs = "Maintenance Metrics")
  app$set_inputs(`sidebar-select_pkg` = "tidyr")
  app$wait_for_idle()
  
  # Verify that all elements exist and are visible
  el_pos <-
    purrr::map(mm_steps$element, ~ app$get_js(glue::glue('$("{.x}").position()'))[["top"]]) %>%
    unlist()
  expect(length(el_pos) == length(mm_steps$element), "One or more Maintenance Metrics introJS elements are missing.")
  expect(all(el_pos != 0), "Not all Maintenance Metrics introJS elements are visible.")
  
  app$click("maintenanceMetrics-introJS-help")
  app$wait_for_idle()
  
  tt_txt <- app$get_text(".introjs-tooltiptext")
  expect_equal(tt_txt, mm_steps$intro[1])
  
  app$click(selector = ".introjs-skipbutton")
  
  app$set_inputs(tabs = "Community Usage Metrics")
  app$wait_for_idle()
  
  # Verify that all elements exist and are visible
  el_pos <-
    purrr::map(cum_steps$element, ~ app$get_js(glue::glue('$("{.x}").position()'))[["top"]]) %>%
    unlist()
  expect(length(el_pos) == length(cum_steps$element), "One or more Community Usage Metrics introJS elements are missing.")
  expect(all(el_pos != 0), "Not all Community Usage Metrics introJS elements are visible.")
  
  app$click("communityMetrics-introJS-help")
  app$wait_for_idle()
  
  tt_txt <- app$get_text(".introjs-tooltiptext")
  expect_equal(tt_txt, cum_steps$intro[1])
  
  app$click(selector = ".introjs-skipbutton")
  app$set_inputs(tabs = "Report Preview")
  app$wait_for_idle()
  
  # Verify that all elements exist and are visible
  el_pos <-
    purrr::map(rp_steps$element, ~ app$get_js(glue::glue('$("{.x}").position()'))[["top"]]) %>%
    unlist()
  expect(length(el_pos) == length(rp_steps$element), "One or more Report Preview introJS elements are missing.")
  expect(all(el_pos != 0), "Not all Report Preview introJS elements are visible.")
  
  app$click("reportPreview-introJS-help")
  app$wait_for_idle()
  
  tt_txt <- app$get_text(".introjs-tooltiptext")
  expect_equal(tt_txt, rp_steps$intro[1])
  
  app$stop()
})