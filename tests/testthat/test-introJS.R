test_that("The introJS module works as expected for admins", {
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
  
  getBoundingClientRect <- function(appDriver, el) {
    appDriver$get_js(glue::glue('const rect = $("{el}")[0].getBoundingClientRect(); [rect.left, rect.top, rect.bottom, rect.right]')) %>% purrr::possibly(purrr::set_names, otherwise = .)(c("left", "top", "bottom", "right"))
  }
  
  # set up new app driver object
  app <- shinytest2::AppDriver$new(app_dir = test_path("test-apps"),
                                   load_timeout = 600 * 1000)
  app$set_window_size(width = 1619, height = 1057)
  app$wait_for_idle()
  
  expect_equal(app$get_value(input = "tabs"), "Upload Package")
  
  # note upload_pkgs 
  upload_pkgs <- bind_rows(list(upload_pkg, upload_adm, apptab_admn, apptab_steps, sidebar_steps))
  
  app$click("upload_package-introJS-help")
  app$wait_for_idle()
  
  # Verify that all elements exist and are visible
  el_pos <- purrr::map(upload_pkgs$element, getBoundingClientRect, appDriver = app)
  introjs_bullets <- app$get_js("$('.introjs-bullets ul li').length")
  expect(length(el_pos) == introjs_bullets, "One or more Upload Package introJS elements are missing.")
  expect(all(purrr::map_lgl(el_pos, ~ any(.x > 0))), "One or more elements are not visible")
  steps <- app$get_value(export = "upload_package-introJS-steps")
  expect_equal(upload_pkgs, steps)

  app$click(selector = ".introjs-skipbutton")
  
  app$run_js("Shiny.setInputValue('upload_package-load_cran', 'load')")
  app$wait_for_idle()
  app$set_inputs(`upload_package-pkg_lst` = "tidyr")
  app$click("upload_package-add_pkgs", wait_ = FALSE)
  app$wait_for_value(
    output = "upload_package-upload_pkgs_table",
    ignore = list(NULL),
    timeout = 30 * 1000
  )
  
  app$click("upload_package-introJS-help")
  app$wait_for_idle()
  
  upload_pkg_complete <- dplyr::bind_rows(list(upload_pkg, upload_adm, upload_pkg_comp, apptab_admn, apptab_steps, sidebar_steps))
  
  # Verify that all elements exist and are visible
  el_pos <- purrr::map(upload_pkg_complete$element, getBoundingClientRect, appDriver = app)
  introjs_bullets <- app$get_js("$('.introjs-bullets ul li').length")
  expect(length(el_pos) == introjs_bullets, "One or more Upload Package introJS elements are missing.")
  expect(all(purrr::map_lgl(el_pos, ~ any(.x > 0))), "One or more elements are not visible")
  steps <- app$get_value(export = "upload_package-introJS-steps")
  expect_equal(upload_pkg_complete, steps)
  
  app$click(selector = ".introjs-skipbutton")
  app$set_inputs(tabs = "Maintenance Metrics")
  app$set_inputs(`sidebar-select_pkg` = "tidyr")
  app$wait_for_idle()
  
  app$click("maintenanceMetrics-introJS-help")
  app$wait_for_idle()
  
  maintenance_metrics <- dplyr::bind_rows(mm_steps, apptab_admn, apptab_steps, sidebar_steps)
  
  # Verify that all elements exist and are visible
  el_pos <- purrr::map(maintenance_metrics$element, getBoundingClientRect, appDriver = app)
  introjs_bullets <- app$get_js("$('.introjs-bullets ul li').length")
  expect(length(el_pos) == introjs_bullets, "One or more Upload Package introJS elements are missing.")
  expect(all(purrr::map_lgl(el_pos, ~ any(.x > 0))), "One or more elements are not visible")
  steps <- app$get_value(export = "maintenanceMetrics-introJS-steps")
  expect_equal(maintenance_metrics, steps)
  
  app$click(selector = ".introjs-skipbutton")
  
  app$set_inputs(tabs = "Community Usage Metrics")
  app$wait_for_idle()
  
  app$click("communityMetrics-introJS-help")
  app$wait_for_idle()
  
  community_metrics <- dplyr::bind_rows(cum_steps, apptab_admn, apptab_steps, sidebar_steps)
  
  # Verify that all elements exist and are visible
  el_pos <- purrr::map(community_metrics$element, getBoundingClientRect, appDriver = app)
  introjs_bullets <- app$get_js("$('.introjs-bullets ul li').length")
  expect(length(el_pos) == introjs_bullets, "One or more Upload Package introJS elements are missing.")
  expect(all(purrr::map_lgl(el_pos, ~ any(.x > 0))), "One or more elements are not visible")
  steps <- app$get_value(export = "communityMetrics-introJS-steps")
  expect_equal(community_metrics, steps)
  
  app$click(selector = ".introjs-skipbutton")
  app$set_inputs(tabs = "Report Preview")
  app$wait_for_idle()
  
  app$click("reportPreview-introJS-help")
  app$wait_for_idle()
  
  report_preview <- dplyr::bind_rows(rp_steps, apptab_admn, apptab_steps, sidebar_steps)
  
  # Verify that all elements exist and are visible
  el_pos <- purrr::map(report_preview$element, getBoundingClientRect, appDriver = app)
  introjs_bullets <- app$get_js("$('.introjs-bullets ul li').length")
  expect(length(el_pos) == introjs_bullets, "One or more Upload Package introJS elements are missing.")
  expect(all(purrr::map_lgl(el_pos, ~ any(.x > 0))), "One or more elements are not visible")
  steps <- app$get_value(export = "reportPreview-introJS-steps")
  expect_equal(report_preview, steps)
  
  app$stop()
})

test_that("The introJS module works as expected for nonadmins", {
  app_db_loc <- test_path("test-apps", "nonadmin-app", "database.sqlite")
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
  
  getBoundingClientRect <- function(appDriver, el) {
    appDriver$get_js(glue::glue('const rect = $("{el}")[0].getBoundingClientRect(); [rect.left, rect.top, rect.bottom, rect.right]')) %>% purrr::possibly(purrr::set_names, otherwise = .)(c("left", "top", "bottom", "right"))
  }
  
  # set up new app driver object
  app <- shinytest2::AppDriver$new(app_dir = test_path("test-apps", "nonadmin-app"),
                                   load_timeout = 600 * 1000)
  app$set_window_size(width = 1619, height = 1057)
  app$wait_for_idle()
  
  expect_equal(app$get_value(input = "tabs"), "Upload Package")
  
  # note upload_pkgs 
  upload_pkgs <- bind_rows(list(upload_pkg, apptab_steps, sidebar_steps))
  
  app$click("upload_package-introJS-help")
  app$wait_for_idle()
  
  # Verify that all elements exist and are visible
  el_pos <- purrr::map(upload_pkgs$element, getBoundingClientRect, appDriver = app)
  introjs_bullets <- app$get_js("$('.introjs-bullets ul li').length")
  expect(length(el_pos) == introjs_bullets, "One or more Upload Package introJS elements are missing.")
  expect(all(purrr::map_lgl(el_pos, ~ any(.x > 0))), "One or more elements are not visible")
  steps <- app$get_value(export = "upload_package-introJS-steps")
  expect_equal(upload_pkgs, steps)
  
  app$click(selector = ".introjs-skipbutton")
  
  app$run_js("Shiny.setInputValue('upload_package-load_cran', 'load')")
  app$wait_for_idle()
  app$set_inputs(`upload_package-pkg_lst` = "tidyr")
  app$click("upload_package-add_pkgs", wait_ = FALSE)
  app$wait_for_value(
    output = "upload_package-upload_pkgs_table",
    ignore = list(NULL),
    timeout = 30 * 1000
  )
  
  app$click("upload_package-introJS-help")
  app$wait_for_idle()
  
  upload_pkg_complete <- dplyr::bind_rows(list(upload_pkg, upload_pkg_comp, apptab_steps, sidebar_steps))
  
  # Verify that all elements exist and are visible
  el_pos <- purrr::map(upload_pkg_complete$element, getBoundingClientRect, appDriver = app)
  introjs_bullets <- app$get_js("$('.introjs-bullets ul li').length")
  expect(length(el_pos) == introjs_bullets, "One or more Upload Package introJS elements are missing.")
  expect(all(purrr::map_lgl(el_pos, ~ any(.x > 0))), "One or more elements are not visible")
  steps <- app$get_value(export = "upload_package-introJS-steps")
  expect_equal(upload_pkg_complete, steps)
  
  app$stop()
})