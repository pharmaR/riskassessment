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
  
  app_tar_loc <- test_path("test-apps", "tarballs", "tidyr_1.3.0.tar.gz")
  if (!dir.exists(dirname(app_tar_loc))) {
    dir.create(dirname(app_tar_loc))
  }
  if (!file.exists(app_tar_loc)) {
    download.file(
      "https://cran.r-project.org/src/contrib/Archive/tidyr/tidyr_1.3.0.tar.gz",
      app_tar_loc,
      mode = "wb"
    )
  }
  
  getBoundingClientRect <- function(appDriver, el) {
    appDriver$get_js(glue::glue('const rect = $("{el}")[0].getBoundingClientRect(); [rect.left, rect.top, rect.bottom, rect.right]')) %>% purrr::possibly(purrr::set_names, otherwise = .)(c("left", "top", "bottom", "right"))
  }
  
  # set up new app driver object
  app <- shinytest2::AppDriver$new(app_dir = test_path("test-apps"))
  app$set_window_size(width = 1619, height = 1057)
  app$wait_for_idle()
  
  expect_equal(app$get_value(input = "tabs"), "Upload Package")
  
  # note upload_pkgs 
  upload_pkgs <- bind_rows(list(upload_pkg, upload_pkg_add, upload_pkg_delete, upload_pkg_dec_adj, apptab_admn, apptab_steps, sidebar_steps))
  
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
  
  app$run_js("Shiny.setInputValue('upload_package-load_repo_pkgs', 'load')")
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
  
  upload_pkg_complete <- bind_rows(list(upload_pkg, upload_pkg_add, upload_pkg_delete, upload_pkg_dec_adj, upload_pkg_comp, apptab_admn, apptab_steps, sidebar_steps))
  
  # Verify that all elements exist and are visible
  el_pos <- purrr::map(upload_pkg_complete$element, getBoundingClientRect, appDriver = app)
  introjs_bullets <- app$get_js("$('.introjs-bullets ul li').length")
  expect(length(el_pos) == introjs_bullets, "One or more Upload Package introJS elements are missing.")
  expect(all(purrr::map_lgl(el_pos, ~ any(.x > 0))), "One or more elements are not visible")
  steps <- app$get_value(export = "upload_package-introJS-steps")
  expect_equal(upload_pkg_complete, steps)
  
  app$click(selector = ".introjs-skipbutton")
  app$set_inputs(tabs = "Package Metrics",
                 metric_type = "mm")
  app$set_inputs(`sidebar-select_pkg` = "tidyr")
  app$wait_for_idle(timeout = 30 * 1000)
  
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
  
  app$set_inputs(tabs = "Source Explorer",
                 explorer_type = "fb")
  
  app$set_inputs(`sidebar-select_pkg` = "tidyr")
  
  app$wait_for_idle()  

  app$click("pkg_explorer-introJS-help")
 
  
  
  pkg_explorer <- dplyr::bind_rows(pe_steps, apptab_admn, apptab_steps, sidebar_steps)
  
  # Verify that all elements exist and are visible
  el_pos <- purrr::map(pkg_explorer$element, getBoundingClientRect, appDriver = app)
  introjs_bullets <- app$get_js("$('.introjs-bullets ul li').length")
  expect(length(el_pos) == introjs_bullets, "One or more Upload Package introJS elements are missing.")
  expect(all(purrr::map_lgl(el_pos, ~ any(.x > 0))), "One or more elements are not visible")
  steps <- app$get_value(export = "pkg_explorer-introJS-steps")
  expect_equal(pkg_explorer, steps)
  
  app$click(selector = ".introjs-skipbutton")
  
  app$set_inputs(tabs = "Source Explorer",
                 explorer_type = "fe")
  
  app$set_inputs(`sidebar-select_pkg` = "tidyr")
  app$wait_for_idle()
  
  app$click("code_explorer-introJS-help")
  
  
  code_explorer <- dplyr::bind_rows(fe_steps, apptab_admn, apptab_steps, sidebar_steps)
  
  # Verify that all elements exist and are visible
  el_pos <- purrr::map(code_explorer$element, getBoundingClientRect, appDriver = app)
  introjs_bullets <- app$get_js("$('.introjs-bullets ul li').length")
  expect(length(el_pos) == introjs_bullets, "One or more Upload Package introJS elements are missing.")
  expect(all(purrr::map_lgl(el_pos, ~ any(.x > 0))), "One or more elements are not visible")
  steps <- app$get_value(export = "code_explorer-introJS-steps")
  expect_equal(code_explorer, steps)
  
  app$click(selector = ".introjs-skipbutton")
  
  app$set_inputs(tabs = "Package Metrics",
                 metric_type = "cum")
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
  app$set_inputs(tabs = "Build Report")
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
  app <- shinytest2::AppDriver$new(app_dir = test_path("test-apps", "nonadmin-app"))
  app$set_window_size(width = 1619, height = 1057)
  app$wait_for_idle()
  
  expect_equal(app$get_value(input = "tabs"), "Upload Package")
  
  # note upload_pkgs 
  upload_pkgs <- bind_rows(list(upload_pkg, upload_pkg_add, apptab_steps, sidebar_steps))
  
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
  
  app$run_js("Shiny.setInputValue('upload_package-load_repo_pkgs', 'load')")
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
  
  upload_pkg_complete <- dplyr::bind_rows(list(upload_pkg, upload_pkg_add, upload_pkg_comp, apptab_steps, sidebar_steps))
  
  # Verify that all elements exist and are visible
  el_pos <- purrr::map(upload_pkg_complete$element, getBoundingClientRect, appDriver = app)
  introjs_bullets <- app$get_js("$('.introjs-bullets ul li').length")
  expect(length(el_pos) == introjs_bullets, "One or more Upload Package introJS elements are missing.")
  expect(all(purrr::map_lgl(el_pos, ~ any(.x > 0))), "One or more elements are not visible")
  steps <- app$get_value(export = "upload_package-introJS-steps")
  expect_equal(upload_pkg_complete, steps)
  
  app$stop()
})
