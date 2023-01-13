# Test Reactivity of database view table

# delete app DB if exists to ensure clean test
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
app <- AppDriver$new(app_dir = test_path("test-apps"))

app$set_inputs(apptabs = "database-tab")

test_that("The `table_data` loads correctly", {
  tbl_expect <-
    structure(list(name = "dplyr", version = "1.0.10", score = 0.1, 
                   was_decision_made = FALSE, decision = "-", 
                   last_comment = "-"), 
              class = "data.frame", row.names = c(NA, -1L))
  tbl_actual <-
    app$get_value(export = "databaseView-table_data")
  
  expect_equal(tbl_actual, tbl_expect)
})

test_that("`table_data` updates in response to `changes`", {
  app$set_inputs(`sidebar-select_pkg` = "dplyr")
  app$click("sidebar-submit_decision")
  app$wait_for_idle()
  app$click("sidebar-submit_confirmed_decision")
  
  tbl_expect <-
    structure(list(name = "dplyr", version = "1.0.10", score = 0.1, 
                   was_decision_made = TRUE, decision = "Low Risk", 
                   last_comment = "-"), 
              class = "data.frame", row.names = c(NA, -1L))
  tbl_actual <-
    app$get_value(export = "databaseView-table_data")
  
  expect_equal(tbl_actual, tbl_expect)
})

test_that("`table_data` updates in response to `uploaded_pkgs`", {
  tbl_actual <-
    app$get_value(export = "databaseView-table_data")
  
  app$run_js("Shiny.setInputValue('upload_package-load_cran', 'load')")
  app$wait_for_idle()
  app$set_inputs(`upload_package-pkg_lst` = "tidyr")
  app$click("upload_package-add_pkgs", wait_ = FALSE)
  app$wait_for_value(export = "databaseView-table_data", 
                     ignore = tbl_actual, timeout = 30 * 1000 )
  
  tbl_expect <- structure(list(name = c("tidyr", "dplyr"), 
                               was_decision_made = c(FALSE, TRUE), 
                               decision = c("-", "Low Risk"), 
                               last_comment = c("-", "-")), 
                          class = "data.frame", row.names = c(NA, -2L))
  tbl_actual <-
    app$get_value(export = "databaseView-table_data")
  
  expect_equal(tbl_actual %>% dplyr::select(1,4,5,6) %>% dplyr::arrange(1), tbl_expect)
})

test_that("`packages_table` is loaded correctly", {
  tbl_actual <-
    app$get_value(export = "databaseView-table_data") %>% 
    dplyr::mutate(was_decision_made = dplyr::if_else(was_decision_made, "Yes", "No"))
  
  packages_table <-
    app$get_html("#databaseView-packages_table") %>%
    rvest::minimal_html() %>%
    rvest::html_table() %>%
    `[[`(1)
  
  expect_equal(packages_table, tbl_actual, 
               ignore_attr = TRUE)
})

test_that("Download button is disabled if no packages selected.", {
  expect_equal(app$get_js("$('#databaseView-download_reports').attr('disabled')"), "disabled")
  app$run_js("Shiny.setInputValue('databaseView-packages_table_rows_selected', 1)")
  app$wait_for_idle()
  expect_equal(app$get_js("$('#databaseView-download_reports').attr('disabled')"), NULL)
  app$run_js("Shiny.setInputValue('databaseView-packages_table_rows_selected', null)")
  app$wait_for_idle()
  expect_equal(app$get_js("$('#databaseView-download_reports').attr('disabled')"), "disabled")
})

test_that("Download file works as expected.", {
  app$run_js("Shiny.setInputValue('databaseView-packages_table_rows_selected', 1)")
  app$wait_for_idle()
  # report <- app$get_download("databaseView-download_reports")
  
  app_download2 <- function(self, private,
                            output,
                            name = NULL,
                            save_dir = tempdir(),
                            use_counter = FALSE) {
    # Find the URL to download from (the href of the <a> tag)
    sub_url <- shinytest2:::chromote_eval(
      self$get_chromote_session(),
      paste0("$('#", output, "').attr('href')"),
      timeout = 100 * 1000
    )$result$value
    if (identical(sub_url, "")) {
      shinytest2:::app_abort(self, private, paste0("Download from '#", output, "' failed"))
    }

    # Add the base location to the URL
    full_url <- paste0(private$shiny_url$get(), sub_url)
    req <- shinytest2:::app_httr_get(self, private, full_url)

    # Find suggested name
    content_dispo <- httr::headers(req)[["content-disposition"]]
    filename_header <- NULL
    if (length(content_dispo) == 1 && is.character(content_dispo) && nzchar(content_dispo)) {
      vals <- strsplit(content_dispo, "filename=")[[1]]
      if (length(vals) > 1) {
        filename_header <- gsub("\"", "", vals[2], fixed = TRUE)
        self$log_message(paste0("Content disposition file name: ", filename_header))
      }
    }

    include_dash <- TRUE
    filename <-
      if (!is.null(name)) {
        # If a name is provided, use it
        name
      } else if (!is.null(filename_header)) {
        # If a suggested file name is provided, use it
        # Benefit: Snapshot `compare` will be smart if `compare == NULL`
        filename_header
      } else {
        # Use default name
        if (use_counter) {
          # `$expect_download()`
          # (The file counter will be prefixed in the follow chunk of code)
          include_dash <- FALSE
          ".download"
        } else {
          # `$get_download()`
          fs::path_file(tempfile(fileext = ".download"))
        }
      }
    # Prefix the downloaded file to make it uniquely named
    # Must be implemented until https://github.com/r-lib/testthat/pull/1592 is addressed
    if (use_counter) {
      dash <- if (include_dash) "-" else ""
      filename <- sprintf("%03d%s%s", private$counter$increment(), dash, filename)
    }

    download_path <- fs::path(
      # Save file to temp app dir
      save_dir,
      # Don't allow weird file paths from the application
      fs::path_sanitize(filename, "_")
    )
    # Save contents
    writeBin(req$content, download_path)

    list(
      download_path = download_path,
      filename_header = filename_header
    )
  }


  app_get_download2 <- function(self, private,
                                output,
                                filename = NULL) {

    if (is.null(filename)) {
      # Save to a temporary file with possibly suggested name
      save_dir <- tempdir()
      name <- NULL
    } else {
      # Use provided filename info
      save_dir <- fs::path_dir(filename)
      name <- fs::path_file(filename)
    }

    snapshot_info <- app_download2(
      self, private,
      output = output,
      name = name,
      save_dir = save_dir,
      use_counter = FALSE
    )

    snapshot_info$download_path
  }
  
  
  report <- app_get_download2(
    self = app$.__enclos_env__$self, app$.__enclos_env__$private,
    output = "databaseView-download_reports",
    filename = NULL
  )

  expect_equal(tools::file_ext(report), "html")
  
  # app$run_js("Shiny.setInputValue('databaseView-packages_table_rows_selected', [1,2])")
  # app$wait_for_idle()
  # report <- app$get_download("databaseView-download_reports")
  # expect_equal(tools::file_ext(report), "zip")
})
