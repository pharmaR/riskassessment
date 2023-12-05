# MAKE SURE YOU HAVE INSTALLED YOUR BRANCH WITH THE CHANGES!!!!
# devtools::install(dependencies = FALSE)

devtools::load_all()
old <- options()
options(golem.app.prod = FALSE)
db_config <- get_db_config(NULL)
shiny::shinyOptions(golem_options = list(pre_auth_user = "admin",
                                         decision_categories = db_config$decisions$categories))

skeleton_db <- "./inst/testdata/skeleton.sqlite"
if (file.exists(skeleton_db))
  file.remove(skeleton_db)
initialize_raa(skeleton_db, tempfile("credentials", fileext = ".sqlite"))

options(shiny.testmode = TRUE)
options(shinytest2.timeout = 90*1000)

app_db_loc <- test_path("test-apps", "database.sqlite")
if (file.exists(app_db_loc)) {
  file.remove(app_db_loc)
}

# copy in already instantiated database to avoid need to rebuild
# this is a database that has been built via inst/testdata/upload_format.csv
test_db_loc <- app_sys("testdata", "skeleton.sqlite")
file.copy(
  test_db_loc,
  app_db_loc
)

# Using the testing app for minimal impact on environment
app <- shinytest2::AppDriver$new(app_dir = test_path("test-apps"),
                                 load_timeout = 90*1000)

app$run_js("Shiny.setInputValue('upload_package-load_repo_pkgs', 'load')")
app$wait_for_idle()
app$set_inputs(`upload_package-pkg_lst` = "dplyr")
app$click("upload_package-add_pkgs", wait_ = FALSE)
app$wait_for_value(
  output = "upload_package-upload_pkgs_table",
  ignore = list(NULL),
  timeout = 30 * 1000
)

file.copy(
  app_db_loc, 
  "./inst/testdata/upload_format.database", 
  overwrite = TRUE
)

app$set_inputs(`upload_package-pkg_lst` = "tidyr")
app$click("upload_package-add_pkgs", wait_ = FALSE)
app$wait_for_value(
  output = "upload_package-upload_pkgs_table",
  ignore = list(NULL),
  timeout = 30 * 1000
)

file.copy(
  app_db_loc, 
  test_path("test-apps", "downloadHandler-app", "dplyr_tidyr.sqlite"), 
  overwrite = TRUE
)

app$stop()
options(shiny.testmode = NULL)
options(shinytest2.timeout = NULL)

Sys.setenv("GOLEM_CONFIG_ACTIVE" = "example")
db_config <- get_db_config(NULL)
shiny::shinyOptions(golem_options = list(pre_auth_user = "admin",
                                         decision_categories = db_config$decisions$categories))
dec_auto_db <- "./inst/testdata/decision_automation_ex1.sqlite"
if (file.exists(dec_auto_db))
  file.remove(dec_auto_db)
initialize_raa(dec_auto_db, tempfile("credentials", fileext = ".sqlite"))

options(golem.app.prod = NULL)
options(old)
Sys.unsetenv("GOLEM_CONFIG_ACTIVE")
rm(app, db_config, app_db_loc, dec_auto_db, skeleton_db, test_db_loc, old)
