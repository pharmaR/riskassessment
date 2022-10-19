#' Initialize the Risk Assessment Application
#'
#' @description This sets up the environment when running the Risk Assessment
#'   Application. It sets the log file, initializes the package database if
#'   missing, and initializes the credentials database if missing.
#'
#' @return There is no return value. The function is run for its side effects.
#' @importFrom loggit set_logfile
#'
#' @export
initialize_raa <- function() {
  # Start logging info.
  loggit::set_logfile("loggit.json")
  
  
  # Create package db & credentials db if it doesn't exist yet.
  if(!file.exists(database_name)) create_db()
  if(!file.exists(credentials_name)) create_credentials_db()
}


#' Application Theme
#'
#' @description This sets the Risk Assessment Application theme object using
#'   bslib's bs_theme() function. The app_theme object gets used in run_app.R,
#'   in addition to app_ui.R
#'
#' @return an bs_theme object of several classes
#' @importFrom bslib bs_theme
#'
#' @export
app_theme <- bslib::bs_theme(
  bootswatch = "lux",
  version = 5,
  # bg = "white", 
  # fg = "#023967",
  primary = "#24305E",
  secondary = "#F76C6C",
  # success = "orange",
  # info = "yellow",
  # warning = "pink"
)
