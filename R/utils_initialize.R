#' Initialize the Risk Assessment Application 
#'
#' @description This sets up the environment when running the Risk Assessment Application. It sets the log file, initializes the package database if missing, and initializes the credentials database if missing.
#'
#' @return There is no return value. The function is run for its side effects.
#'
#' @export
initialize_raa <- function() {
  # Start logging info.
  loggit::set_logfile("loggit.json")
  
  
  # Create package db & credentials db if it doesn't exist yet.
  if(!file.exists(database_name)) create_db()
  if(!file.exists(credentials_name)) create_credentials_db()
}