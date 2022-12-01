#' Create package database
#' 
#' @description Note: the database_name object is assigned by deployment users in R/run_app.R
#' 
#' @param db_name a string
#' 
#' @import dplyr
#' @importFrom DBI dbConnect dbDisconnect dbSendStatement dbClearResult
#' @importFrom RSQLite SQLite
#' @importFrom loggit loggit
#' 
create_db <- function(db_name = golem::get_golem_options('assessment_db_name')){
  
  # Create an empty database.
  con <- DBI::dbConnect(RSQLite::SQLite(), db_name)
  
  # Set the path to the queries.
  path <- app_sys("app/www/sql_queries") #file.path('sql_queries')
  
  # Queries needed to run the first time the db is created.
  queries <- c(
    "create_package_table.sql",
    "create_metric_table.sql",
    "initialize_metric_table.sql",
    "create_package_metrics_table.sql",
    "create_community_usage_metrics_table.sql",
    "create_comments_table.sql"
  )
  
  # Append path to the queries.
  queries <- file.path(path, queries)
  
  # Apply each query.
  sapply(queries, function(x){
    
    tryCatch({
      rs <- DBI::dbSendStatement(
        con,
        paste(scan(x, sep = "\n", what = "character"), collapse = ""))
    }, error = function(err) {
      message <- paste("dbSendStatement",err)
      message(message, .loggit = FALSE)
      loggit::loggit("ERROR", message)
      DBI::dbDisconnect(con)
    })
    
    DBI::dbClearResult(rs)
  })
  
  DBI::dbDisconnect(con)
}



#' Create credentials database
#' 
#' Note: the credentials_db_name object is assigned by the deployment user in R/run_app.R
#' 
#' @param db_name a string
#' 
#' @import dplyr
#' @importFrom DBI dbConnect dbDisconnect
#' @importFrom RSQLite SQLite
#' @importFrom shinymanager read_db_decrypt write_db_encrypt
#' 
create_credentials_db <- function(db_name = golem::get_golem_options('credentials_db_name')){
  
  # Init the credentials table for credentials database
  credentials <- data.frame(
    user = "ADMIN",
    password = "QWERTY1",
    # password will automatically be hashed
    admin = TRUE,
    expire = as.character(Sys.Date()),
    stringsAsFactors = FALSE
  )
  
  # Init the credentials database
  shinymanager::create_db(
    credentials_data = credentials,
    sqlite_path = file.path(db_name), 
    passphrase = passphrase
  )
  
  # set pwd_mngt$must_change to TRUE
  con <- DBI::dbConnect(RSQLite::SQLite(), db_name)
  pwd <- shinymanager::read_db_decrypt(
    con, name = "pwd_mngt",
    passphrase = passphrase) %>%
    dplyr::mutate(must_change = ifelse(
      have_changed == "TRUE", must_change, as.character(TRUE)))
  
  shinymanager::write_db_encrypt(
    con,
    value = pwd,
    name = "pwd_mngt",
    passphrase = passphrase
  )
  DBI::dbDisconnect(con)
  
  # update expire date here to current date + 365 days
  con <- DBI::dbConnect(RSQLite::SQLite(), db_name)
  dat <- shinymanager::read_db_decrypt(con, name = "credentials", passphrase = passphrase) %>%
    dplyr::mutate(expire = as.character(Sys.Date() + 365))
  
  shinymanager::write_db_encrypt(
    con,
    value = dat,
    name = "credentials",
    passphrase = passphrase
  )
  
  DBI::dbDisconnect(con)
}


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
  if(!file.exists(golem::get_golem_options('assessment_db_name'))) create_db()
  if(!file.exists(golem::get_golem_options('credentials_db_name'))) create_credentials_db()
}


#' The 'Add tags' function
#' 
#' @param ui placeholder
#' @param ... placeholder
#' 
#' 
#' @importFrom shinymanager fab_button
#' @importFrom shinyjs useShinyjs
add_tags <- function(ui, ...) {
  ui <- force(ui)
  
  function(request) {
    query <- parseQueryString(request$QUERY_STRING)
    admin <- query$admin
    
    if (is.function(ui)) {
      ui <- ui(request)
    }
    
    if (identical(admin, "true")) {
      ui <- tagList(ui, 
                    tags$script(HTML("document.getElementById('admin-add_user').style.width = 'auto';")),
                    tags$script(HTML("var oldfab = Array.prototype.slice.call(document.getElementsByClassName('mfb-component--br'), 0);
                             for (var i = 0; i < oldfab.length; ++i) {
                               oldfab[i].remove();
                             }")),
                    shinymanager::fab_button(
                      position = "bottom-right",
                      actionButton(
                        inputId = ".shinymanager_logout",
                        label = "Logout",
                        icon = icon("right-from-bracket")
                      ),
                      actionButton(
                        inputId = ".shinymanager_app",
                        label = "Go to application",
                        icon = icon("share")
                      )
                    )
      )
    }
    
    tagList(shinyjs::useShinyjs(),
            ui,
            tags$script(HTML("$(document).on('shiny:value', function(event) {
                             if (event.target.id === 'admin-table_users') {
                             Shiny.onInputChange('table_users-returns', document.getElementById('admin-table_users').innerHTML)
                             } else if (event.target.id === 'admin-table_pwds') {
                             Shiny.onInputChange('table_pwds-returns', document.getElementById('admin-table_pwds').innerHTML)
                             }
                             });")))
  }
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
