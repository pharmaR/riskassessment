#####################################################################################################################
# utils.R - UI and Server utility functions for the application.
# Author: K Aravind Reddy
# Date: July 13th, 2020
# License: MIT License
#####################################################################################################################

# Init DB using credentials data
credentials <- data.frame(
  user = c("shinyuser", "shinyadmin"),
  password = c("qwerty", "asdfgh"),
  # password will automatically be hashed
  admin = c(FALSE, TRUE),
  stringsAsFactors = FALSE
)

# you can use keyring package to set database key
library(keyring)
key_set_with_value("R-shinymanager-key", "obiwankenobi", password = "secret")

# Stores the database name.
db_name <- "database.sqlite"

# Create a local database.
create_db <- function(){
  
  # Create an empty database.
  con <- dbConnect(RSQLite::SQLite(), db_name)
  
  # Set the path to the queries.
  path <- file.path("Utils", "sql_queries")
  
  # Queries needed to run the first time the db is created.
  queries <- c(
    "create_package_table.sql",
    "create_metric_table.sql",
    "initialize_metric_table.sql",
    "create_package_metrics_table.sql",
    "create_CommunityUsageMetrics_table.sql",
    "create_Comments_table.sql"
  )
  
  # Append path to the queries.
  queries <- file.path(path, queries)
  
  # Apply each query.
  sapply(queries, function(x){
    res <- dbSendStatement(
      con,
      paste(scan(x, sep = "\n", what = "character"), collapse = ""))
    
    dbClearResult(res)
  })
  
  dbDisconnect(con)

  # Init the credentials database
  shinymanager::create_db(
    credentials_data = credentials,
    sqlite_path = file.path("credentials.sqlite"), 
    passphrase = key_get("R-shinymanager-key", "obiwankenobi")
    # passphrase = "passphrase_without_keyring"
  )
}

db_fun <- function(query, db_name = "database.sqlite"){
  con <- dbConnect(RSQLite::SQLite(), db_name)
  dat <- dbGetQuery(con,query)  # this does SendQuery, Fetch and ClearResult all in one
  dbDisconnect(con)
  return(dat)
}

db_rde <- function(query, db_name = "credentials.sqlite") {
  con <- dbConnect(RSQLite::SQLite(), db_name)
  dat <- read_db_decrypt(con, name = "credentials", 
         passphrase = key_get("R-shinymanager-key", "obiwankenobi"))
  dbDisconnect(con)
  return(dat)
}

# You need to use dbExecute() to perform delete, update or insert queries.
db_ins <- function(query, db_name = "database.sqlite"){
  con <- dbConnect(RSQLite::SQLite(), db_name)
  dbExecute(con, query)
  dbDisconnect(con)
}

TimeStamp <- function(){
  Timestamp_intial<-str_replace(Sys.time()," ", "; ")
  Timestamp <- paste(Timestamp_intial, Sys.timezone())
  return(Timestamp)
}

GetUserName <- function() {
  # Returns user name of computer with twist for Unix
  # Args
  #   none
  # Returns
  #  string of user login name
  
  x <- Sys.info()[["user"]]
  
  # if blank try other methods
  if (is.null(x) | x == "") {
    # On windows machines
    Sys.getenv("USERNAME")  
  } else {
    # from helpfiles for Unix
    Sys.getenv("LOGNAME")  
  }
  
  # Could get something but it is unknown error
  if (identical(x, "unknown")) {
    warning("unknown returned")
  }
  
  return(x)
}

# function to re-run everytime a package is uploaded to db, or
# when a comment is submitted
update_db_dash <- function(){
  db_fun(
    "SELECT 
       pi.name
      , pi.version
      , pi.score
      , pi.decision
      , c.last_comment
      FROM package as pi
      LEFT JOIN (
        SELECT comm_id
             , max(added_on) as last_comment
        FROM Comments
        GROUP BY comm_id
      ) as c
      on c.comm_id = pi.name
      ORDER BY 1 DESC
    "
  )
}

#' Function taken from formattable and modified for the risk assessment app.
#' Normalize a vector to fit zero-to-one scale
#'
#' @param x a numeric vector
#' @param min numeric value. The lower bound of the interval to normalize \code{x}.
#' @param max numeric value. The upper bound of the interval to normalize \code{x}.
#' @param na.rm a logical indicating whether missing values should be removed
#' @export
#' @examples
#' normalize(mtcars$mpg)
normalize <- function(x, min = 0, max = 1, na.rm = FALSE) {
  if (all(is.na(x))) return(rep(0, length(x)))
  if (!is.numeric(x)) stop("x must be numeric")
  x <- unclass(x)
  if (min > max) stop("min <= max must be satisfied")
  if (all(x == 0, na.rm = na.rm)) return(x)
  xmax <- 1
  xmin <- 0
  if (xmax == xmin) return(rep(1, length(x)))
  min + (max - min) * (x - xmin) / (xmax - xmin)
}
  