#####################################################################################################################
# utils.R - UI and Server utility functions for the application.
# Author: K Aravind Reddy
# Date: July 13th, 2020
# License: MIT License
#####################################################################################################################

# function defined to provide consistent risk color palettes across multiple "modules"
# https://www.rapidtables.com/web/color/html-color-codes.html
low_risk_color  <- "#228B22"  # forest green
med_risk_color  <- "#d1b000"  # dark gold
high_risk_color <- "#B22222"  # firebrick
colfunc <- colorRampPalette(c(low_risk_color, med_risk_color, high_risk_color))


# Init DB using credentials data.
credentials <- data.frame(
  user = "admin",
  password = "qwerty",
  # password will automatically be hashed
  admin = TRUE,
  expire = as.character(Sys.Date()),
  stringsAsFactors = FALSE
)

# Use keyring package to set database key.
key_set_with_value("R-shinymanager-key", "obiwankenobi", password = "secret")


# Stores the database name.
database_name <- "database.sqlite"

# Create a local database.
create_db <- function(db_name = database_name){
  
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
    
    tryCatch({
      rs <- dbSendStatement(
        con,
        paste(scan(x, sep = "\n", what = "character"), collapse = ""))
    }, error = function(err) {
      message <- paste("dbSendStatement",err)
      message(message, .loggit = FALSE)
      loggit("ERROR", message)
      dbDisconnect(con)
    })
    
    dbClearResult(rs)
  })
  
  dbDisconnect(con)
}

# Stores the database name.
credentials_name <- "credentials.sqlite"

# Create credentials database
create_credendials_db <- function(db_name = credentials_name){
  
  # Init the credentials database
  shinymanager::create_db(
    credentials_data = credentials,
    sqlite_path = file.path(db_name), 
    passphrase = key_get("R-shinymanager-key", "obiwankenobi")
  )
  
  # set pwd_mngt$must_change to TRUE
  con <- dbConnect(RSQLite::SQLite(), db_name)
  pwd <- read_db_decrypt(
    con, name = "pwd_mngt",
    passphrase = key_get("R-shinymanager-key", "obiwankenobi")) %>%
    mutate(must_change = ifelse(
      have_changed == "TRUE", must_change, as.character(TRUE)))
  
  write_db_encrypt(
    con,
    value = pwd,
    name = "pwd_mngt",
    passphrase = key_get("R-shinymanager-key", "obiwankenobi")
  )
  
  dbDisconnect(con)
}

db_fun <- function(query, db_name = database_name){
  errFlag <- FALSE
  con <- dbConnect(RSQLite::SQLite(), db_name)
  tryCatch(
    expr = {
      rs <- dbSendQuery(con, query)
    },
    warning = function(warn) {
      message <- paste0("warning:\n", query, "\nresulted in\n", warn)
      message(message, .loggit = FALSE)
      loggit("WARN", message)
      errFlag <<- TRUE
    },
    error = function(err) {
      message <- paste0("error:\n", query, "\nresulted in\n",err)
      message(message, .loggit = FALSE)
      loggit("ERROR", message)
      dbDisconnect(con)
      errFlag <<- TRUE
    },
    finally = {
      if (errFlag) return(NULL) 
    })
  
  dat <- dbFetch(rs)
  dbClearResult(rs)
  if (nrow(dat) == 0 
     & (str_detect(query, "name = 'Select'") == FALSE) && str_detect(query, "name = ''") == FALSE) {
    message(paste0("No rows were returned from db_fun query\n",query))
  }
  dbDisconnect(con)
  return(dat)
}

db_rde <- function(query, name = "credentials", db_name = "credentials.sqlite") {
  con <- dbConnect(RSQLite::SQLite(), db_name)
  dat <- read_db_decrypt(con, name, 
         passphrase = key_get("R-shinymanager-key", "obiwankenobi"))
  dbDisconnect(con)
  return(dat)
}

# You need to use dbExecute() to perform delete, update or insert queries.
db_ins <- function(command, db_name = database_name){
  con <- dbConnect(RSQLite::SQLite(), db_name)
  tryCatch({
    rs <- dbSendStatement(con, command)
  }, error = function(err) {
    message <- paste0("command:\n",command,"\nresulted in\n",err)
    message(message, .loggit = FALSE)
    loggit("ERROR", message)
    dbDisconnect(con)
  })
  nr <- dbGetRowsAffected(rs)
  dbClearResult(rs)
  if (nr == 0) {
    message <- paste0("zero rows were affected by the db_ins command:\n",command)
    message(message, .loggit = FALSE)
  }
  dbDisconnect(con)
}


TimeStamp <- function(){
  Timestamp_intial <- str_replace(Sys.time(), " ", "; ")
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

# Get each metric's weight.
get_metric_weights <- function(){
  db_fun(
    "SELECT name, weight
    FROM metric"
  )
}

# Update metric's weight.
update_metric_weight <- function(metric_name, metric_weight){
  db_ins(paste0(
    "UPDATE metric ",
    "SET weight = ", metric_weight, " ",
    "WHERE name = ", "'", metric_name, "'"
  ))
}


