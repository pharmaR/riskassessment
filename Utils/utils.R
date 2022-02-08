# Init DB using credentials data.
credentials <- data.frame(
  user = "admin",
  password = "qwerty",
  # password will automatically be hashed
  admin = TRUE,
  expire = as.character(Sys.Date()),
  stringsAsFactors = FALSE
)

# Stores the database name.
database_name <- "database.sqlite"
# Store default backup name.
bk_name <- "dbbackup.sqlite"


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
    "create_community_usage_metrics_table.sql",
    "create_comments_table.sql"
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
create_credentials_db <- function(db_name = credentials_name, username = getOption("keyring_user")){
  
  key_set_with_value("R-shinymanager-key", username, 
                     password = rstudioapi::askForPassword("Please create a keyring password"))
  
  # Init the credentials database
  shinymanager::create_db(
    credentials_data = credentials,
    sqlite_path = file.path(db_name), 
    passphrase = key_get("R-shinymanager-key", username)
  )
  
  # set pwd_mngt$must_change to TRUE
  con <- dbConnect(RSQLite::SQLite(), db_name)
  pwd <- read_db_decrypt(
    con, name = "pwd_mngt",
    passphrase = key_get("R-shinymanager-key", username)) %>%
    mutate(must_change = ifelse(
      have_changed == "TRUE", must_change, as.character(TRUE)))
  
  write_db_encrypt(
    con,
    value = pwd,
    name = "pwd_mngt",
    passphrase = key_get("R-shinymanager-key", username)
  )
  dbDisconnect(con)
  
  # update expire date here to current date + 365 days
  con <- dbConnect(RSQLite::SQLite(), db_name)
  dat <- read_db_decrypt(con, name = "credentials",
                         passphrase = key_get("R-shinymanager-key", username))
  
  dat <- dat %>%
    mutate(expire = as.character(Sys.Date()+365))
  
  write_db_encrypt(
    con,
    value = dat,
    name = "credentials",
    passphrase = key_get("R-shinymanager-key", username)
  )
  
  dbDisconnect(con)
}

dbSelect <- function(query, db_name = database_name){
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
    message(paste0("No rows were returned from query\n",query))
  }
  dbDisconnect(con)
  return(dat)
}

# Deletes, updates or inserts queries.
dbUpdate <- function(command, db_name = database_name){
  con <- dbConnect(RSQLite::SQLite(), db_name)
  tryCatch({
    rs <- dbSendStatement(con, command)
  }, error = function(err) {
    message <- glue("command: {command} resulted in {err}")
    message(message, .loggit = FALSE)
    loggit("ERROR", message)
    dbDisconnect(con)
  })
  nr <- dbGetRowsAffected(rs)
  dbClearResult(rs)
  if (nr == 0) {
    message <- glue("zero rows were affected by the command: {command}")
    message(message, .loggit = FALSE)
  }
  dbDisconnect(con)
}


getTimeStamp <- function(){
  initial <- str_replace(Sys.time(), " ", "; ")
  return(paste(initial, Sys.timezone()))
}

# Get each metric's weight.
get_metric_weights <- function(){
  dbSelect(
    "SELECT name, weight, weight AS new_weight
     FROM metric"
  )
}

# Used to add a comment on every tab saying how the risk and weights changed, and that
# the overall comment & final decision may no longer be applicable.
weight_risk_comment <- function(pkg_name) {
  
  pkg_score <- dbSelect(glue(
    "SELECT score
     FROM package
     WHERE name = '{pkg_name}'"
  ))
  
  glue('Metric re-weighting has occurred.
       The previous risk score was {pkg_score}.')
}

# Update metric's weight.
update_metric_weight <- function(metric_name, metric_weight){
  dbUpdate(glue(
    "UPDATE metric
    SET weight = {metric_weight},
    WHERE name = '{metric_name}'"
  ))
}
