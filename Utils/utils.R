#####################################################################################################################
# utils.R - UI and Server utility functions for the application.
# Author: K Aravind Reddy
# Date: July 13th, 2020
# License: MIT License
#####################################################################################################################

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
    "create_Packageinfo_table.sql",
    "create_MaintenanceMetrics_table.sql",
    "create_CommunityUsageMetrics_table.sql",
    "create_TestMetrics_table.sql",
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
}

db_fun <- function(query){
  con <- dbConnect(RSQLite::SQLite(), db_name)
  dat <- dbGetQuery(con,query)  # this does SendQuery, Fetch and ClearResult all in one
  dbDisconnect(con)
  return(dat)
}

# You need to use dbExecute() to perform delete, update or insert queries.
db_ins<-function(query){
  # con <- dbConnect(RSQLite::SQLite(), "./risk_assessment_app.db")
  con <- dbConnect(RSQLite::SQLite(), db_name)
  dbExecute(con, query)
  dbDisconnect(con)
}


TimeStamp<-function(){
  # Timestamp_intial<-str_replace(Sys.time()," ", "; ")
  # Timestamp<-paste(Timestamp_intial, Sys.timezone())
  return(lubridate::with_tz(Sys.time(), "UTC"))
  # return(Timestamp)
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

get_versns <- function(package) {
  # given package name, return char vector of versions
  # using package versions seems really slow  
  # vrsn_lst <- versions::available.versions(package)
  # vrsn_vec <- unlist(vrsn_lst[[1]]$version)
  # this code replaces it.
  pkg_html <- read_html(paste0("https://github.com/cran/", package, "/tags"))
  pkg_nodes_v <- html_nodes(pkg_html, 'h4')
  pkg_text_v <- html_text(pkg_nodes_v)
  pkg_text_v <- str_split(pkg_text_v,"\n")
  pkg_vers <- rep("", length(pkg_text_v))
  for (k in 1:length(pkg_text_v)) {
    pkg_vers[k]<-(trimws(pkg_text_v[[k]][3]))
  }
  return(pkg_vers[which(!is.na(pkg_vers))]) 
}

packinfo <- function(package, versn) {

  if (package %in% installed.packages()[,1] && versn == gsub("'",'"',packageVersion(package)) ) {
    package_rm <- pkg_ref(package)
  } else {
    # is it on the search() path? then detach / install / attach
    if (package %in% (.packages())) {  
    detach_package(package)
    versions::install.versions(package, versn, lib=tempdir(), quiet = TRUE, type = "source")
    require(package, character.only=TRUE, lib.loc = .libPaths(), quietly = TRUE, warn.conflicts = FALSE)
   } else {
     # using remotes to do the install
     remotes::install_version(package, version = versn, lib = tempdir(), repos = "http://cran.us.r-project.org", quiet = TRUE, upgrade = FALSE)
   }
    package_rm <- pkg_ref(paste0(gsub("\\\\","/",tempdir()),"/",package)) 
    # remove.packages(package, lib = tempdir())
  }
  descr <- package_rm$description %>% as_tibble()
  
  # Need to remove single quotes from Title and Description
  descr$Title       <- str_replace_all(descr$Title, "'", "")  
  descr$Description <- str_replace_all(descr$Description, "'", "")
  
  return(list(ver= descr$Version, title= descr$Title, desc= descr$Description, 
              main= descr$Maintainer, auth= descr$Author, lis= descr$License, pub=descr$Packaged))
  
}

# this is to ensure that all copies of a package are detached.
detach_package <- function(pkg)
{
  search_item <- paste("package", pkg, sep = ":")
  while(search_item %in% search())
  {
    detach(search_item, character.only = TRUE, unload = FALSE)
  }
}