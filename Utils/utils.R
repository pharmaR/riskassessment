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

install_tempdir <- function(package_name, package_ver) {
  packageurl <- paste0("https://cran.r-project.org/src/contrib/Archive/", package_name, "/", package_name, "_", package_ver, ".tar.gz")
  temptargz <- tempfile("download", fileext = ".tar.gz")
  download.file(packageurl, destfile= temptargz, quiet = TRUE)
  untar(temptargz, files = c(paste0(package_name,"/DESCRIPTION"),
                             paste0(package_name,"/NEWS.md"),
                             paste0(package_name,"/vignettes/")),
        exdir = tempdir() )
}

packinfo <- function(package, versn) {
  
  if (package %in% installed.packages()[,1] && versn == getNamespaceVersion(package) ) {
    package_rm <- pkg_ref(package)  # pkg_install
  } else if(versn == get_versns(package)[[1]]){
    package_rm <- pkg_ref(package)  # pkg_remote
  } else {
    if (package %in% (.packages())) {  
      # is it on the search() path? then detach / install / attach
      detach_package(package)
      install_tempdir(package, versn)
      require(package, character.only=TRUE, lib.loc = .libPaths(), quietly = TRUE, warn.conflicts = FALSE)
    } else {
      install_tempdir(package, versn)
    }
    package_rm <- pkg_ref(file.path(gsub("\\\\","/",tempdir()),package)) # pkg_source
  }
  descr <- package_rm$description %>% as_tibble() 
  
  # Need to remove single quotes from Title and Description
  descr$Title       <- str_replace_all(descr$Title, "'", "")  
  descr$Description <- str_replace_all(descr$Description, "'", "")
  
  if (package_rm$source == "pkg_remote") {
    publ <- descr$Published
  } else {
    publ <- descr$`Date/Publication`
  }
  return(list(ver= descr$Version, title= descr$Title, desc= descr$Description, source=package_rm$source,
              main= descr$Maintainer, auth= descr$Author, lis= descr$License, pub=publ))
  
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

pkg_ref_cache.description.pkg_remote <- function(x, name, ...) {

  webpage <- httr::content(httr::GET(paste0(x$repo_base_url,"/package=",x$name)),
                           encoding = "UTF-8")
  
  title <- webpage %>% 
    rvest::html_nodes('h2') %>% 
    rvest::html_text() %>% 
    gsub(c("\n |\n|'|\""),"", .)   # remove line breaks, single and double quotes

  desc <- webpage %>% 
    rvest::html_nodes('p') %>% 
    rvest::html_text() %>% 
    gsub(c("\n |\n|'|\""),"", .)   # remove line breaks, single and double quotes
  desc <- desc[[1]]
  
  td_nodes <- webpage %>% 
    rvest::html_nodes('td') %>% 
    rvest::html_text()
  
  nodnames <- td_nodes[seq_along(td_nodes) %% 2 >  0] %>% 
    lapply(., function(x) trimws(x)) %>%  
    lapply(., function(x) gsub(":", "", x)) %>% 
    unlist()
  
  nodvalus <- td_nodes[seq_along(td_nodes) %% 2 == 0] %>% 
    lapply(., function(x) trimws(x)) %>%  
    lapply(., function(x) gsub(c("\n|'|\""),"", x)) %>% 
    unlist()

  retlist <- as.list(setNames(nodvalus, nodnames))
  retlist[["Title"]] <- title
  retlist[["Description"]] <- desc
  
  return(retlist)
}