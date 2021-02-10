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
    "create_package_table.sql",
    "create_metric_table.sql",
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

  # build the table to initialize the metric db and run the query
  package_name <- "riskmetric"
  
  riskmetric_assess <-
    pkg_ref(package_name) %>%
    as_tibble() %>%
    pkg_assess()
  
  riskmetric_score <-
    riskmetric_assess %>%
    pkg_score()

  # exclude the following names
  excl_name <- c("package","version","pkg_ref","license","downloads_1yr","pkg_score")
  
  rm_assess <- select(riskmetric_assess, 
                      which(!names(riskmetric_assess) %in% excl_name))
  rm_score  <- select(riskmetric_score, 
                      which(!names(riskmetric_score) %in% excl_name))

  # get name and description directly from abbreviated riskmetric_assess
  # get name and description directly from riskmetric_assess
  name        <- names(rm_assess)
  label       <- map_chr(rm_assess, ~attr(.x, "label") )
  description <- map_chr(rm_score,  ~attr(.x, "label") )
  
  add_cmts <- tibble(
    name = c("news_current","has_vignettes","has_bug_reports_url",
             "bugs_status","export_help","has_website",
             "has_maintainer","has_news","has_source_control"),
    comment = c("News is current?","Presence of vignettes?",
                "Bugs publicly documented?",
                "Bug closure","Documentation","Associated website URL?",
                "Has a maintainer?","NEWS?","Source code public?"),
    ord = c(4,1,5,6,7,2,9,3,8)
    )
  
  # build tbl
  df_metric <- tibble(name,label,description) %>% 
    mutate(info_type = ifelse(str_detect(description,"fraction"), "percent", "binary")) %>% 
    mutate(description = str_replace(description,"fraction","percentage")) %>% 
    mutate(class = ifelse(name == "covr_coverage", "test", "maintenance")) %>% 
    mutate(weight = 1)
  
  df_metric <- left_join(df_metric, add_cmts, by = "name") %>% 
    mutate(comment = ifelse(is.na(comment), "", comment)) %>% 
    arrange(ord) %>% 
    select(name, comment, label, description, info_type, class, weight)
  
  # transpose it
  t_dfm <- t(df_metric)
  
  # build query -- assumes weight is always initalized to 1
  query <- paste0("INSERT INTO metric (name, comment, label, description, info_type, class, weight) 
                  values('",paste(t_dfm, collapse="','"), "')")
  query <- gsub("'1',","1),(",query)  # separate new rows with )(
  query <- gsub("'1'","1",query)
  
  # initialize the table
  db_ins(query)
}

db_fun <- function(query){
  con <- dbConnect(RSQLite::SQLite(), db_name)
  dat <- dbGetQuery(con,query)  # this does SendQuery, Fetch and ClearResult all in one
  dbDisconnect(con)
  return(dat)
}

# You need to use dbExecute() to perform delete, update or insert queries.
db_ins <- function(query){
  # con <- dbConnect(RSQLite::SQLite(), "./risk_assessment_app.db")
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
  