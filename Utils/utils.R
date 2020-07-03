

db_fun<-function(query){
  con <- dbConnect(RSQLite::SQLite(), "./risk_assessment_app.db")
  res <- dbSendQuery(con, query)
  res <- dbFetch(res)
  dbDisconnect(con)
  return(res)
}


TimeStamp<-function(){
  #with_tz(Sys.time(), "Asia/Kolkata")
  Timestamp_intial<-str_replace(Sys.time()," ", "; ")
  Timestamp<-paste(Timestamp_intial, Sys.timezone())
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