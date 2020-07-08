library(RSQLite)
library(DBI)

con <- dbConnect(RSQLite::SQLite(), "./risk_assessment_app.db")
dbListTables(con)



con <- dbConnect(RSQLite::SQLite(), "./risk_assessment_app.db")
dbSendQuery(con, "delete from Packageinfo")
dbDisconnect(con)

con <- dbConnect(RSQLite::SQLite(), "./risk_assessment_app.db")
q<-dbSendQuery(con, "select * from Packageinfo")
q<-dbFetch(q)
q
dbDisconnect(con)



con <- dbConnect(RSQLite::SQLite(), "./risk_assessment_app.db")
dbSendQuery(con, "delete from MaintenanceMetrics")
dbDisconnect(con)

con <- dbConnect(RSQLite::SQLite(), "./risk_assessment_app.db")
q<-dbSendQuery(con, "select * from MaintenanceMetrics")
q<-dbFetch(q)
q
dbDisconnect(con)




con <- dbConnect(RSQLite::SQLite(), "./risk_assessment_app.db")
dbSendQuery(con, "delete from CommunityUsageMetrics")
dbDisconnect(con)

con <- dbConnect(RSQLite::SQLite(), "./risk_assessment_app.db")
q<-dbSendQuery(con, "select * from CommunityUsageMetrics")
q<-dbFetch(q)
q
dbDisconnect(con)




con <- dbConnect(RSQLite::SQLite(), "./risk_assessment_app.db")
dbSendQuery(con, "delete from TestMetrics")
dbDisconnect(con)

con <- dbConnect(RSQLite::SQLite(), "./risk_assessment_app.db")
q<-dbSendQuery(con, "select * from TestMetrics")
q<-dbFetch(q)
q
dbDisconnect(con)




con <- dbConnect(RSQLite::SQLite(), "./risk_assessment_app.db")
dbSendQuery(con, "delete from Comments")
dbDisconnect(con)

con <- dbConnect(RSQLite::SQLite(), "./risk_assessment_app.db")
q<-dbSendQuery(con, "select * from Comments")
q<-dbFetch(q)
q
dbDisconnect(con)
