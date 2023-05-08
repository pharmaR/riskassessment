
#' Select data from database
#' 
#' @param query a sql query as a string
#' @param db_name character name (and file path) of the database
#' @param .envir Environemtn to evaluate each expression in
#' 
#' @import dplyr
#' @importFrom DBI dbConnect dbSendQuery dbFetch dbClearResult dbDisconnect
#' @importFrom RSQLite SQLite
#' @importFrom loggit loggit
#' 
#' @returns a data frame
#'
#' @noRd
dbSelect <- function(query, db_name = golem::get_golem_options('assessment_db_name'), .envir = parent.frame()){
  errFlag <- FALSE
  con <- DBI::dbConnect(RSQLite::SQLite(), db_name)
  
  tryCatch(
    expr = {
      rs <- DBI::dbSendQuery(con, glue::glue_sql(query, .envir = .envir, .con = con))
    },
    warning = function(warn) {
      message <- paste0("warning:\n", query, "\nresulted in\n", warn)
      message(message, .loggit = FALSE)
      loggit::loggit("WARN", message, echo = FALSE)
      errFlag <<- TRUE
    },
    error = function(err) {
      message <- paste0("error:\n", query, "\nresulted in\n",err)
      message(message, .loggit = FALSE)
      loggit::loggit("ERROR", message, echo = FALSE)
      DBI::dbDisconnect(con)
      errFlag <<- TRUE
    },
    finally = {
      if (errFlag) return(NULL) 
    })
  
  dat <- DBI::dbFetch(rs)
  DBI::dbClearResult(rs)
  DBI::dbDisconnect(con)
  
  return(dat)
}


# Below are a series of get_* functions that help us query
# certain sql tables in a certain way. They are used 2 - 3
# times throughout the app, so it's best to maintain them
# in a central location

#' The 'Get Overall Comments' function
#' 
#' Retrieves the overall comments for a specific package
#' 
#' @param pkg_name character name of the package
#' @param db_name character name (and file path) of the database
#' 
#' @importFrom glue glue
#' 
#' @returns a data frame
#' @noRd
get_overall_comments <- function(pkg_name, db_name = golem::get_golem_options('assessment_db_name')) {
  dbSelect(
    "SELECT * FROM comments 
     WHERE comment_type = 'o' AND id = {pkg_name}", db_name
  )
}


#' The 'Get Maintenance Metrics Comments' function
#' 
#' Retrieves the Maint Metrics comments for a specific package
#' 
#' @param pkg_name character name of the package
#' @param db_name character name (and file path) of the database
#' 
#' @importFrom glue glue
#' @importFrom purrr map
#'
#' @returns a data frame
#' @noRd 
get_mm_comments <- function(pkg_name, db_name = golem::get_golem_options('assessment_db_name')) {
  dbSelect(
      "SELECT user_name, user_role, comment, added_on
       FROM comments
       WHERE id = {pkg_name} AND comment_type = 'mm'"
    , db_name
  ) %>%
    purrr::map(rev)
}


#' The 'Get Community Usage Metrics Comments' function
#' 
#' Retrieve the Community Metrics comments for a specific package
#' 
#' @param pkg_name character name of the package
#' @param db_name character name (and file path) of the database
#' 
#' @importFrom glue glue
#' @importFrom purrr map
#' 
#' @returns a data frame
#' @noRd
get_cm_comments <- function(pkg_name, db_name = golem::get_golem_options('assessment_db_name')) {
  dbSelect(
      "SELECT user_name, user_role, comment, added_on
       FROM comments
       WHERE id = {pkg_name} AND comment_type = 'cum'"
    , db_name
  ) %>%
    purrr::map(rev)
}

#' The 'Get Maintenance Metrics Data' function
#' 
#' Pull the maint metrics data for a specific package id, and create 
#' necessary columns for Cards UI
#' 
#' @param pkg_id integer package id
#' @param db_name character name (and file path) of the database
#' 
#' @import dplyr
#' @importFrom glue glue
#' 
#' @returns a data frame
#' @noRd
get_mm_data <- function(pkg_id, db_name = golem::get_golem_options('assessment_db_name')){
  dbSelect(
    "SELECT metric.name, metric.long_name, metric.description, metric.is_perc,
                    metric.is_url, package_metrics.value
                    FROM metric
                    INNER JOIN package_metrics ON metric.id = package_metrics.metric_id
                    WHERE package_metrics.package_id = {pkg_id} AND 
                    metric.class = 'maintenance' ;", db_name) %>%
    dplyr::mutate(
      title = long_name,
      desc = description,
      succ_icon = rep(x = 'check', times = nrow(.)), 
      unsucc_icon = rep(x = 'times', times = nrow(.)),
      icon_class = rep(x = 'text-success', times = nrow(.)),
      .keep = 'unused'
    )
}


#' The 'Get Community Data' function
#' 
#' Get all community metric data on a specific package
#' 
#' @param pkg_name character name of the package
#' @param db_name character name (and file path) of the database
#' 
#' @importFrom glue glue
#' 
#' @returns a data frame
#' @noRd
get_comm_data <- function(pkg_name, db_name = golem::get_golem_options('assessment_db_name')){
  dbSelect(
    "SELECT *
     FROM community_usage_metrics
     WHERE id = {pkg_name}"), db_name
  )
}

#' The 'Get Package Info' function
#' 
#' Get all general info on a specific package
#' 
#' @param pkg_name character name of the package
#' @param db_name character name (and file path) of the database
#' 
#' @importFrom glue glue
#' 
#' @returns a data frame
#' @noRd
get_pkg_info <- function(pkg_name, db_name = golem::get_golem_options('assessment_db_name')){
  dbSelect(
    "SELECT p.*, dc.decision
     FROM package p
     LEFT JOIN decision_categories dc
      ON p.decision_id = dc.id
     WHERE name = {pkg_name}", db_name
  )
}


#' get_metric_weights
#'
#' Retrieves metric name and current weight from metric table
#' 
#' @param db_name character name (and file path) of the database
#'
#' @returns a data frame
#' @noRd
get_metric_weights <- function(db_name = golem::get_golem_options('assessment_db_name')){
  dbSelect(
    "SELECT name, weight
     FROM metric", db_name
  )
}


##### End of get_* functions #####


#' weight_risk_comment
#'
#' Used to add a comment on every tab saying how the risk and weights changed,
#' and that the overall comment & final decision may no longer be applicable.
#' 
#' @param pkg_name character name of the package
#' @param db_name character name (and file path) of the database
#' @importFrom glue glue
#' 
#' @returns a data frame
#' @noRd
weight_risk_comment <- function(pkg_name, db_name = golem::get_golem_options('assessment_db_name')) {
  
  pkg_score <- dbSelect(
    "SELECT score
     FROM package
     WHERE name = {pkg_name}"
  , db_name)
  
  glue::glue('Metric re-weighting has occurred.
       The previous risk score was {pkg_score}.')
}

