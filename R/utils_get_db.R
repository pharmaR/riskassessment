
#' Select data from database
#' 
#' @param query a sql query as a string
#' @param db_name character name (and file path) of the database
#' @param .envir Environment to evaluate each expression in
#' 
#' @import dplyr
#' @importFrom DBI dbConnect dbSendQuery dbFetch dbClearResult dbDisconnect
#' @importFrom RSQLite SQLite
#' @importFrom loggit loggit
#' @importFrom glue glue glue_sql
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
      message <- glue::glue("warning:\n {query} \nresulted in\n {warn}")
      message(message, .loggit = FALSE)
      loggit::loggit("WARN", message, echo = FALSE)
      errFlag <<- TRUE
    },
    error = function(err) {
      message <- glue::glue("error:\n {query} \nresulted in\n {err}")
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

#' The 'Get Package Summary' function
#' 
#' Retrieves the package summary "comment" for a specific package
#' 
#' @param pkg_name character name of the package
#' @param db_name character name (and file path) of the database
#' 
#' @importFrom glue glue
#' 
#' @returns a data frame
#' @noRd
get_pkg_summary <- function(pkg_name, db_name = golem::get_golem_options('assessment_db_name')) {
  dbSelect(
    "SELECT * FROM comments 
     WHERE comment_type = 's' AND id = {pkg_name}", db_name
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

#' The 'Get Source Explorer Comments' function
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
get_se_comments <- function(pkg_name, db_name = golem::get_golem_options('assessment_db_name')) {
  dbSelect(
    "SELECT user_name, user_role, comment, added_on
       FROM comments
       WHERE id = {pkg_name} AND comment_type = 'se'"
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
     WHERE id = {pkg_name}", db_name
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
     FROM metric where name != 'suggests'", db_name
  )
}

#' Get Assessments
#'
#' Retrieves metric name and current weight from metric table
#' 
#' @param pkg_name character name of the package
#' @param db_name character name (and file path) of the database
#'
#' @returns a data frame
#' @noRd
get_assess_blob<- function(pkg_name, db_name = golem::get_golem_options('assessment_db_name')) {
  db_table <- dbSelect("SELECT metric.name, package_metrics.encode FROM package 
                       INNER JOIN package_metrics ON package.id = package_metrics.package_id
                       INNER JOIN metric ON package_metrics.metric_id = metric.id
                       WHERE package.name = {pkg_name}", 
                       db_name = db_name)
  
  purrr::pmap_dfc(db_table, function(name, encode) {dplyr::tibble(unserialize(encode)) %>% purrr::set_names(name)}) 
}


#' Get Dependency Pkg Versions and Scores
#'
#' 
#' @param pkg_name character name of the package
#' @param verify_data a data.frame used to verify whether a pkg exists in the db
#' @param cran_pkgs a data.frame containing all available cran package names/versions
#'
#' @returns a list
#' @noRd
get_versnScore <- function(pkg_name, verify_data, cran_pkgs) {
  
  if (pkg_name %in% verify_data$name) { #loaded2_db()$name
    tmp_df <- verify_data %>% filter(name == pkg_name) %>% select(score, version)
    pkg_score <- tmp_df %>% pull(score) %>% as.character
    pkg_versn <- tmp_df %>% pull(version) %>% as.character
  } else {
    pkg_score <- ""
    pkg_versn <- if_else(pkg_name %in% c(rownames(installed.packages(priority="base"))), "",
                 subset(cran_pkgs, Package == pkg_name, c("Version")) %>% as.character())
  } 
  
  return(list(name = pkg_name, version = pkg_versn, score = pkg_score))   
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

get_credential_config <- function(db_name = golem::get_golem_options('assessment_db_name')) {
  roles_tbl <- dbSelect("SELECT * FROM roles", db_name)
  config <- list()
  config[["roles"]] <- roles_tbl$user_role
  config[["privileges"]] <- purrr::map(roles_tbl$user_role, ~ 
                                         roles_tbl[roles_tbl$user_role == .x, used_privileges] %>% 
                                         unlist() %>% 
                                         `[`(. != 0) %>% 
                                         names()) %>%
    purrr::set_names(roles_tbl$user_role) %>%
    purrr::compact()
  config
}

get_roles_table <- function(db_name = golem::get_golem_options('assessment_db_name')) {
  dbSelect("SELECT * FROM roles", db_name = db_name) %>%
    {rownames(.) <- .$user_role; .} %>%
    dplyr::select(-id, -user_role) %>%
    t()
}

get_credentials_table <- function(db_name = golem::get_golem_options('credentials_db_name'), passphrase) {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_name)
  
  tbl <- shinymanager::read_db_decrypt(conn = con, name = "credentials", passphrase = passphrase)
  
  DBI::dbDisconnect(con)
  tbl
}

