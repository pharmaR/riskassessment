
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
dbSelect <- function(query, db_name = golem::get_golem_options('assessment_db_name'), .envir = parent.frame(), params = NULL){
  errFlag <- FALSE
  con <- DBI::dbConnect(RSQLite::SQLite(), db_name)
  
  tryCatch(
    expr = {
      rs <- DBI::dbSendQuery(con, glue::glue_sql(query, .envir = .envir, .con = con))
      if (!is.null(params))
        DBI::dbBind(rs, params)
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

#' The 'Get Function Explorer Comments' function
#' 
#' Retrieve the Function Explorer comments for a specific package
#' 
#' @param pkg_name character name of the package
#' @param db_name character name (and file path) of the database
#' 
#' @importFrom glue glue
#' @importFrom purrr map
#' 
#' @returns a data frame
#' @noRd
get_fe_comments <- function(pkg_name, db_name = golem::get_golem_options('assessment_db_name')) {
  dbSelect(
    "SELECT user_name, user_role, comment, added_on
       FROM comments
       WHERE id = {pkg_name} AND comment_type = 'fe'"
    , db_name
  ) %>%
    purrr::map(rev)
}

#' The 'Get Dependency Comments' function
#' 
#' Retrieve the Dependency comments for a specific package
#' 
#' @param pkg_name character name of the package
#' @param db_name character name (and file path) of the database
#' 
#' @importFrom glue glue
#' @importFrom purrr map
#' 
#' @returns a data frame
#' @noRd
get_dep_comments <- function(pkg_name, db_name = golem::get_golem_options('assessment_db_name')) {
  dbSelect(
    "SELECT user_name, user_role, comment, added_on
       FROM comments
       WHERE id = {pkg_name} AND comment_type = 'dep'"
    , db_name
  ) %>%
    purrr::map(rev)
}


#' The 'Get Maintenance Metrics Data' function
#' 
#' Pull the maint metrics data for a specific package id, and create 
#' necessary columns for Cards UI
#' 
#' @param pkg_name character name of package
#' @param metric_class character, corresponding to values desired from the metric table's metric_class var
#' @param db_name character name (and file path) of the database
#' 
#' @import dplyr
#' @importFrom glue glue
#' 
#' @returns a data frame
#' @noRd
get_metric_data <- function(pkg_name, metric_class = 'maintenance', db_name = golem::get_golem_options('assessment_db_name')){

  dbSelect(
    "SELECT metric.name, metric.long_name, metric.description, metric.is_perc,
                    metric.is_url, package_metrics.value, package_metrics.metric_score,
                    'information' as type
                    FROM metric
                    INNER JOIN package_metrics ON metric.id = package_metrics.metric_id
                    INNER JOIN package on package_metrics.package_id = package.id
                    WHERE package.name = {pkg_name} AND 
                    metric.class = {metric_class} ;", db_name) %>%
    dplyr::mutate(
      title = long_name,
      desc = description,
      score = metric_score,
      succ_icon = rep(x = 'check', times = nrow(.)), 
      unsucc_icon = rep(x = 'times', times = nrow(.)),
      icon_class = rep(x = 'text-success', times = nrow(.)),
      .keep = 'unused'
    )
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
  
  if (rlang::is_empty(pkg_name)) 
    return(list(name = character(), version = character(), score = character(),
                decision_id = character(), decision = character()))
  
  if (pkg_name %in% verify_data$name) { #loaded2_db()$name
    tmp_df <- verify_data %>% filter(name == pkg_name) %>% select(score, version, decision_id, decision)
    pkg_score <- tmp_df %>% pull(score) %>% as.character
    pkg_versn <- tmp_df %>% pull(version) %>% as.character
    pkg_decision_id <- tmp_df %>% pull(decision_id) %>% as.character
    pkg_decision <- tmp_df %>% pull(decision) %>% as.character
  } else {
    pkg_score <- ""
    pkg_versn <- if_else(pkg_name %in% c(rownames(installed.packages(priority="base"))), "",
                         subset(cran_pkgs, Package == pkg_name, c("Version")) %>% as.character())
    pkg_decision_id <- ""
    pkg_decision <- ""
  } 
  
  return(list(name = pkg_name, version = pkg_versn, score = pkg_score,
              decision_id = pkg_decision_id, decision = pkg_decision
  ))   
}



#' The 'Get Dependencies Metrics Data' function
#' 
#' Pull the depenencies data for a specific package id, and create 
#' necessary columns for Cards UI
#' 
#' @param pkg_name character name of package
#' @param db_name character name (and file path) of the database
#' @param loaded2_db a data.frame containing variables: name, version, score, decision_id, decision
#' @param repo_pkgs a data.frame containing variables: Package & Version, defaulting to output from available.packages()
#' 
#' @import dplyr
#' @importFrom stringr str_replace
#' 
#' @returns a data frame with package, type, and name
#' @noRd
get_depends_data <- function(pkg_name,
                             suggests,
                             db_name = golem::get_golem_options('assessment_db_name'),
                             loaded2_db = dplyr::tibble(package = character(0), type = character(0), name = character(0),
                                                        version = character(0), score = character(0), decision_id = character(0),
                                                        decision = character(0)
                                                        ),
                             repo_pkgs = as.data.frame(utils::available.packages()[,1:2])){
  
  pkgref <- get_assess_blob(pkg_name, db_name, metric_lst = c("dependencies", "suggests"))
  
  if(suppressWarnings(is.null(nrow(pkgref$dependencies[[1]])) || nrow(pkgref$dependencies[[1]]) == 0)) {
    deps <- dplyr::tibble(package = character(0), type = character(0), name = character(0),
                          version = character(0), score = character(0), decision = character(0),
                          decision_id = character(0))
  } else {
    deep_ends <- pkgref$dependencies[[1]] %>% dplyr::as_tibble() %>% 
      mutate(package = stringr::str_replace(package, "\n", " ")) %>%
      mutate(name = stringr::str_extract(package, "^((([[A-z]]|[.][._[A-z]])[._[A-z0-9]]*)|[.])")) 
    
    deps_decision_data <- purrr::map_df(deep_ends$name, ~get_versnScore(.x, loaded2_db, repo_pkgs))
    if(nrow(deps_decision_data) == 0) {
      deps_w_decision <- dplyr::tibble(name = character(0), version = character(0),
             score = character(0), decision = character(0), decision_id = character(0))
    } else {
      deps_w_decision <- deps_decision_data
    }
    deps <- deps_w_decision %>%
      right_join(deep_ends, by = "name") %>% 
      select(package, type, name, version, score, decision, decision_id) %>%
      arrange(name, type) %>% 
      distinct()
  }
  
  if(isTruthy(suggests)) {
    if(suppressWarnings(is.null(nrow(pkgref$suggests[[1]])) || nrow(pkgref$suggests[[1]]) == 0)) {
      sugg <- dplyr::tibble(package = character(0), type = character(0), name = character(0),
                            version = character(0), score = character(0), decision = character(0),
                            decision_id = character(0))
    } else {
      shrug_jests <- pkgref$suggests[[1]] %>% dplyr::as_tibble() %>% 
        mutate(package = stringr::str_replace(package, "\n", " ")) %>%
        mutate(name = stringr::str_extract(package, "^((([[A-z]]|[.][._[A-z]])[._[A-z0-9]]*)|[.])")) 
      
      sugg_decision_data <- purrr::map_df(shrug_jests$name, ~get_versnScore(.x, loaded2_db, repo_pkgs))
      if(nrow(sugg_decision_data) == 0) {
        suggs_w_decision <- dplyr::tibble(name = character(0), version = character(0),
                                          score = character(0), decision = character(0), decision_id = character(0))
      } else {
        suggs_w_decision <- sugg_decision_data
      }
      sugg <- suggs_w_decision %>%
        right_join(shrug_jests, by = "name") %>% 
        select(package, type, name, version, score, decision, decision_id) %>%
        arrange(name, type) %>% 
        distinct()
    }
    return(bind_rows(deps, sugg))
  } else {
    return(deps) }
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
#' @param pkg_lst character name of the package
#' @param db_name character name (and file path) of the database
#' @param metric_lst character name of metric
#'
#' @returns a data frame
#' @noRd
#' 
#' @import dplyr
#' @importFrom purrr map pmap_dfc reduce
get_assess_blob <- function(pkg_lst, db_name = golem::get_golem_options('assessment_db_name'),
                            metric_lst = NA) {
  if (length(pkg_lst) == 0) return(dplyr::tibble(name = character()))
  
  db_table <- dbSelect("SELECT package.name, metric.name metric, package_metrics.encode FROM package 
                       INNER JOIN package_metrics ON package.id = package_metrics.package_id
                       INNER JOIN metric ON package_metrics.metric_id = metric.id
                       WHERE package.name = $pkg_name AND metric.name = COALESCE($metric_name, metric.name)", 
                       db_name = db_name,
                       params = list(pkg_name = rep(pkg_lst, each = length(metric_lst)),
                                     metric_name = rep(metric_lst, length(pkg_lst))))
  
  # This approach was used to avoid adding a dependency on tidyr to use pivot_wider
  purrr::map(pkg_lst, ~ 
               db_table %>% 
               dplyr::filter(name == .x) %>% 
               purrr::pmap_dfc(function(name, metric, encode) {dplyr::tibble(!!metric := unserialize(encode))}) %>%
               dplyr::mutate(name = .x, .before = 0)) %>%
    purrr::reduce(dplyr::bind_rows)
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

