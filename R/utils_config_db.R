

#' Set colors for decision categories
#' 
#' Gets the correct color palette based on the number of decision categories
#' 
#' @param decision_categories A vector containing the decision categories
#' 
#' @return A vector of colors for displaying the decision categories
#' 
#' @noRd
set_colors <- function(decision_categories) {
  num_cat <- length(decision_categories)
  if (num_cat == 1)
    return(color_palette[1])
  cat_list <- (seq_along(decision_categories) - 1) * 10/min(num_cat - 1, 11) + 1
  color_palette[round(purrr::map_dbl(cat_list, min, 11))] %>% purrr::set_names(decision_categories)
}

configure_db <- function(dbname, config) {
  if (missing(config)) config <- get_golem_config(NULL, file = app_sys("db-config.yml"))

  check_decision_config(config[["decisions"]])
  
  # Set decision categories
  purrr::walk(config[["decisions"]]$categories, ~ dbUpdate("INSERT INTO decision_categories (decision) VALUES ({.x})", dbname))
  
  # Set decision rules
  if (!is.null(config[["decisions"]]$rules)) 
    purrr::iwalk(config[["decisions"]]$rules, ~ dbUpdate("UPDATE decision_categories SET lower_limit = {.x[1]}, upper_limit = {.x[length(.x)]} WHERE decision = {.y}", dbname))
  else
    message("No decision rules applied from configuration")
  
  # Set decision category colors
  col_lst <- set_colors(config[["decisions"]]$categories)
  purrr::iwalk(config[["decisions"]]$colors, ~ {col_lst[.y] <<- .x})
  purrr::iwalk(col_lst, ~ dbUpdate("UPDATE decision_categories SET color = {.x} WHERE decision = {.y}", dbname))
  
  if (!is.null(config[["metric_weights"]]))
    check_metric_weights(config[["metric_weights"]])
  
  # Set metric weights
  if (!is.null(config[["metric_weights"]]))
    purrr::iwalk(config[["metric_weights"]], ~ if (!is.null(.x)) dbUpdate("UPDATE metric SET weight = {.x} WHERE name = {.y}", dbname))
}

check_decision_config <- function(dec_config) {
  check_dec_cat(dec_config$categories)
  
  if (!is.null(dec_config$rules))
    check_dec_rules(dec_config$categories, dec_config$rules)
}

#' Check decision categories
#' 
#' Checks that the decision categories supplied by the configuration file are valid
#' 
#' @param decision_categories A vector containing the decision categories
#' 
#' @noRd
check_dec_cat <- function(decision_categories) {
  if (!(length(decision_categories) > 1))
    stop("The number of decision categories must be at least 2")
  
  if (length(decision_categories) != length(unique(decision_categories)))
    stop("The decision categories must be unique")
}

#' Check decision rules
#' 
#' Checks that the decision rules supplied by the configuration file are valid
#' 
#' @param decision_categories A vector containing the decision categories
#' @param decision_rules A named list containing the decision rules
#' 
#' @noRd
check_dec_rules <- function(decision_categories, decision_rules) {
  if (!all(names(decision_rules) %in% decision_categories))
    stop("All decision rule categories should be included in the list of decision categories")
  
  if (length(names(decision_rules)) != length(unique(names(decision_rules))))
    stop("The decision categories must be unique for the decision rules")
  
  if (!all(purrr::map_lgl(decision_rules, ~ is.numeric(unlist(.x)))))
    stop("The rules must be numeric values")
  
  if (!all(purrr::map_lgl(decision_rules, ~ length(unlist(.x)) <= 2)))
    stop("At most two values can be provided for a decision rule")
  
  dec_lst <- unlist(decision_rules[decision_categories])
  if (!all(dec_lst >= 0 & dec_lst <= 1))
    stop("All rules must be between 0 and 1")
  
  if (!all(dec_lst == sort(dec_lst)))
    stop("The rules should be ascending in order of the categories")
  
  if (decision_categories[1] %in% names(decision_rules) && unlist(decision_rules[[decision_categories[1]]])[1] != 0)
    stop("Rules for the first decision category must have a lower bound of 0")
  
  if (decision_categories[length(decision_categories)] %in% names(decision_rules) && unlist(decision_rules[[decision_categories[length(decision_categories)]]])[2] != 1)
    stop("Rules for the last decision category must have an upper bound of 1")
}

#' Check metric weights
#' 
#' Checks that the metric weights supplied by the configuration file are valid
#' 
#' @param metric_weights A vector containing the metric weights
#' 
#' @noRd
check_metric_weights <- function(metric_weights) {
  allowed_lst <- c('has_vignettes', 'has_news', 'news_current', 'has_bug_reports_url', 'has_website', 'has_maintainer', 'has_source_control', 'export_help', 'bugs_status', 'license', 'covr_coverage', 'downloads_1yr')
  if (!all(names(metric_weights) %in% allowed_lst))
    stop(glue::glue("The metric weights must be a subset of the following: {paste(allowed_lst, collapse = ', ')}"))
  
  if (length(names(metric_weights)) != length(unique(names(metric_weights))))
    stop("The metric weights must be unique")
  
  if (!all(purrr::map_lgl(metric_weights, ~ is.null(.x) || is.numeric(.x) && length(.x) == 1 && .x >= 0)))
    stop("The weights must be single, non-negative, numeric values")
}

#' Check credential configuration file
#' 
#' Checks that the credentials design supplied by the configuration file are valid
#' 
#' @param credentials_lst A list containing the design of the credentials database and privileges
#' 
#' @noRd
check_credentials <- function(credentials_lst) {
  if (is.null(credentials_lst) )
    stop("No credentials configuration found in db-config.yml")  
  
  if (!all(c("roles", "privileges") %in% names(credentials_lst)))
    stop("Both 'roles' and 'privileges' must be present in credentials configuration")
  
  if(length(credentials_lst$roles) != length(unique(credentials_lst$roles)))
    stop("The roles must be unique")
  
  privileges_roles <- names(credentials_lst$privileges)
  privileges <- unique(unlist(credentials_lst$privileges, use.names = FALSE))
  if (!"admin" %in% privileges)
    stop("The roles corresponding to 'admin' privileges must be specified")
  
  if (!all(privileges_roles %in% credentials_lst$roles))
    warning(glue::glue("The following role(s) designated under privileges is(are) not present in the 'roles' configuration: {paste(privileges_roles[!privileges_roles %in% credentials_lst$roles], collapse = ', ')}"))
  
  valid_privileges <- unique(unlist(credentials_lst$privileges[credentials_lst$roles], use.names = FALSE))
  if (!all(used_privileges %in% valid_privileges))
    warning(glue::glue("The following privilege(s) is(are) not assigned to any 'role' in the credentials configuration: {paste(used_privileges[!used_privileges %in% valid_privileges], collapse = ', ')}"))
}
