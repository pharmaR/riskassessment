

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
  if (missing(config)) dec_config <- get_golem_config('decisions', file = app_sys("db-config.yml"))
  
  check_decision_config(dec_config)
  
  dbUpdate(glue::glue("INSERT INTO decision_categories (decision) VALUES {paste0('(\\'', dec_config$categories, '\\')', collapse = ', ')}"), dbname)
  if (!is.null(dec_config$rules)) 
    purrr::iwalk(dec_config$rules, ~ dbUpdate("UPDATE decision_categories SET lower_limit = {.x[1]}, upper_limit = {.x[length(.x)]} WHERE decision = {.y}", dbname))
  else
    message("No decision rules applied from configuration")
  col_lst <- set_colors(dec_config$categories)
  purrr::iwalk(dec_config$colors, ~ {col_lst[.y] <<- .x})
  purrr::iwalk(col_lst, ~ dbUpdate("UPDATE decision_categories SET color = {.x} WHERE decision = {.y}", dbname))
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
  
  if (is.null(credentials_lst$privileges[["admin"]]))
    stop("The roles corresponding to 'admin' privileges must be specified")
  
  privileges_roles <- unique(unlist(credentials_lst$privileges, use.names = FALSE))
  if (!all(privileges_roles %in% credentials_lst$roles))
    stop(glue::glue("The following role(s) designated under privileges is(are) not present in the 'roles' configuration: {paste(privileges_roles[!privileges_roles %in% credentials_lst$roles], collapse = ', ')}"))
}