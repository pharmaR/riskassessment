

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
#' @param decisions A named list containing the decision rules
#' 
#' @noRd
check_dec_rules <- function(decision_categories, decisions) {
  if (!all(names(decisions) %in% decision_categories))
    stop("All decision rule categories should be included in the list of decisions")
  
  if (length(names(decisions)) != length(unique(names(decisions))))
    stop("The decision categories must be unique for the decision rules")
  
  if (!all(purrr::map_lgl(decisions, ~ is.numeric(unlist(.x)))))
    stop("The rules must be numeric values")
  
  if (!all(purrr::map_lgl(decisions, ~ length(unlist(.x)) <= 2)))
    stop("At most two values can be provided for a decision rule")
  
  dec_lst <- unlist(decisions[decision_categories])
  if (!all(dec_lst >= 0 & dec_lst <= 1))
    stop("All rules must be between 0 and 1")
  
  if (!all(dec_lst == sort(dec_lst)))
    stop("The rules should be ascending in order of the categories")
  
  if (decision_categories[1] %in% names(decisions) & unlist(decisions[[decision_categories[1]]])[1] != 0)
    stop("Rules for the first decision category must have a lower bound of 0")
  
  if (decision_categories[length(decision_categories)] %in% names(decisions) & unlist(decisions[[decision_categories[length(decision_categories)]]])[2] != 1)
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
  
  if (all(!names(credentials_lst) %in% c("roles", "privileges")))
    stop("Both 'roles' and 'privileges' must be present in credentials configuration")
  
  if(length(credentials_lst$roles) != length(unique(credentials_lst$roles)))
    stop("The roles must be unique")
  
  if (is.null(credentials_lst$privileges[["admin"]]))
    stop("The roles corresponding to 'admin' privileges must be specified")
  
  privileges_roles <- unique(unlist(credentials_lst$privileges, use.names = FALSE))
  if (!all(privileges_roles %in% credentials_lst$roles))
    stop(glue::glue("The following role(s) designated under privileges is(are) not present in the 'roles' configuration: {paste(privileges_roles[!privileges_roles %in% credentials_lst$roles], collapse = ', ')}"))
}