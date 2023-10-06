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

#' Configure database
#' 
#' Utilizes a configuration file to configure a database
#' 
#' @param dbname name of the database
#' @param config a list of configurations
#' 
#' @noRd
configure_db <- function(dbname, config) {
  if (missing(config)) config <- get_db_config(NULL)

  # Perform checks before configuring database
  check_decision_config(config[["decisions"]])
  if (!is.null(config[["metric_weights"]]))
    check_metric_weights(config[["metric_weights"]])
  check_credentials(config[["credentials"]])
  
  # Set decision categories
  purrr::walk(config[["decisions"]]$categories, ~ dbUpdate("INSERT INTO decision_categories (decision) VALUES ({.x})", dbname))
  
  # Set decision rules
  if (!is.null(config[["decisions"]]$rules)) {
    dec_rules <- parse_rules(config[["decisions"]])
    purrr::iwalk(dec_rules, ~ {
      if (.y %in% config[["decisions"]]$categories) {
      dbUpdate("UPDATE decision_categories SET lower_limit = {.x[1]}, upper_limit = {.x[length(.x)]} WHERE decision = {.y}", dbname)
      dbUpdate("INSERT INTO rules (rule_type, condition, decision_id) VALUES ('overall_score', '~ {.x[1]} <= .x & .x <= {.x[length(.x)]}', {match(.y, config[['decisions']]$categories)});", dbname)
      } else if (grepl("^rule_\\d+$", .y)) {
        dbUpdate("INSERT INTO rules (rule_type, metric_id, condition, decision_id) VALUES ('assessment', {.x$metric_id}, {.x$condition}, {.x$decision_id})", dbname)
      } else if (.y == "rule_else") {
        dbUpdate("INSERT INTO rules (rule_type, condition, decision_id) VALUES ('else', 'ELSE', {.x$decision_id})", dbname)
      }
      })
    
  } else {
    message("No decision rules applied from configuration")
  }
  
  # Set decision category colors
  col_lst <- set_colors(config[["decisions"]]$categories)
  purrr::iwalk(config[["decisions"]]$colors, ~ {col_lst[.y] <<- .x})
  purrr::iwalk(col_lst, ~ dbUpdate("UPDATE decision_categories SET color = {.x} WHERE decision = {.y}", dbname))
  
  # Set metric weights
  if (!is.null(config[["metric_weights"]]))
    purrr::iwalk(config[["metric_weights"]], ~ if (!is.null(.x)) dbUpdate("UPDATE metric SET weight = {.x} WHERE name = {.y}", dbname))
  
  # Set user roles
  purrr::walk(config[["credentials"]]$roles, ~ dbUpdate("INSERT INTO roles (user_role) VALUES ({.x})", dbname))
  
  # Set privileges
  update_statements <- purrr::imap(config[["credentials"]]$privileges, ~ if (.y %in% config[["credentials"]]$roles) glue::glue("UPDATE roles SET {.x} = 1 WHERE user_role = '{.y}'")) %>%
    unlist(use.names = FALSE)
  purrr::iwalk(update_statements, ~ dbUpdate(.x, dbname))
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
  
  config_active <- Sys.getenv("GOLEM_CONFIG_ACTIVE", Sys.getenv("R_CONFIG_ACTIVE", "default"))
  decision_categories_combined <- 
    if (config_active != "default") unique(c(decision_categories, get_db_config("decisions", "default")[["categories"]])) else decision_categories
  
  if (!all(names(decision_rules) %in% decision_categories_combined | grepl("^rule_\\d+$|^rule_else$", names(decision_rules))))
    stop("All decision rules should be either named after a decision category or following the convention `rule_{d}`")
  
  if (length(names(decision_rules)) != length(unique(names(decision_rules))))
    stop("The rule names must be unique")
  
  if (any(names(decision_rules) %in% decision_categories)) {
    dec_cat_rules <- decision_rules[names(decision_rules) %in% decision_categories]
  
  if (!all(purrr::map_lgl(dec_cat_rules, ~ is.numeric(unlist(.x)))))
    stop("The rules must be numeric values")
  
  if (!all(purrr::map_lgl(dec_cat_rules, ~ length(unlist(.x)) <= 2)))
    stop("At most two values can be provided for a decision rule")
  
  dec_lst <- unlist(dec_cat_rules[decision_categories])
  if (!all(dec_lst >= 0 & dec_lst <= 1))
    stop("All rules must be between 0 and 1")
  
  if (!all(dec_lst == sort(dec_lst)))
    stop("The rules should be ascending in order of the categories")
  
  if (decision_categories[1] %in% names(dec_cat_rules) && unlist(decision_rules[[decision_categories[1]]])[1] != 0)
    stop("Rules for the first decision category must have a lower bound of 0")
  
  if (decision_categories[length(decision_categories)] %in% names(decision_rules) && unlist(decision_rules[[decision_categories[length(decision_categories)]]])[2] != 1)
    stop("Rules for the last decision category must have an upper bound of 1")
  }
  
  if (any(grepl("^rule_\\d$", names(decision_rules)))) {
    dec_metric_rules <- decision_rules[grepl("^rule_\\d$", names(decision_rules))]
    
    if (!all(purrr::map(dec_metric_rules, ~ c("metric", "condition", "decision") %in% names(.x)) %>% unlist(use.names = FALSE)))
      stop("Rules for metrics must contain the following three elements: 'metric', 'condition', & 'decision'")

    if (!all(purrr::map_chr(dec_metric_rules, ~ as.character(.x$metric)) %in% metric_lst))
      stop("Rules for metrics must have a valid value for the 'metric' element: ", paste(metric_lst, collapse = ", "))
    
    mappers <- purrr::map(dec_metric_rules, ~ evalSetTimeLimit(parse(text = .x$condition))) %>%
      purrr::map_lgl(~ rlang::is_formula(.x) || rlang::is_function(.x))
    if (!all(mappers))
      stop("Rules for metrics must have a valid formula or function for the 'condition' element")
    
    if (!all(purrr::map_chr(dec_metric_rules, ~ as.character(.x$decision)) %in% decision_categories_combined))
      stop("Rules for metrics must have a valid value for the 'decision' element: ", paste(decision_categories, collapse = ", "))
  }
  
  if (any(names(decision_rules) == "rule_else")) {
    dec_else_rule <- decision_rules[["rule_else"]]
    
    if (!"decision" %in% names(dec_else_rule))
      stop("The ELSE condition rule must have a 'decision' element")
    
    if (!as.character(dec_else_rule$decision) %in% decision_categories_combined)
      stop("The ELSE condition rule must have a valid value for the 'decision' element", paste(decision_categories, collapse = ", "))
  }
}

#' Check metric weights
#' 
#' Checks that the metric weights supplied by the configuration file are valid
#' 
#' @param metric_weights A vector containing the metric weights
#' 
#' @noRd
check_metric_weights <- function(metric_weights) {
  config_active <- Sys.getenv("GOLEM_CONFIG_ACTIVE", Sys.getenv("R_CONFIG_ACTIVE", "default"))
  
  if (!all(names(metric_weights) %in% metric_lst))
    stop(glue::glue("The metric weights must be a subset of the following: {paste(metric_lst, collapse = ', ')}"))
  
  if (length(names(metric_weights)) != length(unique(names(metric_weights))))
    stop("The metric weights must be unique")
  
  if (!all(purrr::map_lgl(metric_weights, ~ is.null(.x) || is.numeric(.x) && length(.x) == 1 && .x >= 0)))
    stop("The weights must be single, non-negative, numeric values")
  
  if (config_active != "default") {
    default_config <- get_db_config("metric_weights", "default")
    common_weights <- intersect(default_config, metric_weights)
    if (length(common_weights) > 0) {
      warning(glue::glue("The following weights were applied from the default configuration:\n{purrr::imap_chr(common_weights, ~ paste(.y, .x, sep = ': ')) %>% paste(collapse = '\n')}"))
    }
  }
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

parse_rules <- function(dec_config) {
  config_active <- Sys.getenv("GOLEM_CONFIG_ACTIVE", Sys.getenv("R_CONFIG_ACTIVE", "default"))
  
  if (config_active != "default") {
    default_config <- get_db_config("decisions", "default")[["rules"]]
    common_rules <- intersect(default_config, dec_config[["rules"]])
    if (length(common_rules) > 0) {
      warning(glue::glue("The following rules were applied from the default configuration:\n{purrr::imap_chr(common_rules, ~ paste(.y, .x, sep = ': ')) %>% paste(collapse = '\n')}"))
    }
  }
  
  rule_lst <- dec_config[["rules"]] %>%
    `[`(names(.) %in% dec_config[["categories"]] | grepl("^rule_\\d+$|^rule_else$", names(.))) %>%
    purrr::imap( ~ if (.y %in% dec_config[["categories"]]) {
      .x
    } else if (.x$decision %in% dec_config[["categories"]]) {
      .x$decision_id <- match(.x$decision, dec_config[["categories"]])
      .x$metric_id <- match(.x$metric, metric_lst)
      .x
    }) %>%
    purrr::compact()
  
  rule_lst
}
