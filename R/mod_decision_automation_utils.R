#' Assign decision rules
#' 
#' Automates the decision for a package based upon the provided rules
#' 
#' @param decision_list A named list containing the lower and upper bounds for the risk
#' @param package A character string of the name of the package
#' 
#' @return A character string of the decision made
#' 
#' @noRd
assign_decisions <- function(decision_list, package) {
  score <- get_pkg_info(package)$score
  decision <- paste0(names(decision_list)[purrr::map_lgl(decision_list, ~ .x[1] < score && score <= .x[2])], "")
  if (decision != "") {
    dbUpdate(glue::glue("UPDATE package SET decision = '{decision}' WHERE name = '{package}'"))
    loggit::loggit("INFO",
                   glue::glue("decision for the package {package} was assigned {decision} by decision automation rules"))
    comment <- glue::glue("Decision was assigned ''{decision}'' by decision rules because the risk score was between {decision_list[[decision]][1]} and {decision_list[[decision]][2]}")
    dbUpdate(glue::glue(
      "INSERT INTO comments
          VALUES ('{package}', 'auto_assign', 'admin',
          '{comment}', 'o', '{getTimeStamp()}')"))
  }
  
  return(decision)
}

#' Check decision categories
#' 
#' Checks that the decision categories supplied by the configuration file are valid
#' 
#' @param decision_categories A vector containing the decision categories
#' 
#' @noRd
check_dec_cat <- function(decision_categories) {
  if (!(length(decision_categories) > 0))
    stop("The number of decision categories must be at least 1")
  
  if (!all.equal(decision_categories, unique(decision_categories)))
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
  
  if (!all.equal(names(decisions), unique(names(decisions))))
    stop("The decision categories must be unique for the decision rules")
  
  if (!all(purrr::map_lgl(decisions, ~ is.numeric(unlist(.x)))))
    stop("The rules must be numeric values")
  
  if (!all(purrr::map_lgl(decisions, ~ length(unlist(.x)) <= 2)))
    stop("At most two values can be provided for a decision rule")
  
  dec_lst <- unlist(decisions[decision_categories])
  if (!all(dec_lst >= 0 & dec_lst <= 1))
    stop("All rules must be between 0 and 1")
  
  if (!all.equal(dec_lst, sort(dec_lst)))
    stop("The rules should be ascending in order of the categories")
}

#' Get colors for decision categories
#' 
#' Gets the correct color palette based on the number of decision categories
#' 
#' @param decision_categories A vector containing the decision categories
#' 
#' @return A vector of colors for displaying the decision categories
#' 
#' @noRd
get_colors <- function(decision_categories) {
  num_cat <- length(decision_categories)
  if (num_cat == 1)
    return(color_palette[1])
  cat_list <- (seq_along(decision_categories) - 1) * 10/min(num_cat - 1, 11) + 1
  color_palette[round(purrr::map_dbl(cat_list, min, 11))]
}


#' Create risk decision label
#' 
#' Creates HTML friendly labels for the decision categories
#' 
#' @param x A character string containing the decision category
#' @param input A logical indicating whether to return an input ID for the category
#' 
#' @return A character string containing the generated label
#' 
#' @noRd
risk_lbl <- function(x, input = TRUE) {
  lbl <- x %>% tolower() %>% stringr::str_replace_all(" +", "_")
  
  if (input)
    paste(lbl, "attr", sep = "_")
  else
    lbl
}

#' Process decision category table
#' 
#' Process the decision category table from the assessment database for use within the application
#' 
#' @param db_name character name (and file path) of the assessment database
#' 
#' @return A named list containing the lower and upper bounds for the risk
#' 
#' @noRd
process_dec_tbl <- function(db_name = golem::get_golem_options('assessment_db_name')) {
  dec_tbl <- dbSelect("SELECT * FROM decision_categories", db_name)
  dec_tbl %>%
    purrr::pmap(function(lower_limit, upper_limit, ...) {list(lower_limit, upper_limit)}) %>% 
    purrr::set_names(dec_tbl$decision) %>%
    purrr::map(purrr::discard, is.na) %>%
    purrr::compact()
}
