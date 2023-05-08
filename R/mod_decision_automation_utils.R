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
  decision_id <- dbSelect(glue::glue("SELECT id FROM decision_categories WHERE decision = '{decision}'"))
  if (decision != "") {
    dbUpdate("UPDATE package SET decision_id = {decision_id},
                        decision_by = 'Auto Assigned', decision_date = {Sys.Date()}
                         WHERE name = {package}")
    loggit::loggit("INFO",
                   glue::glue("decision for the package {package} was assigned {decision} by decision automation rules"))
    comment <- glue::glue("Decision was assigned ''{decision}'' by decision rules because the risk score was between {decision_list[[decision]][1]} and {decision_list[[decision]][2]}")
    dbUpdate(
      "INSERT INTO comments
          VALUES ({package}, 'Auto Assigned', 'admin',
          {comment}, 'o', {getTimeStamp()})")
  }
  
  return(decision)
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
get_colors <- function(dbname) {
  dbname %>%
    dbSelect(query = "SELECT decision, color FROM decision_categories") %>%
    purrr::pmap(function(decision, color) {setNames(color, decision)}) %>% 
    unlist()
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
  if (is.null(db_name))
    return(list())
  
  dec_tbl <- dbSelect("SELECT * FROM decision_categories", db_name)
  dec_tbl %>%
    purrr::pmap(function(lower_limit, upper_limit, ...) {c(lower_limit, upper_limit)}) %>% 
    purrr::set_names(dec_tbl$decision) %>%
    purrr::map(purrr::discard, is.na) %>%
    purrr::compact()
}
