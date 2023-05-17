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
  decision_id <- dbSelect("SELECT id FROM decision_categories WHERE decision = {decision}")
  if (decision != "") {
    dbUpdate("UPDATE package SET decision_id = {decision_id},
                        decision_by = 'Auto Assigned', decision_date = {Sys.Date()}
                         WHERE name = {package}")
    loggit::loggit("INFO",
                   glue::glue("decision for the package {package} was assigned {decision} by decision automation rules"))
    comment <- glue::glue("Decision was assigned '{decision}' by decision rules because the risk score was between {decision_list[[decision]][1]} and {decision_list[[decision]][2]}")
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

#' Get contrasting text color
#' 
#' Returns the color for the text based on provided background color
#' 
#' @param hex A string containing a hexidecimal
#' 
#' @return A hexidecimal corresponding to white or black
#' 
#' @noRd
get_text_color <- function(hex) {
  lum <-
    hex %>%
    stringr::str_remove("#") %>%
    substring(0:2*2+1,1:3*2) %>%
    strtoi(16) %>%
    `/`(255) %>%
    {ifelse(. <= 0.03928, ./12.92, ((.+0.055)/1.055)^2.4)}

  ifelse(lum[1]*0.2326 + lum[2]*0.6952 + lum[3]*0.0722 <= 0.22, "#ffffff", "#000000")
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
#' @importFrom stringr str_replace_all regex
#' 
#' @noRd
risk_lbl <- function(x, input = TRUE) {
  lbl <- x %>% tolower() %>% 
    paste("risk", .) %>%
    stringr::str_replace_all(" +", "_") %>%
    stringr::str_replace_all(stringr::regex("[^a-zA-Z0-9_-]"), "")
  
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
