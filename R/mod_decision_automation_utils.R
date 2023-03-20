#' Assign decision rules
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

check_dec_cat <- function(decision_categories) {
  if (!(length(decision_categories) > 0))
    stop("The number of decision categories must be at least 1")
  
  if (!all.equal(decision_categories, unique(decision_categories)))
    stop("The decision categories must be unique")
}

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

get_colors <- function(decision_categories) {
  num_cat <- length(decision_categories)
  if (num_cat == 1)
    return(color_palette[1])
  cat_list <- (seq_along(decision_categories) - 1) * 10/min(num_cat - 1, 11) + 1
  color_palette[round(purrr::map_dbl(cat_list, min, 11))]
}

risk_lbl <- function(x, input = TRUE) {
  lbl <- x %>% tolower() %>% stringr::str_replace_all(" +", "_")
  
  if (input)
    paste(lbl, "attr", sep = "_")
  else
    lbl
}

process_dec_tbl <- function(db_name = golem::get_golem_options('assessment_db_name')) {
  dec_tbl <- dbSelect("SELECT * FROM decision_categories", db_name)
  dec_tbl %>%
    purrr::pmap(function(lower_limit, upper_limit, ...) {list(lower_limit, upper_limit)}) %>% 
    purrr::set_names(dec_tbl$decision) %>%
    purrr::map(purrr::discard, is.na) %>%
    purrr::compact()
}
