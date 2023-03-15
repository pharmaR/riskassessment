#' Assign decision rules
#' 
#' @noRd
assign_decisions <- function(decision_list, package) {
  score <- get_pkg_info(package)$score
  decision <- paste0(names(decision_list)[purrr::map_lgl(decision_list, ~ .x[1] < score && score <= .x[2])], "")
  if (decision != "") {
    dbUpdate(glue::glue("UPDATE package SET decision = '{decision}',
                        decision_by = 'auto_assign', decision_date = '{Sys.Date()}'
                        WHERE name = '{package}'"))
    loggit::loggit("INFO",
                   glue::glue("decision for the package {package} was assigned {decision} by decision automation rules"))
    comment <- glue::glue("Risk was assigned ''{decision}'' by decision rules because the risk score was between {decision_list[[decision]][1]} and {decision_list[[decision]][2]}")
    dbUpdate(glue::glue(
      "INSERT INTO comments
          VALUES ('{package}', 'auto_assign', 'admin',
          '{comment}', 'o', '{getTimeStamp()}')"))
  }
  
  return(decision)
}