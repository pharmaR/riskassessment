#' Assign decision rules
#' 
#' @noRd
assign_decisions <- function(decision_list, package) {
  score <- get_pkg_info(package)$score
  decision <- paste0(names(decision_list)[purrr::map_lgl(decision_list, ~ .x[1] <= score && score < .x[2])], "")
  if (decision != "") {
    dbUpdate(glue::glue("UPDATE package SET decision = '{decision}' WHERE name = '{package}'"))
    loggit::loggit("INFO",
                   glue::glue("decision for the package {package} was assigned {decision} by decision automation rules"))
    comment <- glue::glue("Risk was assigned '{decision}' by decision rules because the risk score was between {decision_list[[decision]][1]} and {decision_list[[decision]][2]}")
    dbUpdate(
      glue::glue(
        "UPDATE comments
          SET comment = '{comment}', added_on = '{getTimeStamp()}'
          WHERE id = '{package}' AND
          comment_type = 'o'"
      ))
  }
  
  return(decision)
}