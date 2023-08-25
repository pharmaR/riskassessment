#' Assign decision rules based on risk score
#' 
#' Automates the decision for a package based upon the provided rules
#' 
#' @param rule_list A named list containing the decision rules
#' @param package A character string of the name of the package
#' 
#' @return A character string of the decision made
#' 
#' @noRd
#' 
#' @importFrom purrr map_lgl possibly
#' @importFrom rlang is_function is_formula
#' @importFrom glue glue
#' @importFrom loggit loggit
assign_decisions <- function(rule_list, package) {
  decision <- ""
  if (any(purrr::map_lgl(rule_list, ~ !is.na(.x$metric))))
    assessments <- get_assess_blob(package)
  
  for (rule in rule_list) {
    if (decision != "") break
    
    fn <- purrr::possibly(rule$mapper, otherwise = FALSE)
    if (is.na(rule$metric)) {
      decision <- if (fn(get_pkg_info(package)$score)) rule$decision else ""
      measure <- "Risk Score"
    } else if (rlang::is_function(rule$mapper) | rlang::is_formula(rule$mapper)) {
      decision <- if (fn(assessments[[rule$metric]][[1]])) rule$decision else ""
      measure <- glue::glue("{rule$metric} assessment")
    } else {
      warning(glue::glue("Unable to apply rule for {rule$metric}."))
      decision <- ""
    }
    if (decision == "") next
    
    decision_id <- dbSelect("SELECT id FROM decision_categories WHERE decision = {decision}")
    dbUpdate("UPDATE package SET decision_id = {decision_id},
                        decision_by = 'Auto Assigned', decision_date = {get_Date()}
                         WHERE name = {package}")
    loggit::loggit("INFO",
                   glue::glue("decision for the package {package} was assigned {decision} by decision automation rules"))
    comment <- glue::glue("Decision was assigned '{decision}' by decision rules because the {measure} returned TRUE for `{rule$filter}`")
    dbUpdate(
      "INSERT INTO comments
          VALUES ({package}, 'Auto Assigned', 'admin',
          {comment}, 'o', {getTimeStamp()})")
  }
  
  decision
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
risk_lbl <- function(x, type = c("input", "attribute", "module")) {
  type <- match.arg(type)
  lbl <- x %>% tolower() %>% 
    paste("cat", .) %>%
    stringr::str_replace_all(" +", "_") %>%
    stringr::str_replace_all(stringr::regex("[^a-zA-Z0-9_-]"), "")
  
  switch(
    type,
    input = lbl,
    attribute = paste(lbl, "attr", sep = "_"),
    module = paste(lbl, "mod", sep = "_")
  )
}

#' Process decision category table
#'
#' Process the decision category table from the assessment database for use
#' within the application
#'
#' @param db_name character name (and file path) of the assessment database
#'
#' @return A named list containing the lower and upper bounds for the risk
#'
#' @noRd
#' 
#' @importFrom purrr pmap set_names map compact
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

#' Process decision rules table
#'
#' Process the decision rules table from the assessment database for use within
#' the application
#'
#' @param db_name character name (and file path) of the assessment database
#'
#' @return A named list containing the ordered decision rules
#'
#' @noRd
#' 
#' @importFrom purrr pmap set_names map_chr
process_rule_tbl <- function(db_name = golem::get_golem_options('assessment_db_name')) {
  if (is.null(db_name))
    return(list())
  
   rule_tbl <- dbSelect("SELECT m.name metric, r.filter, d.decision FROM rules r LEFT JOIN metric m ON r.metric_id = m.id LEFT JOIN decision_categories d ON r.decision_id = d.id", db_name)
   rule_tbl %>%
     purrr::pmap(~ {
       out <- list(...) %>%
         within(mapper <- evalSetTimeLimit(parse(text = filter)))
     }) %>%
     purrr::set_names(ifelse(is.na(purrr::map_chr(., ~ .x$metric)), purrr::map_chr(., ~ risk_lbl(.x$decision, type = "module")), paste("rule", seq_along(.), sep = "_")))
}

#' Create rule divs
#' 
#' Helper function to create the UI's associated with rule, metric, and decision category lists
#' 
#' @param rule_lst The ordered list of rules to create UI's for
#' @param metric_lst The named list of `{rismetric}` assessments
#' @param decision_lst The vector of allowable decision categories
#' @param ns The namespace the UI is being created inside of
#' 
#' @noRd
#' 
#' @importFrom purrr imap compact
create_rule_divs <- function(rule_lst, metric_lst, decision_lst, ns = NS(NULL)) {
  purrr::imap(rule_lst, ~ {
    if (isTRUE(.x == "remove")) return(NULL)
    
    if (grepl("^rule_\\d+$", .y)) {
      number <- strsplit(.y, "_")[[1]][2]
      mod_metric_rule_ui(ns("rule"), number, metric_lst, decision_lst, .x)
    } else {
      if (isTRUE(.x == "remove")) return(NULL)
      
      mod_risk_rule_ui(ns(risk_lbl(.x$decision, type = "module")), risk_lbl(.x$decision, type = "module"))
    }
  }) %>%
    purrr::compact()
}

#' Create rule observer
#'
#' Helper function to create the "remove" rule observer that cleans up the
#' environment including the reactive rule list, moduels inputs and module
#' observers
#'
#' @param rv The reactive value associated with the module
#' @param rule_lst the reactive values that contains `rv`
#' @param .input The shiny input object from the environment the module was
#'   called inside of
#' @param ns The namespace of the module
#' @param session The session object passed to function given to `shinyServer`.
#'   Default is `getDefaultReactiveDomain()`
#'
#' @noRd
#' 
#' @importFrom shinyjs runjs
#' @importFrom glue glue
create_rule_obs <- function(rv, rule_lst, .input, ns = NS(NULL), session = getDefaultReactiveDomain()) {
  o <- observeEvent(rule_lst[[rv]], {
    req(isTRUE(rule_lst[[rv]] == "remove"))
    removeUI(glue::glue('[data-rank-id={rv}]'))
    remove_shiny_inputs(rv, .input, ns = ns)
    session$onFlushed(function() {
      shinyjs::runjs(glue::glue("Shiny.setInputValue('{ns(\"rules_order\")}:sortablejs.rank_list', $.map($('#{ns(\"rules_list\")}').children(), function(child) {{return $(child).attr('data-rank-id') || $.trim(child.innerText);}}))"))
    })
    .subset2(rule_lst, "impl")$.values$remove(rv)
    o$destroy()
  })
}

#' Set evaluation time limit
#'
#' Sets a time limit on evaluation of an expression. This is a helper function
#' to allow users to add their own formulas or functions, which should have a
#' short evaluation time frame. This helps keep the application from getting
#' boggged down or for malicious code to be submitted.
#'
#' @param expr The expression to be evaluated
#' @param cpu,elapsed double (of length one). Set a limit on the total or
#'   elapsed cpu time in seconds, respectively.
#'
#' @noRd
evalSetTimeLimit <- function(expr, cpu = .25, elapsed = Inf) {
  setTimeLimit(cpu = cpu, elapsed = elapsed, transient = TRUE)
  on.exit({
    setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE)
  })
  try(eval(expr), silent = TRUE)
}
