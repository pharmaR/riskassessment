#' metric_rule UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_metric_rule_ui <- function(id, number, metric_lst, decision_lst, .inputs = list()){
  ns <- NS(paste(id, number, sep = "_"))
  div(`data-rank-id` = paste("rule", number, sep = "_"), style = "display: flex; align-items: center;",
      icon("grip-vertical", class = "rule_handle"),
      selectInput(ns("metric"), NULL, metric_lst, .inputs$metric),
      textInput(ns("filter"), NULL, .inputs$filter %||% "", placeholder = "~ is.na(.x)"),
      selectInput(ns("decision"), NULL, decision_lst, .inputs$decision),
      actionLink(ns("remove_rule"), NULL, style = 'float: right;', shiny::icon("times"))
  )
}
    
#' metric_rule Server Functions
#'
#' @noRd 
mod_metric_rule_server <- function(id, number, rule_lst){
  inputId <- paste(id, number, sep = "_")
  moduleServer( inputId, function(input, output, session){
    ns <- session$ns
 
    input_observer <- 
      observe({
        rule_lst[[paste("rule", number, sep = "_")]] <- 
          list(
            metric = input$metric,
            filter = input$filter,
            decision = input$decision
          )
      }) %>%
      bindEvent(input$metric, input$filter, input$decision)
    
    rules_observer <-
      observeEvent(rule_lst[[paste("rule", number, sep = "_")]], {
        
        updateSelectInput(session, "metric", selected = rule_lst[[paste("rule", number, sep = "_")]]$metric)
        updateTextInput(session, "filter", value = rule_lst[[paste("rule", number, sep = "_")]]$filter)
        updateSelectInput(session, "decision", selected = rule_lst[[paste("rule", number, sep = "_")]]$decision)
      })
    
    remove_observer <-
      observeEvent(input$remove_rule, {
        rule_lst[[paste("rule", number, sep = "_")]] <- "remove"
        input_observer$destroy()
        rules_observer$destroy()
        remove_observer$destroy()
      })
  })
}
    
## To be copied in the UI
# mod_metric_rule_ui("metric_rule_1")
    
## To be copied in the server
# mod_metric_rule_server("metric_rule_1")
