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
  div(`data-rank-id` = paste("rule", number, sep = "_"),
      div(icon("grip-lines-vertical", class = c("rule_handle", "fa-xl"))),
      selectInput(ns("metric"), NULL, metric_lst, .inputs$metric),
      textInput(ns("filter"), NULL, .inputs$filter %||% "", placeholder = "~ is.na(.x)"),
      selectInput(ns("decision"), NULL, decision_lst, .inputs$decision),
      div(actionLink(ns("remove_rule"), NULL, style = 'float: right;', shiny::icon("times", class = "fa-xl")))
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
            decision = input$decision,
            mapper = evalSetTimeLimit(parse(text = input$filter))
          )
      }) %>%
      bindEvent(input$metric, input$filter, input$decision,
                ignoreInit = TRUE)
    
    rules_observer <-
      observeEvent(rule_lst[[paste("rule", number, sep = "_")]], {
        req(!isTRUE(rule_lst[[paste("rule", number, sep = "_")]] == "remove"))
        
        updateSelectInput(session, "metric", selected = rule_lst[[paste("rule", number, sep = "_")]]$metric)
        updateTextInput(session, "filter", value = rule_lst[[paste("rule", number, sep = "_")]]$filter)
        updateSelectInput(session, "decision", selected = rule_lst[[paste("rule", number, sep = "_")]]$decision)
      })
    
    remove_observer <-
      observe({
        req(input$remove_rule > 0 | isTRUE(rule_lst[[paste("rule", number, sep = "_")]] == "remove"))
        rule_lst[[paste("rule", number, sep = "_")]] <- "remove"
        input_observer$destroy()
        rules_observer$destroy()
        remove_observer$destroy()
      }) %>%
      bindEvent(input$remove_rule, rule_lst[[paste("rule", number, sep = "_")]])
  })
}

#' metric_rule UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_risk_rule_ui <- function(id, data_rank_id){
  ns <- NS(id)
  if (missing(data_rank_id)) data_rank_id <- id
  div(`data-rank-id` = data_rank_id, style = "padding-bottom: 5px",
      div(icon("grip-lines-vertical", class = c("rule_handle", "fa-xl"))),
      tagList(
        textOutput(ns("metric")),
        textOutput(ns("filter")),
        textOutput(ns("decision"))
      ) %>%
        purrr::map(~ tagAppendAttributes(.x, class = c("form-control", "shiny-input-container"))),
      div(actionLink(ns("remove_rule"), NULL, style = 'float: right;', shiny::icon("times", class = "fa-xl")))
  )
}

#' metric_rule Server Functions
#'
#' @noRd 
mod_risk_rule_server <- function(id, filter, decision, rule_lst){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    out_return <- reactiveVal()
    
    output$metric <- renderText("Risk Score")
    output$filter <- renderText(filter())
    output$decision <- renderText(decision)
    
    # This is necessary to initialize the reactive value when `filter()` is
    # NULL. This happens when the rules are reset.
    rule_lst[[risk_lbl(decision, type = "module")]] <- 
      list(
        metric = NA_character_,
        filter = "",
        decision = decision,
        mapper = evalSetTimeLimit(parse(text = ""))
      )
    
    input_observer <- 
      observeEvent(filter(), {
        req(filter())
        
        rule_lst[[risk_lbl(decision, type = "module")]] <- 
          list(
            metric = NA_character_,
            filter = filter(),
            decision = decision,
            mapper = evalSetTimeLimit(parse(text = filter()))
          )
      })
    
    remove_observer <-
      observe({
        req(input$remove_rule > 0 | isTRUE(rule_lst[[risk_lbl(decision, type = "module")]] == "remove"))
        out_return(decision)
        rule_lst[[risk_lbl(decision, type = "module")]] <- "remove"
        input_observer$destroy()
        remove_observer$destroy()
      }) %>%
      bindEvent(input$remove_rule, rule_lst[[risk_lbl(decision, type = "module")]])
    
    out_return
  })
}
    
## To be copied in the UI
# mod_metric_rule_ui("metric_rule_1")
    
## To be copied in the server
# mod_metric_rule_server("metric_rule_1")
