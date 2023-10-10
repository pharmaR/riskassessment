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
      div(
        class = "icon",
        icon("grip-lines-vertical", class = c("rule_handle", "fa-xl"))
      ),
      div(
        selectInput(ns("metric"), NULL, metric_lst, .inputs$metric),
        textInput(ns("condition"), NULL, .inputs$condition %||% "", placeholder = "~ is.na(.x)"),
        selectInput(ns("decision"), NULL, decision_lst, .inputs$decision)
      ),
      div(
        class = "icon",
        actionLink(ns("remove_rule"), NULL, style = 'float: right;', shiny::icon("times", class = "fa-xl"))
      )
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
            type = "assessment",
            metric = input$metric,
            condition = input$condition,
            decision = input$decision,
            mapper = evalSetTimeLimit(parse(text = input$condition))
          )
      }) %>%
      bindEvent(input$metric, input$condition, input$decision,
                ignoreInit = TRUE)
    
    rules_observer <-
      observeEvent(rule_lst[[paste("rule", number, sep = "_")]], {
        req(!isTRUE(rule_lst[[paste("rule", number, sep = "_")]] == "remove"))
        
        updateSelectInput(session, "metric", selected = rule_lst[[paste("rule", number, sep = "_")]]$metric)
        updateTextInput(session, "condition", value = rule_lst[[paste("rule", number, sep = "_")]]$condition)
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
      div(
        class = "icon",
        icon("grip-lines-vertical", class = c("rule_handle", "fa-xl"))
      ),
      tagList(
        textOutput(ns("metric")),
        textOutput(ns("condition")),
        textOutput(ns("decision"))
      ) %>%
        purrr::map(~ tagAppendAttributes(.x, class = c("form-control", "shiny-input-container"))),
      div(
        class = "icon",
        actionLink(ns("remove_rule"), NULL, style = 'float: right;', shiny::icon("times", class = "fa-xl"))
      )
  )
}

#' metric_rule Server Functions
#'
#' @noRd 
mod_risk_rule_server <- function(id, condition, decision, rule_lst){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    out_return <- reactiveVal()
    
    output$metric <- renderText("Risk Score")
    output$condition <- renderText(condition())
    output$decision <- renderText(decision)
    
    # This is necessary to initialize the reactive value when `condition()` is
    # NULL. This happens when the rules are reset.
    rule_lst[[risk_lbl(decision, type = "module")]] <- 
      list(
        type = "overall_score",
        metric = NA_character_,
        condition = "",
        decision = decision,
        mapper = evalSetTimeLimit(parse(text = ""))
      )
    
    input_observer <- 
      observeEvent(condition(), {
        req(condition())
        
        rule_lst[[risk_lbl(decision, type = "module")]] <- 
          list(
            type = "overall_score",
            metric = NA_character_,
            condition = condition(),
            decision = decision,
            mapper = evalSetTimeLimit(parse(text = condition()))
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

mod_else_rule_ui <- function(id, decision_lst, .inputs = list()) {
  ns <- NS(id)
  
  div(
    style = "float: right; display: inline-flex",
    h5("ELSE", style = "padding-right: 5px"),
    selectInput(ns("decision"), NULL, c("No Decision" = "nd", decision_lst), .inputs$decision)
  )
}

mod_else_rule_server <- function(id, rule_lst) {
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    observe({
      rule_lst[["rule_else"]] <- 
        list(
          type = "else",
          metric = NA_character_,
          condition = "ELSE",
          decision = if (input$decision == "nd") NULL else input$decision,
          mapper = ~ .
        )
    }) %>%
      bindEvent(input$decision)
    
  })
}

## To be copied in the UI
# mod_metric_rule_ui("metric_rule_1")
    
## To be copied in the server
# mod_metric_rule_server("metric_rule_1")
