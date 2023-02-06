#' decision_automation UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
mod_decision_automation_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("auto_classify")),
    uiOutput(ns("auto_settings"))
  )
}

#' decision_automation Server Functions
#'
#' @noRd
#' 
#' @importFrom jsonlite read_json write_json
#' @importFrom purrr compact
mod_decision_automation_server <- function(id, user){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    auto_json <- jsonlite::read_json("auto_decisions.json")
    auto_list <- reactiveVal(auto_json)
    
    observe({
      req(user$role)
      
      if (rlang::is_empty(auto_list())) {
        if (user$role == "admin")
          output$auto_classify <-
            renderUI({
              tagList(
                br(),br(),
                h5("Decision Automation:"),
                p("Decision automation is not enabled. Click on the gear to the right if you wish to add.")
              )
            })
      } else {
        output$auto_classify <-
          renderUI({
            tagList(
              br(), br(),
              h5("Decision Automation:")
            )
          })
      }
      
      if (user$role == "admin") {
        output$auto_settings <-
          renderUI({
            div(
              style = "float: right",
                shinyWidgets::dropdownButton(
                  checkboxGroupInput(ns("auto_include"), "Auto-Assign Risk Decisions For...", c("Low", "Medium", "High"), inline = TRUE),
                  div(
                    risk = "low",
                    class = "shinyjs-hide",
                    style = "width: 100%",
                    sliderInput(ns("low_risk"), "Low Risk", 0, 1, .2,
                                width = '100%', step = .01)
                  ),
                  div(
                    risk = "medium",
                    class = "shinyjs-hide",
                    style = "width: 100%",
                    sliderInput(ns("med_risk"), "Medium Risk", 0, 1, c(.2,.5),
                                width = '100%', step = .01)
                  ),
                  div(
                    risk = "high",
                    class = "shinyjs-hide",
                    style = "width: 100%",
                    sliderInput(ns("high_risk"), "High Risk", 0, 1, .5,
                                width = '100%', step = .01)
                  ),
                  br(),
                  actionButton(ns("submit_auto"), "Apply Decision Rules", width = "100%"),
                  tags$style(HTML("[risk=low] .irs-bar {
                                    border-top: 1px solid #06B756FF;
                                    border-bottom: 1px solid #06B756FF;
                                    background: #06B756FF;
                                  }
                                  
                                  [risk=low] .irs-single {
                                    background-color: #06B756FF;
                                  }
                                  
                                  [risk=medium] .irs-bar {
                                    border-top: 1px solid #A99D04FF;
                                    border-bottom: 1px solid #A99D04FF;
                                    background: #A99D04FF;
                                  }
                                  
                                  [risk=medium] .irs-from,
                                  [risk=medium] .irs-to {
                                    background-color: #A99D04FF;
                                  }
                                  
                                  [risk=high] .irs-line {
                                    background: #A63E24FF;
                                    border: 1px solid #A63E24FF;
                                  }
                                  
                                  [risk=high] .irs-bar {
                                    border-top: 1px solid #ddd;
                                    border-bottom: 1px solid #ddd;
                                    background: linear-gradient(to bottom, #DDD -50%, #FFF 150%);
                                  }
                                  
                                  [risk=high] .irs-single {
                                    background-color: #A63E24FF;
                                  }")),
                  circle = TRUE,
                  icon = icon("gear"),
                  right = TRUE,
                  width = '21vw'
                )
            )
          })
        
        purrr::iwalk(auto_json, ~ 
                       if (.y == "Low") {
                         updateSliderInput(session, "low_risk", value = .x[2])
                       } else if (.y == "Medium") {
                         updateSliderInput(session, "med_risk", value = .x)
                       } else if (.y == "High") {
                         updateSliderInput(session, "high_risk", value = .x[1])
                       })
        updateCheckboxGroupInput(session, "auto_include", selected = names(auto_json))
        auto_decision <- reactiveValues()
        auto_current <- reactiveVal(names(auto_json))
        
        observeEvent(input$auto_include, {
          grp_added <- setdiff(input$auto_include, auto_current())
          grp_removed <- setdiff(auto_current(), input$auto_include)
          
          if (!rlang::is_empty(grp_added))
            purrr::walk(grp_added, ~ {
              if ("Low" == .x) {
                value_l <- 0
                if ("Medium" %in% input$auto_include) {
                  value_u <- min(input$low_risk, input$med_risk[1])
                  updateSliderInput(session, "low_risk", value = value_u)
                } else if ("High" %in% input$auto_include) {
                  value_u <- min(input$low_risk, input$high_risk)
                  updateSliderInput(session, "low_risk", value = value_u)
                } else {
                  value_u <- input$low_risk
                }
              } else if ("Medium" == .x) {
                if ("Low" %in% input$auto_include) {
                  value_l <- max(input$low_risk, input$med_risk[1])
                  updateSliderInput(session, "med_risk", value = c(value_l, max(value_l, input$med_risk[2])))
                } else {
                  value_l <- input$med_risk[1]
                }
                if ("High" %in% input$auto_include) {
                  value_u <- min(input$med_risk[2], input$high_risk)
                  updateSliderInput(session, "med_risk", value = c(min(input$med_risk[1], value_u), value_u))
                } else {
                  value_u <- input$med_risk[2]
                }
              } else if ("High" %in% grp_added) {
                if ("Medium" %in% input$auto_include) {
                  value_l <- max(input$med_risk[2], input$high_risk)
                  updateSliderInput(session, "high_risk", value = value_l)
                } else if ("Low" %in% input$auto_include) {
                  value_l <- max(input$low_risk, input$high_risk)
                  updateSliderInput(session, "high_risk", value = value_l)
                } else {
                  value_l <- input$high_risk
                }
                value_u <- 1
              }
              shinyjs::show(selector = glue::glue("[risk={tolower(.x)}"))
              auto_decision[[.x]] <- c(value_l, value_u)
            })
          
          if (!rlang::is_empty(grp_removed))
            purrr::walk(grp_removed, ~ {
              shinyjs::hide(selector = glue::glue("[risk={tolower(.x)}"))
              auto_decision[[.x]] <- NULL
            })
          
          auto_current(input$auto_include)
        }, ignoreNULL = FALSE)
        
        observeEvent(input$low_risk, {
          req("Low" %in% input$auto_include)
          
          auto_decision$Low <- c(0, input$low_risk)
          if ("Medium" %in% input$auto_include)
            updateSliderInput(session, "med_risk", value = c(max(input$low_risk, input$med_risk[1]), max(input$low_risk, input$med_risk[2])))
          else if ("High" %in% input$auto_include)
            updateSliderInput(session, "high_risk", value = max(input$low_risk, input$high_risk))
        })
        
        observeEvent(input$med_risk, {
          req("Medium" %in% input$auto_include)
          
          auto_decision$Medium <- input$med_risk
          if ("Low" %in% input$auto_include)
            updateSliderInput(session, "low_risk", value = min(input$low_risk, input$med_risk[1]))
          if ("High" %in% input$auto_include)
            updateSliderInput(session, "high_risk", value = max(input$med_risk[2], input$high_risk))
        })
        
        observeEvent(input$high_risk, {
          req("High" %in% input$auto_include)
          
          auto_decision$High <- c(input$high_risk, 1)
          if ("Medium" %in% input$auto_include)
            updateSliderInput(session, "med_risk", value = c(min(input$med_risk[1], input$high_risk), min(input$med_risk[2], input$high_risk)))
          else if ("Low" %in% input$auto_include) 
            updateSliderInput(session, "low_risk", value = min(input$low_risk, input$high_risk))
        })
        
        observeEvent(input$submit_auto, {
          req(user$role == "admin")
          
          out_lst <- purrr::compact(reactiveValuesToList(auto_decision))
          jsonlite::write_json(out_lst, "auto_decisions.json")
          auto_list(out_lst)
        })
      }
    })
    
    return(auto_list)
  })
}

## To be copied in the UI
# mod_decision_automation_ui("decision_automation_1")

## To be copied in the server
# mod_decision_automation_server("decision_automation_1")
