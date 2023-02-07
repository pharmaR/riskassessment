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
    DT::dataTableOutput(ns("auto_table"))
  )
}

#' decision_automation Server Functions
#'
#' @noRd
#' 
#' @importFrom jsonlite read_json write_json
#' @importFrom purrr compact
#' @importFrom shinyWidgets tooltipOptions
mod_decision_automation_server <- function(id, user){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    auto_json <- jsonlite::read_json("auto_decisions.json")
    auto_list <- reactiveVal(auto_json)
    
    output$auto_table <-
      DT::renderDataTable({
        req(!rlang::is_empty(auto_list()))
        
        DT::datatable({
          auto_list() %>%
            purrr::imap_dfr(~ dplyr::tibble(decision = .y, ll = .x[[1]], ul = .x[[2]])) %>%
            dplyr::arrange(ll,)
        },
        escape = FALSE,
        class = "cell-border",
        selection = 'none',
        colnames = c("Decision Category", "Lower Limit", "Upper Limit"),
        rownames = FALSE,
        options = list(
          dom = "t",
          searching = FALSE,
          sScrollX = "100%",
          iDisplayLength = -1,
          ordering = FALSE
        ))
      })
    
    output$empty_auto <- 
      renderUI({
        if (rlang::is_empty(auto_list())) {
          tagList(
            br(),
            p("Decision automation is not enabled. Click on the gear to the right if you wish to add.")
          )
        }
      })
    
    observe({
      req(user$role)
      req(user$role == "admin" || !rlang::is_empty(auto_json))
      
      if (user$role == "admin") {
        output$auto_classify <-
          renderUI({
            tagList(
              br(),br(),
              hr(),
              fluidRow(
              column(9, h5("Decision Automation:")),
              column(3, uiOutput(ns("auto_settings")))
              ),
              uiOutput(ns("empty_auto")),
            )
          })
      } else if (!rlang::is_empty(auto_json)) {
        output$auto_classify <-
          renderUI({
            tagList(
              br(),br(),
              hr(),
              h5("Decision Automation:"),
            )
          })
      }
      
      if (user$role == "admin") {
        initial_values <- list(Low = .2, Medium = c(.2,.5), High = .5)
        initial_selection <- NULL
        if (!rlang::is_empty(auto_json)) {
          for (.y in names(auto_json)) {
            initial_values[[.y]] <- 
            if (.y == "Low") {
              unlist(auto_json[[.y]][2])
            } else if (.y == "Medium") {
              unlist(auto_json[[.y]])
            } else if (.y == "High") {
              unlist(auto_json[[.y]][1])
            }
          }
          initial_selection <- names(auto_json)
        }
        
        output$auto_settings <-
          renderUI({
            div(
              style = "float: right;",
              shinyWidgets::dropdownButton(
                checkboxGroupInput(ns("auto_include"), "Auto-Assign Risk Decisions For...", c("Low", "Medium", "High"), selected = initial_selection, inline = TRUE),
                div(
                  risk = "low",
                  class = "shinyjs-hide",
                  style = "width: 100%",
                  sliderInput(ns("low_risk"), "Low Risk", 0, 1, initial_values$Low,
                              width = '100%', step = .01)
                ),
                div(
                  risk = "medium",
                  class = "shinyjs-hide",
                  style = "width: 100%",
                  sliderInput(ns("med_risk"), "Medium Risk", 0, 1, initial_values$Medium,
                              width = '100%', step = .01)
                ),
                div(
                  risk = "high",
                  class = "shinyjs-hide",
                  style = "width: 100%",
                  sliderInput(ns("high_risk"), "High Risk", 0, 1, initial_values$High,
                              width = '100%', step = .01)
                ),
                br(),
                actionButton(ns("submit_auto"), "Apply Decision Rules", width = "100%"),
                circle = TRUE,
                icon = icon("gear"),
                right = TRUE,
                width = '350px',
                inputId  = ns("auto_dropdown"),
                tooltip = shinyWidgets::tooltipOptions(title = "Click here to add/adjust decision automation rules.", placement = "left")
              )
            )
          })
        
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
        
        output$modal_table <- 
          DT::renderDataTable({
            out_lst <- purrr::compact(reactiveValuesToList(auto_decision))
            
            DT::datatable({
              out_lst %>%
                purrr::imap_dfr(~ dplyr::tibble(decision = .y, ll = .x[[1]], ul = .x[[2]])) %>%
                dplyr::arrange(ll,)
            },
            escape = FALSE,
            class = "cell-border",
            selection = 'none',
            colnames = c("Decision Category", "Lower Limit", "Upper Limit"),
            rownames = FALSE,
            options = list(
              dom = "t",
              searching = FALSE,
              sScrollX = "100%",
              iDisplayLength = -1,
              ordering = FALSE
            ))
          })
        
        observeEvent(input$submit_auto, {
          req(user$role == "admin")
          
          showModal(modalDialog(
            size = "l",
            easyClose = TRUE,
            h5("Apply Decision Rules", style = 'text-align: center !important'),
            hr(),
            br(),
            fluidRow(
              column(
                width = 12,
                'Please confirm your chosen decision classification rules: ',
                br(),
                if (!rlang::is_empty(purrr::compact(reactiveValuesToList(auto_decision)))) DT::DTOutput(ns("modal_table")) else h2("Disable Decision Automation"),
                br(),
                br(),
                em('Note: Once submitted, these rules will be applied to any new packages uploaded or if reweighting.')
              )
            ),
            br(),
            footer = tagList(
              actionButton(ns('confirm_submit_auto'), 'Submit'),
              actionButton(ns('cancel'), 'Cancel')
            )))
        })
        
        # Close modal if user cancels decision submission.
        observeEvent(input$cancel, {
          removeModal()
        })
        
        observeEvent(input$confirm_submit_auto, {
          req(user$role == "admin")
          
          out_lst <- purrr::compact(reactiveValuesToList(auto_decision))
          jsonlite::write_json(out_lst, "auto_decisions.json")
          auto_list(out_lst)
          
          removeModal()
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
