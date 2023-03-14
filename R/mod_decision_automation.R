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
mod_decision_automation_server <- function(id, user, decision_lst = c("Low", "Medium", "High")){
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
        num_dec <- length(decision_lst)
        initial_values <- purrr::imap(decision_lst, ~ c((.y - 1)/num_dec, .y/num_dec) %>% `[`(!. %in% c(0,1)) %>% round(2)) %>% 
          purrr::set_names(decision_lst)
        initial_selection <- NULL
        if (!rlang::is_empty(auto_json)) {
          for (.y in names(auto_json)) {
            initial_values[[.y]] <- 
              if (.y == decision_lst[1]) {
                unlist(auto_json[[.y]][2])
              } else if (.y == decision_lst[num_dec]) { 
                unlist(auto_json[[.y]][1])
              } else if (.y %in% decision_lst) {
                unlist(auto_json[[.y]])
              } else {
                warning(glue::glue("The decision category '{.y}' is not present in the allowed decision list!"))
              }
          }
          initial_selection <- names(auto_json)
        }
        
        output$auto_settings <-
          renderUI({
            dec_divs <- purrr::map(decision_lst, ~ div(
              risk = risk_lbl(.x, input = FALSE),
              class = "shinyjs-hide",
              style = "width: 100%",
              sliderInput(ns(risk_lbl(.x)), 
                          paste(.x, "Risk"), 0, 1, initial_values[[.x]],
                          width = "100%", sep = .01)
            ))
            div(
              style = "float: right;",
              shinyWidgets::dropdownButton(
                div(
                  style = "display: flex;",
                checkboxGroupInput(ns("auto_include"), "Auto-Assign Risk Decisions For...", decision_lst, selected = intersect(initial_selection, decision_lst), inline = TRUE),
                actionButton(ns("auto_reset"), label = icon("refresh"), class = "btn-circle-sm", style = "margin-left: auto;")
                ),
                dec_divs,
                br(),
                actionButton(ns("submit_auto"), "Apply Decision Rules", width = "100%"),
                circle = TRUE,
                icon = icon("gear"),
                right = TRUE,
                width = '600px',
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
        
        purrr::iwalk(decision_lst, function(.x, .y) {
          if (.y == 1) {
            this_lbl <- risk_lbl(.x)
            next_lbl <- risk_lbl(decision_lst[.y + 1])
            observeEvent(input[[this_lbl]], {
              if (req(.x %in% input$auto_include))
                auto_decision[[.x]] <- c(0, input[[this_lbl]])
              
              updateSliderInput(session, next_lbl, value = c(max(input[[this_lbl]], input[[next_lbl]][1]), max(input[[this_lbl]], input[[next_lbl]][2])))
            })
          } else if (.y == length(decision_lst)) {
            this_lbl <- risk_lbl(.x)
            prev_lbl <- risk_lbl(decision_lst[.y - 1])
            
            observeEvent(input[[this_lbl]], {
              if (req(.x %in% input$auto_include))
                auto_decision[[.x]] <- c(input[[this_lbl]], 1)
              
              updateSliderInput(session, prev_lbl, value = c(min(input[[prev_lbl]][1], input[[this_lbl]]), min(input[[prev_lbl]][2], input[[this_lbl]])))
            })
          } else {
            this_lbl <- risk_lbl(.x)
            next_lbl <- risk_lbl(decision_lst[.y + 1])
            prev_lbl <- risk_lbl(decision_lst[.y - 1])
            
            observeEvent(input[[this_lbl]], {
              if (req(.x %in% input$auto_include))
                auto_decision[[.x]] <- input[[this_lbl]]
              
              prev_value <- 
                if (.y - 1 == 1) 
                min(input[[prev_lbl]], input[[this_lbl]][1]) 
              else 
                c(min(input[[prev_lbl]][1], input[[this_lbl]][1]), min(input[[prev_lbl]][2], input[[this_lbl]][1]))
              
              next_value <-
                if (.y == length(decision_lst) - 1)
                  max(input[[this_lbl]][2], input[[next_lbl]])
              else
                c(max(input[[this_lbl]][2], input[[next_lbl]][1]), max(input[[this_lbl]][2], input[[next_lbl]][2]))

              updateSliderInput(session, prev_lbl, value = prev_value)
              updateSliderInput(session, next_lbl, value = next_value)
            })
          }
        })
        
        observeEvent(input$auto_reset, {
          req(user$role == "admin")
          
          purrr::iwalk(auto_list(), function(.x, .y) {
            reset_vals <- 
            if (.y == decision_lst[1]) {
              .x[2]
            } else if (.y == decision_lst[length(decision_lst)]) {
              .x[1]
            } else {
              .x
            }
            
            updateSliderInput(session, risk_lbl(.y), value = reset_vals)
          })
          updateCheckboxGroupInput(session, "auto_include", selected = names(auto_list()))
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
                'Please confirm your chosen decision rules: ',
                br(),
                if (!rlang::is_empty(purrr::compact(reactiveValuesToList(auto_decision)))) DT::DTOutput(ns("modal_table")) else h2("Disable Decision Automation"),
                br(),
                br(),
                em('Note: Once submitted, these rules will be applied to all new packages loaded into the app or when any metric re-weighting is performed.')
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
          shinyjs::click("auto_dropdown")
        })
        
        observeEvent(input$confirm_submit_auto, {
          req(user$role == "admin")
          
          out_lst <- purrr::compact(reactiveValuesToList(auto_decision))
          jsonlite::write_json(out_lst, "auto_decisions.json")
          auto_list(out_lst)
          
          if (length(out_lst) == 0) {
            loggit::loggit("INFO", glue::glue("Decision automation rules have been disabled by {user$name} ({user$role})."))
          } else {
            rules <- out_lst %>%
              purrr::imap_chr(~ glue::glue("{.y} = ({.x[[1]]}, {.x[[2]]}]"))
            loggit::loggit("INFO", glue::glue("The following decision rules were implemented by {user$name} ({user$role}): {paste(rules, collapse = '; ')}."))
          }
          
          removeModal()
          shinyjs::click("auto_dropdown")
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
