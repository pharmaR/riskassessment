#' decision_automation UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
mod_decision_automation_ui <- function(id){
  ns <- NS(id)
  
  decision_lst <- if (!is.null(golem::get_golem_options("decision_categories"))) golem::get_golem_options("decision_categories") else c("Low Risk", "Medium Risk", "High Risk")
  color_lst <- get_colors(golem::get_golem_options("assessment_db_name"))
  dec_num <- length(decision_lst)
  dec_root <- glue::glue("--{risk_lbl(decision_lst, input = FALSE)}-color: {color_lst};") %>%
    glue::glue_collapse(sep = "\n") %>%
    {glue::glue(":root {{
                {.}
                }}")}
  dec_css <- purrr::imap_chr(decision_lst, function(.x, .y) {
    lbl <- risk_lbl(.x, input = FALSE)

    if (.y == 1) {
      glue::glue("
[risk={lbl}] .irs--shiny .irs-bar {{
  border-top: 1px solid var(--{lbl}-color);
  border-bottom: 1px solid var(--{lbl}-color);
  background: var(--{lbl}-color);
}}

[risk={lbl}] .irs--shiny .irs-single {{
  background-color: var(--{lbl}-color);
}}")
    } else if (.y == dec_num) {
      glue::glue("
[risk={lbl}] .irs--shiny .irs-line {{
  background: var(--{lbl}-color);
  border: 1px solid var(--{lbl}-color);
}}

[risk={lbl}] .irs--shiny .irs-bar {{
  background: linear-gradient(to bottom, #dedede -50%, #fff 150%);
  background-color: #ededed;
  border: 1px solid #cccccc;
  border-radius: 8px;
}}

[risk={lbl}] .irs--shiny .irs-single {{
  background-color: var(--{lbl}-color);
}}")
    } else {
      glue::glue("
[risk={lbl}] .irs--shiny .irs-bar {{
  border-top: 1px solid var(--{lbl}-color);
  border-bottom: 1px solid var(--{lbl}-color);
  background: var(--{lbl}-color);
}}

[risk={lbl}] .irs--shiny .irs-from,
[risk={lbl}] .irs--shiny .irs-to {{
  background-color: var(--{lbl}-color);
}}")
    }
  }) 
  
  tagList(
    tags$head(tags$style(HTML(c(dec_root, dec_css)))),
    uiOutput(ns("auto_classify")),
    DT::dataTableOutput(ns("auto_table"))
  )
}

#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
mod_decision_automation_ui_2 <- function(id){
  ns <- NS(id)
  
  tagList(
    uiOutput(ns("auto_settings2")),
    DT::dataTableOutput(ns("dec_cat_table"))
  )
}

#' decision_automation Server Functions
#'
#' @noRd
#' 
#' @importFrom purrr compact
#' @importFrom shinyWidgets tooltipOptions
#' @importFrom colourpicker colourInput
mod_decision_automation_server <- function(id, user){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    exportTestValues(
      datatable = {
        auto_decision_update() %>%
          purrr::imap_dfr(~ dplyr::tibble(decision = .y, ll = .x[[1]], ul = .x[[2]])) %>%
          dplyr::arrange(ll,)
      },
      auto_decision = {
        reactiveValuesToList(auto_decision)
      }
    )
    
    auto_decision_initial <- process_dec_tbl(golem::get_golem_options('assessment_db_name'))
    auto_decision_update <- reactiveVal(auto_decision_initial)
    auto_decision <- reactiveValues()
    auto_current <- reactiveVal(names(auto_decision_initial))

    dec_table <- reactiveVal({
      dbSelect("SELECT decision, color, lower_limit, upper_limit FROM decision_categories") %>%
        dplyr::mutate(lower_limit = dplyr::if_else(is.na(lower_limit), "-", as.character(lower_limit)),
                      upper_limit = dplyr::if_else(is.na(upper_limit), "-", as.character(upper_limit)))
      
    })
    
    decision_lst <- if (!is.null(golem::get_golem_options("decision_categories"))) golem::get_golem_options("decision_categories") else c("Low Risk", "Medium Risk", "High Risk")
    
    output$auto_table <-
      DT::renderDataTable({
        req(!rlang::is_empty(auto_decision_update()))
        
        DT::datatable({
          auto_decision_update() %>%
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
        if (rlang::is_empty(auto_decision_update())) {
          tagList(
            br(),
            p("Decision automation is not enabled. Click on the gear to the right if you wish to add.")
          )
        }
      })
    
    output$dec_cat_table <- DT::renderDataTable(dec_table())
    
    output$auto_classify <- renderUI({
      if (user$role == "admin") {
        tagList(
          br(),br(),
          hr(),
          fluidRow(
            column(9, h5("Decision Automation:")),
            column(3, uiOutput(ns("auto_settings")))
          ),
          uiOutput(ns("empty_auto")),
        )
      } else if (!rlang::is_empty(auto_decision_initial)) {
        tagList(
          br(),br(),
          hr(),
          h5("Decision Automation:"),
        )
      }
    })
    
    if (!rlang::is_empty(auto_decision_initial)) {
      valid_names <- names(auto_decision_initial) %in% decision_lst
      if (any(!valid_names))
        warning(glue::glue("The decision category(ies) {paste(names(auto_decision_initial)[!valid_names], collapse = ', ')} is(are) note present in the allowed decision list!"))
      initial_values <- purrr::map(auto_decision_initial[valid_names], unlist)
      ranges <- c(0, unlist(initial_values, use.names = FALSE), 1)
      grp_len <- cumsum(decision_lst %in% names(auto_decision_initial)) %>% table()
      iter <- 0
      grp_iter <- 0
      for (.y in decision_lst) {
        if (.y %in% names(initial_values)) {
          ranges <- ranges[as.logical(cumsum(ranges == initial_values[[.y]][2]))]
          iter <- 0
          grp_iter <- grp_iter + 1
        } else {
          initial_values[[.y]] <- ranges[1] + (ranges[2] - ranges[1])*c(iter, iter + 1)/(grp_len[as.character(grp_iter)] - (grp_iter != 0))
          iter <- iter + 1
        }
      }
      initial_selection <- names(auto_decision_initial)
    } else {
      num_dec <- length(decision_lst)
      initial_values <- purrr::imap(decision_lst, ~ c((.y - 1)/num_dec, .y/num_dec)) %>% 
        purrr::set_names(decision_lst)
      initial_selection <- NULL
    }
    initial_values <- purrr::map(initial_values, ~ .x %>% `[`(!. %in% c(0,1)) %>% round(2))
    
    output$auto_settings <-
      renderUI({
        req(user$role)
        req(user$role == "admin")
        
        dec_divs <- purrr::map(decision_lst, ~ div(
          risk = risk_lbl(.x, input = FALSE),
          class = if (!.x %in% initial_selection) "shinyjs-hide",
          style = "width: 100%",
          sliderInput(ns(risk_lbl(.x)), 
                      .x, 0, 1, initial_values[[.x]],
                      width = "100%", sep = .01)
        ))
        div(
          style = "float: right;",
          shinyWidgets::dropdownButton(
            div(
              style = "display: flex;",
              checkboxGroupInput(ns("auto_include"), "Auto-Assign Decisions For...", decision_lst, selected = intersect(initial_selection, decision_lst), inline = TRUE),
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
    
    
    output$auto_settings2 <-
      renderUI({
        req(user$role)
        req(user$role == "admin")

        dec_divs <- purrr::map(decision_lst, ~ div(
          risk = risk_lbl(.x, input = FALSE),
          class = if (!.x %in% initial_selection) "shinyjs-hide",
          style = "width: 100%",
          sliderInput(ns(glue::glue("{risk_lbl(.x)}_2")), 
                      .x, 0, 1, initial_values[[.x]],
                      width = "100%", sep = .01)
        ))
        col_divs <- purrr::map(decision_lst, ~ div(
          class = "shinyjs-hide",
          style = "width: 100%",
          colourpicker::colourInput(ns(glue::glue("{risk_lbl(.x, input = FALSE)}_col_2")),
                                    "")
        ))
        div(
          style = "float: right;",
          shinyWidgets::dropdownButton(
            div(
              style = "display: flex;",
              selectInput(ns("dec_color"), "Select Display Color For...", decision_lst),
              actionButton(ns("auto_reset_2"), label = icon("refresh"), class = "btn-circle-sm", style = "margin-left: auto;")
            ),
            col_divs,
            checkboxGroupInput(ns("auto_include_2"), "Auto-Assign Decisions For...", decision_lst, selected = intersect(initial_selection, decision_lst), inline = TRUE),
            dec_divs,
            br(),
            actionButton(ns("submit_auto_2"), "Apply Decision Rules", width = "100%"),
            circle = TRUE,
            icon = icon("gear"),
            right = TRUE,
            width = '600px',
            inputId  = ns("auto_dropdown_2"),
            tooltip = shinyWidgets::tooltipOptions(title = "Click here to add/adjust decision automation rules.", placement = "left")
          )
        )
      })
    
    outputOptions(output, "auto_settings2", suspendWhenHidden = FALSE)
    
    observeEvent(input$auto_include, {
      
      updateCheckboxGroupInput(session, "auto_include_2", selected = input$auto_include)
      grp_added <- setdiff(input$auto_include, auto_current())
      grp_removed <- setdiff(auto_current(), input$auto_include)
      
      if (!rlang::is_empty(grp_added))
        purrr::walk(grp_added, ~ {
          value_lst <- c(0, input[[risk_lbl(.x)]], 1)
          if (.x == decision_lst[1]) {
            values <- value_lst[1:2]
          } else {
            values <- value_lst[2:3]
          }
          
          shinyjs::show(selector = glue::glue("[risk={risk_lbl(.x, input = FALSE)}"))
          auto_decision[[.x]] <- values
        })
      
      if (!rlang::is_empty(grp_removed))
        purrr::walk(grp_removed, ~ {
          shinyjs::hide(selector = glue::glue("[risk={risk_lbl(.x, input = FALSE)}"))
          auto_decision[[.x]] <- NULL
        })
      
      auto_current(input$auto_include)
    }, ignoreNULL = FALSE)
    
    observeEvent(input$auto_include_2, {
      updateCheckboxGroupInput(session, "auto_include", selected = input$auto_include_2)
    }, ignoreNULL = FALSE)
    
    purrr::iwalk(decision_lst, function(.x, .y) {
      this_lbl <- risk_lbl(.x)
      next_lbl <- risk_lbl(decision_lst[.y + 1])
      prev_lbl <- risk_lbl(decision_lst[.y - 1])
      
      observeEvent(input[[paste(this_lbl, 2, sep = "_")]], {
        updateSliderInput(session, this_lbl, value = input[[paste(this_lbl, 2, sep = "_")]])
      })
      
      observeEvent(input[[this_lbl]], {
        updateSliderInput(session, paste(this_lbl, 2, sep = "_"), value = input[[this_lbl]])
        if (.x %in% input$auto_include) {
          auto_decision[[.x]] <- if (.y == 1) c(0, input[[this_lbl]]) else if (.y == length(decision_lst)) c(input[[this_lbl]], 1) else input[[this_lbl]]
        }
        
        
        prev_value <- 
          if (.y - 1 == 1) 
            min(input[[prev_lbl]], input[[this_lbl]]) 
        else 
          c(min(input[[prev_lbl]][1], input[[this_lbl]]), min(input[[prev_lbl]][2], input[[this_lbl]]))
        
        next_value <-
          if (.y == length(decision_lst) - 1)
            max(input[[this_lbl]], input[[next_lbl]])
        else
          c(max(input[[this_lbl]], input[[next_lbl]][1]), max(input[[this_lbl]], input[[next_lbl]][2]))
        
        
        if (.y != 1)
          updateSliderInput(session, prev_lbl, value = prev_value)
        if (.y != length(decision_lst))
          updateSliderInput(session, next_lbl, value = next_value)
      })
      
    })
    
    observeEvent(input$auto_reset, {
      purrr::iwalk(auto_decision_update(), function(.x, .y) {
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
      updateCheckboxGroupInput(session, "auto_include", selected = names(auto_decision_update()))
    })
    
    observeEvent(input$auto_reset_2, {
      shinyjs::click("auto_reset")
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
    
    observeEvent(input$submit_auto_2, {
      shinyjs::click("submit_auto")
    })
    
    # Close modal if user cancels decision submission.
    observeEvent(input$cancel, {
      removeModal()
    })
    
    observeEvent(input$confirm_submit_auto, {
      req(user$role)
      req(user$role == "admin")
      
      out_lst <- purrr::compact(reactiveValuesToList(auto_decision))
      dbUpdate("UPDATE decision_categories SET lower_limit = NULL, upper_limit = NULL")
      purrr::iwalk(out_lst, ~ dbUpdate(glue::glue("UPDATE decision_categories SET lower_limit = {.x[1]}, upper_limit = {.x[2]} WHERE decision = '{.y}'")))
      auto_decision_update(out_lst)
      
      if (length(out_lst) == 0) {
        loggit::loggit("INFO", glue::glue("Decision automation rules have been disabled by {user$name} ({user$role})."))
      } else {
        rules <- out_lst %>%
          purrr::imap_chr(~ glue::glue("{.y} = ({.x[[1]]}, {.x[[2]]}]"))
        loggit::loggit("INFO", glue::glue("The following decision rules were implemented by {user$name} ({user$role}): {paste(rules, collapse = '; ')}."))
      }
      
      removeModal()
    })
    
    return(auto_decision_update)
  })
}

## To be copied in the UI
# mod_decision_automation_ui("decision_automation_1")

## To be copied in the server
# mod_decision_automation_server("decision_automation_1")
