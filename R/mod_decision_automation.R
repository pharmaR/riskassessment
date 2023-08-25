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
  color_lst <- if (!is.null(golem::get_golem_options("assessment_db_name"))) get_colors(golem::get_golem_options("assessment_db_name")) else c(`Low Risk` = "#06B756", `Medium Risk` = "#A99D04", `High Risk` = "#A63E24")
  dec_num <- length(decision_lst)
  dec_root <- glue::glue("--{risk_lbl(decision_lst, type = 'attribute')}-color: {color_lst};") %>%
    glue::glue_collapse(sep = "\n") %>%
    {glue::glue(":root {{
                {.}
                }}")}
  dec_css <- purrr::imap_chr(decision_lst, function(.x, .y) {
    lbl <- risk_lbl(.x, type = 'attribute')

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
    DT::dataTableOutput(ns("rule_table")),
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
#' @importFrom colourpicker colourInput updateColourInput
#' @importFrom rlang is_formula is_function
#' @importFrom sortable sortable_js sortable_options sortable_js_capture_input
mod_decision_automation_server <- function(id, user, credentials){
  if (missing(credentials))
    credentials <- get_golem_config("credentials", file = app_sys("db-config.yml"))
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
    
    decision_lst <- if (!is.null(golem::get_golem_options("decision_categories"))) golem::get_golem_options("decision_categories") else c("Low Risk", "Medium Risk", "High Risk")
    metric_lst <- dbSelect("SELECT name, long_name FROM metric") %>%
      with(purrr::set_names(name, long_name))
    
    #### Color List ####
    color_lst <- get_colors(golem::get_golem_options("assessment_db_name"))
    color_current <- reactiveVal(color_lst)
    color_updated <- reactiveVal(color_lst)
    
    col_divs <- reactiveVal({
      col_width <- (100/length(decision_lst)) %>% min(50) %>% max(25)
      purrr::map2(decision_lst, color_lst, ~ div(
        style = glue::glue("width: {col_width}%"),
        colourpicker::colourInput(ns(glue::glue("{risk_lbl(.x, type = 'attribute')}_col")),
                                  .x, .y)
      ))
    })
    purrr::walk(c("submit_color", "submit_auto", "close_decision_modal"), ~ {
      observeEvent(input[[.x]], {
        col_width <- (100/length(decision_lst)) %>% min(50) %>% max(25)
        col_divs({
          purrr::map2(decision_lst, color_updated(), ~ div(
            style = glue::glue("width: {col_width}%"),
            colourpicker::colourInput(ns(glue::glue("{risk_lbl(.x, type = 'attribute')}_col")),
                                      .x, .y)
          ))
        })
      })
    })
    
    observeEvent(input$col_reset, {
      purrr::walk2(decision_lst, color_current(), ~ {
        colourpicker::updateColourInput(session, glue::glue("{risk_lbl(.x, type = 'attribute')}_col"), value = .y)
      })
    })
    
    #### Risk Score Rules ####
    auto_decision_initial <- process_dec_tbl(golem::get_golem_options('assessment_db_name'))
    auto_decision_update <- reactiveVal(auto_decision_initial)
    auto_decision <- reactiveValues()
    auto_current <- reactiveVal(names(auto_decision_initial))
    
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
    updated_values <- do.call(reactiveValues, initial_values)
    
    dec_divs <- reactiveVal({
      purrr::map(decision_lst, ~ div(
        risk = risk_lbl(.x, type = 'attribute'),
        class = if (!.x %in% names(auto_decision_initial)) "shinyjs-hide",
        style = "width: 100%",
        sliderInput(ns(risk_lbl(.x)), 
                    .x, 0, 1, initial_values[[.x]],
                    width = "100%", sep = .01)
      ))
    })
    purrr::walk(c("submit_color", "submit_auto", "close_decision_modal"), ~ {
      observeEvent(input[[.x]], {
        dec_divs({
          purrr::map(decision_lst, ~ div(
            risk = risk_lbl(.x, type = 'attribute'),
            class = if (!.x %in% auto_current()) "shinyjs-hide",
            style = "width: 100%",
            sliderInput(ns(risk_lbl(.x)), 
                        .x, 0, 1, updated_values[[.x]],
                        width = "100%", sep = .01)
          ))
        })
      })
    })
    
    observeEvent(input$auto_include, {
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
          
          shinyjs::show(selector = glue::glue("[risk={risk_lbl(.x, type = 'attribute')}"))
          auto_decision[[.x]] <- values
        })
      
      if (!rlang::is_empty(grp_removed))
        purrr::walk(grp_removed, ~ {
          shinyjs::hide(selector = glue::glue("[risk={risk_lbl(.x, type = 'attribute')}"))
          auto_decision[[.x]] <- NULL
        })
      
    }, ignoreNULL = FALSE, ignoreInit = TRUE)
    
    observeEvent(input$auto_include, {
      auto_current(input$auto_include)
    }, ignoreNULL = FALSE, ignoreInit = TRUE,
    priority = -100)
    
    purrr::iwalk(decision_lst, function(.x, .y) {
      this_lbl <- risk_lbl(.x)
      next_lbl <- risk_lbl(decision_lst[.y + 1])
      prev_lbl <- risk_lbl(decision_lst[.y - 1])
      
      observeEvent(input[[this_lbl]], {
        updateSliderInput(session, paste(this_lbl, 2, sep = "_"), value = input[[this_lbl]])
        updated_values[[.x]] <- input[[this_lbl]]
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
    
    #### Metric Rules ####
    risk_rule_initial <- process_rule_tbl(golem::get_golem_options('assessment_db_name'))
    risk_rule_update <- reactiveVal(purrr::imap(risk_rule_initial, ~ {
      if (.y == "risk_score_rule")
        .x$rules <- auto_decision_initial
      .x
    }))
    rule_lst <- do.call(reactiveValues, risk_rule_initial)
    rule_number <- reactiveVal(length(risk_rule_initial))

    purrr::iwalk(risk_rule_initial, ~ {
      if (grepl("^rule_\\d+$", .y)) {
        number <- strsplit(.y, "_")[[1]][2]
        mod_metric_rule_server("rule", number, rule_lst)
        create_rule_obs(.y, rule_lst, .input = input, ns = ns)
      } else {
        out_return <- mod_risk_rule_server(risk_lbl(.x$decision, type = "module"), reactive(glue::glue("~ {auto_decision[[.x$decision]][1]} <= .x & .x <= {auto_decision[[.x$decision]][2]}")), .x$decision, rule_lst)
        risk_module_observer <-
          observeEvent(out_return(), {
            updateCheckboxGroupInput(session, "auto_include", selected = setdiff(input$auto_include, out_return()))
          })
        o <- observeEvent(rule_lst[[risk_lbl(.x$decision, type = "module")]], {
          req(isTRUE(rule_lst[[risk_lbl(.x$decision, type = "module")]] == "remove"))
          risk_module_observer$destroy()
          o$destroy()
        })
        create_rule_obs(risk_lbl(.x$decision, type = "module"), rule_lst, .input = input, ns = ns)
      }
    })
    
    observeEvent(input$add_rule, {
      rule_number(rule_number() + 1)
      insertUI(paste0("#", ns("rules_list")), "beforeEnd", mod_metric_rule_ui(ns("rule"), rule_number(), metric_lst, decision_lst))
      mod_metric_rule_server("rule", rule_number(), rule_lst)
      create_rule_obs(paste("rule", rule_number(), sep = "_"), rule_lst, .input = input, ns = ns)
      session$onFlushed(function() {
        shinyjs::runjs(glue::glue("Shiny.setInputValue('{ns(\"rules_order\")}:sortablejs.rank_list', $.map($('#{ns(\"rules_list\")}').children(), function(child) {{return $(child).attr('data-rank-id') || $.trim(child.innerText);}}))"))
      })
    })
    
    observeEvent(input$auto_include, {
      grp_added <- setdiff(input$auto_include, auto_current())
      grp_removed <- setdiff(auto_current(), input$auto_include)
      
      if (!rlang::is_empty(grp_added)) {
        purrr::walk(grp_added, ~ {
          if (!is.null(rule_lst[[risk_lbl(.x, type = "module")]])) return(NULL)

          insertUI(paste0("#", ns("rules_list")), "beforeEnd", mod_risk_rule_ui(ns(risk_lbl(.x, type = "module")), risk_lbl(.x, type = "module")))
          out_return <- mod_risk_rule_server(risk_lbl(.x, type = "module"), reactive(glue::glue("~ {auto_decision[[.x]][1]} <= .x & .x <= {auto_decision[[.x]][2]}")), .x, rule_lst)
          risk_module_observer <-
            observeEvent(out_return(), {
              updateCheckboxGroupInput(session, "auto_include", selected = setdiff(input$auto_include, out_return()))
            })
          o <- observeEvent(rule_lst[[risk_lbl(.x, type = "module")]], {
            req(isTRUE(rule_lst[[risk_lbl(.x, type = "module")]] == "remove"))
            risk_module_observer$destroy()
            o$destroy()
          })
          create_rule_obs(risk_lbl(.x, type = "module"), rule_lst, .input = input, ns = ns)
        })
      }

      if (!rlang::is_empty(grp_removed)) {
        purrr::walk(grp_removed, ~ {
          if (is.null(rule_lst[[risk_lbl(.x, type = "module")]])) return(NULL)

          rule_lst[[risk_lbl(.x, type = "module")]] <- "remove"
        })
      }
      session$onFlushed(function() {
        shinyjs::runjs(glue::glue("Shiny.setInputValue('{ns(\"rules_order\")}:sortablejs.rank_list', $.map($('#{ns(\"rules_list\")}').children(), function(child) {{return $(child).attr('data-rank-id') || $.trim(child.innerText);}}))"))
      })
    }, ignoreNULL = FALSE, ignoreInit = TRUE)
    
    rule_divs <- reactiveVal(create_rule_divs(risk_rule_initial, metric_lst, decision_lst, ns = ns))
    purrr::walk(c("submit_color", "submit_auto", "close_decision_modal"), ~ {
      observeEvent(input[[.x]], {
        rule_divs(create_rule_divs(purrr::compact(reactiveValuesToList(rule_lst)[input$rules_order]), metric_lst, decision_lst, ns = ns))
      })
    })
    
    observeEvent(input$auto_reset, {
      purrr::walk(input$rules_order, ~ {
        if (!.x %in% names(risk_rule_update())) {
          rule_lst[[.x]] <- "remove"
        } else {
          removeUI(glue::glue('[data-rank-id={.x}]'))
        }
      })
      
      purrr::walk(create_rule_divs(risk_rule_update(), metric_lst, decision_lst, ns = ns), ~ {
        insertUI(paste0("#", ns("rules_list")), "beforeEnd", .x)
      })
      purrr::iwalk(risk_rule_update(), ~ {
        if (.y %in% input$rules_order) return(NULL)
        
        if (.y != "risk_score_rule") {
          number <- strsplit(.y, "_")[[1]][2]
          mod_metric_rule_server("rule", number, rule_lst)
        }
        create_rule_obs(.y, rule_lst, .input = input, ns = ns)
      })
    })
    
    disable_auto_submit <- reactiveVal(TRUE)
    observeEvent(reactiveValuesToList(rule_lst), {
      disable_auto_submit(FALSE)
      rule_list <- reactiveValuesToList(rule_lst)
      for (rule in rule_list) {
        if (isTRUE(rule == "remove")) {
          disable_auto_submit(TRUE)
          break
        }
        if (rule$filter == "risk_score_rule") next
        if (rlang::is_formula(rule$mapper) | rlang::is_function(rule$mapper)) {
          disable_auto_submit(TRUE)
          break
        }
      }
    })
    
    #### Outputs ####
    purrr::walk(c("auto_dropdown", "auto_dropdown2"), ~
                  observeEvent(input[[.x]], {
                    req("auto_decision_adjust" %in% credentials$privileges[[user$role]])
                    
                    showModal(modalDialog(
                      size = "l",
                      footer = actionButton(ns("close_decision_modal"), "Close"),
                      uiOutput(ns("decision_rule_div"))
                    ))
                  })
    )
    
    dec_table <- reactiveVal({
      dbSelect("SELECT decision, color, lower_limit, upper_limit FROM decision_categories") %>%
        dplyr::mutate(lower_limit = dplyr::if_else(is.na(lower_limit), "-", as.character(lower_limit)),
                      upper_limit = dplyr::if_else(is.na(upper_limit), "-", as.character(upper_limit)))
      
    })
    
    output$rule_table <-
      DT::renderDataTable({
        req(!rlang::is_empty(risk_rule_update()))
        
        DT::datatable({
          risk_rule_update() %>% 
            purrr::map_dfr(~ dplyr::as_tibble(.x[c("metric", "filter", "decision")]) %>% dplyr::mutate(metric = if (is.na(metric)) "Risk Score" else metric))
        },
        escape = FALSE,
        class = "cell-border",
        selection = 'none',
        colnames = c("Metric", "Conditional", "Decision"),
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
        if (rlang::is_empty(risk_rule_update())) {
          tagList(
            br(),
            p("Decision automation is not enabled. Click on the gear to the right if you wish to add.")
          )
        }
      })
    
    output$dec_cat_table <- DT::renderDataTable({
      formattable::as.datatable(
        formattable::formattable(
          dec_table(),
          list(
            color = formattable::formatter(
              "span",
              style = x ~ formattable::style(display = "block",
                                             "border-radius" = "4px",
                                             "padding-right" = "4px",
                                             "font-weight" = "bold",
                                             "color" = "white",
                                             "background-color" = x))
          )),
        colnames = c("Category", "Color", "Lower Bound", "Upper Bound"),
        rownames = FALSE,
        options = list(
          ordering = FALSE,
          pageLength = -1,
          dom = 't',
          columnDefs = list(list(className = 'dt-center', targets = "_all"))
        ),
        style = "default"
      ) %>%
        DT::formatStyle(names(dec_table()), textAlign = 'center')
    })
    
    output$auto_classify <- renderUI({
      if ("auto_decision_adjust" %in% credentials$privileges[[user$role]]) {
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
    
    observeEvent(input$close_decision_modal, {
      removeModal()
      shinyjs::runjs("document.body.setAttribute('data-bs-overflow', 'auto');")
    })
    
    output$decision_rule_div <- renderUI({
      req("auto_decision_adjust" %in% credentials$privileges[[user$role]])
      tagList(
        div(
          style = "display: flex",
          span("Select Category Colors", style = "font-size: x-large; font-weight: bold"),
          actionButton(ns("col_reset"), label = icon("refresh"), class = "btn-circle-sm", style = "margin-left: auto;")
        ),
        br(),
        div(style = "display: flex; flex-wrap: wrap; margin-left: 1.5%; margin-right: 1.5%",
          col_divs(), 
          actionButton(ns("submit_color"), width = "100%", "Apply Colors")
        ),
        hr(),
        br(),
        div(
          style = "display: flex;",
          span("Automate Decisions by Metric Value or Risk Score", style = "font-size: x-large; font-weight: bold"),
          actionButton(ns("auto_reset"), label = icon("refresh"), class = "btn-circle-sm", style = "margin-left: auto;")
        ),
        br(),
        div(style = "margin-left: 1.5%; margin-right: 1.5%",
          checkboxGroupInput(ns("auto_include"), label = NULL, decision_lst,
                           selected = isolate(auto_current()), inline = TRUE)
        ),
        div(style = "margin-left: 5%; margin-right: 2.5%",
          dec_divs()
        ),
        br(),
        div(style = "margin-left: 2%; margin-right: 2%",
          div(
            style = "display: flex;",
            span("Rule List", style = "font-size: large; font-weight: bold"),
            actionButton(ns("add_rule"), label = icon("plus"), class = "btn-square-sm", style = "margin-left: auto;")
          ),
          br(),
          div(
            id = ns("rules_list"),
            rule_divs(),
            style = "margin-left: 2%; margin-right: 2%"
          )
        ),
        br(),
        div(style = "margin-left: 1.5%; margin-right: 1.5%", 
            actionButton(ns("submit_auto"), width = "100%", "Apply Decision Rules")
        ),
        sortable::sortable_js(
          ns("rules_list"),
          sortable::sortable_options(
            handle = ".rule_handle",
            onUpdate = sortable::sortable_js_capture_input(ns("rules_order")),
            onLoad = sortable::sortable_js_capture_input(ns("rules_order"))
          )
        )
      )
    })

    output$auto_settings <-
      renderUI({
        req("auto_decision_adjust" %in% credentials$privileges[[user$role]])
        
        div(
          style = "float: right;",
          actionButton(ns("auto_dropdown"), label = icon("gear"), class = "btn-circle", style = "margin-left: auto;"),
          tags$script(glue::glue("$('#{ns(\"auto_dropdown\")}').tooltip({{placement: 'left', title: 'Click here to add/adjust decision automation rules.', html: false, trigger: 'hover'}});"))
        )
      })
    
    
    output$auto_settings2 <-
      renderUI({
        req("auto_decision_adjust" %in% credentials$privileges[[user$role]])
        
        div(
          style = "float: right;",
          actionButton(ns("auto_dropdown2"), label = icon("gear"), class = "btn-circle", style = "margin-left: auto;"),
          tags$script(glue::glue("$('#{ns(\"auto_dropdown2\")}').tooltip({{placement: 'left', title: 'Click here to edit the decision category table.', html: false, trigger: 'hover'}});"))
        )
      })
    
    output$modal_dec_cat_table <- 
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
    
    output$modal_rule_table <- 
      DT::renderDataTable({
        out_lst <- purrr::compact(reactiveValuesToList(rule_lst)[isolate(input$rules_order)])
        DT::datatable({
          out_lst %>% 
            purrr::map_dfr(~ dplyr::as_tibble(.x[c("metric", "filter", "decision")]) %>% dplyr::mutate(metric = if (is.na(metric)) "Risk Score" else metric))
        },
        escape = FALSE,
        class = "cell-border",
        selection = 'none',
        colnames = c("Metric", "Conditional", "Decision"),
        rownames = FALSE,
        options = list(
          dom = "t",
          searching = FALSE,
          sScrollX = "100%",
          iDisplayLength = -1,
          ordering = FALSE
        ))
      })
    
    output$modal_col_table <- 
      DT::renderDataTable({
        
        mod_tbl <-
          dplyr::tibble(
            decision = decision_lst,
            old_color = color_current(),
            new_color = color_updated()
          )
        
        formattable::as.datatable(
          formattable::formattable(
            mod_tbl,
            list(
              old_color = formattable::formatter(
                "span",
                style = x ~ formattable::style(display = "block",
                                               "border-radius" = "4px",
                                               "padding-right" = "4px",
                                               "font-weight" = "bold",
                                               "color" = purrr::map_chr(x, get_text_color),
                                               "background-color" = x)),
              new_color = formattable::formatter(
                "span",
                style = x ~ formattable::style(display = "block",
                                               "border-radius" = "4px",
                                               "padding-right" = "4px",
                                               "font-weight" = "bold",
                                               "color" = purrr::map_chr(x, get_text_color),
                                               "background-color" = x))
            )),
          colnames = c("Category", "Old Color", "New Color"),
          rownames = FALSE,
          options = list(
            ordering = FALSE,
            pageLength = -1,
            dom = 't',
            columnDefs = list(list(className = 'dt-center', targets = "_all"))
          ),
          style = "default"
        ) %>%
          DT::formatStyle(names(mod_tbl), textAlign = 'center')
      })
    
    observeEvent(input$submit_auto, {
      req("auto_decision_adjust" %in% credentials$privileges[[user$role]])
      
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
            if (!rlang::is_empty(purrr::compact(reactiveValuesToList(rule_lst)))) {
              tagList(
                DT::DTOutput(ns("modal_rule_table")),
                if (!rlang::is_empty(purrr::compact(reactiveValuesToList(auto_decision)))) DT::DTOutput(ns("modal_dec_cat_table"))
              )
            } else {
              h2("Disable Decision Automation")
            },
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
    
    observeEvent(input$submit_color, {
      req("auto_decision_adjust" %in% credentials$privileges[[user$role]])
      
      showModal(modalDialog(
        size = "l",
        easyClose = TRUE,
        h5("Change Decision Category Colors", style = 'text-align: center !important'),
        hr(),
        br(),
        fluidRow(
          column(
            width = 12,
            'Please confirm your chosen color palette: ',
            br(),
            DT::DTOutput(ns("modal_col_table")),
            br(),
            br()
          )
        ),
        br(),
        footer = tagList(
          actionButton(ns('confirm_submit_col'), 'Submit'),
          actionButton(ns('cancel'), 'Cancel')
        )))
    })
    
    # Close modal if user cancels decision submission.
    observeEvent(input$cancel, {
      removeModal()
      shinyjs::click("auto_dropdown")
    })
    
    observeEvent(input$confirm_submit_auto, {
      req("auto_decision_adjust" %in% credentials$privileges[[user$role]])
      
      out_lst <- purrr::compact(reactiveValuesToList(auto_decision))
      dbUpdate("UPDATE decision_categories SET lower_limit = NULL, upper_limit = NULL")
      purrr::iwalk(out_lst, ~ dbUpdate("UPDATE decision_categories SET lower_limit = {.x[1]}, upper_limit = {.x[2]} WHERE decision = {.y}"))
      auto_decision_update(out_lst)
      
      if (length(out_lst) == 0) {
        loggit::loggit("INFO", glue::glue("Decision automation rules have been disabled by {user$name} ({user$role})."))
      } else {
        rules <- out_lst %>%
          purrr::imap_chr(~ glue::glue("{.y} = ({.x[[1]]}, {.x[[2]]}]"))
        loggit::loggit("INFO", glue::glue("The following decision rules were implemented by {user$name} ({user$role}): {paste(rules, collapse = '; ')}."))
      }
      
      
      risk_rule_update(reactiveValuesToList(rule_lst)[input$rules_order])
      rule_out <-
        purrr::map(risk_rule_update(), ~ {
          metric_id <- dbSelect("select id from metric where name == {.x$metric}")[[1]] %>% 
            ifelse(test = length(.) == 0, yes = 0)
          decision_id <- dbSelect("select id from decision_categories where decision == {.x$decision}")[[1]]%>% 
            ifelse(test = length(.) == 0, yes = 0)
          glue::glue_sql("({metric_id}, {.x$filter}, {decision_id})", .con = DBI::dbConnect(RSQLite::SQLite()))
        }) %>%
        glue::glue_collapse(", ")
      dbUpdate("DELETE FROM rules")
      dbUpdate(glue::glue("INSERT INTO rules (metric_id, filter, decision_id) VALUES {rule_out};"))
      
      removeModal()
      shinyjs::runjs("document.body.setAttribute('data-bs-overflow', 'auto');")
    })
    
    observeEvent(purrr::walk(decision_lst, ~input[[glue::glue("{risk_lbl(.x, type = 'attribute')}_col")]]), {
      color_updated({
        decision_lst %>%
          purrr::map_chr(~ input[[glue::glue("{risk_lbl(.x, type = 'attribute')}_col")]]) %>%
          purrr::set_names(decision_lst)
      })
    }, ignoreInit = TRUE)
      
    
    observeEvent(input$confirm_submit_col, {
      req("auto_decision_adjust" %in% credentials$privileges[[user$role]])
      
      selected_colors <- 
        decision_lst %>%
        purrr::map_chr(~ input[[glue::glue("{risk_lbl(.x, type = 'attribute')}_col")]]) %>%
        purrr::set_names(decision_lst)
      purrr::iwalk(selected_colors, ~ {
        dbUpdate("UPDATE decision_categories SET color = {.x} WHERE decision = {.y}")
        shinyjs::runjs(glue::glue("document.documentElement.style.setProperty('--{risk_lbl(.y, type = 'attribute')}-color', '{.x}');"))
        })
      loggit::loggit("INFO", glue::glue("The decision category display colors were modified by {user$name} ({user$role})"))
      color_current(selected_colors)
      
      removeModal()
      shinyjs::runjs("document.body.setAttribute('data-bs-overflow', 'auto');")
    })
    
    observe({
      dec_table({
        dbSelect("SELECT decision, color, lower_limit, upper_limit FROM decision_categories") %>%
          dplyr::mutate(lower_limit = dplyr::if_else(is.na(lower_limit), "-", as.character(lower_limit)),
                        upper_limit = dplyr::if_else(is.na(upper_limit), "-", as.character(upper_limit)))
        
      })
    }) %>%
      bindEvent(auto_decision_update(), color_current())
    
    return(list(rules = risk_rule_update, colors = color_current))
  })
}

## To be copied in the UI
# mod_decision_automation_ui("decision_automation_1")

## To be copied in the server
# mod_decision_automation_server("decision_automation_1")
