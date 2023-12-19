#' downloadHandler UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
mod_downloadHandler_button_ui <- function(id, multiple = TRUE){
  ns <- NS(id)
  tagList(
    actionButton(ns("create_reports"), if (multiple) "Create Report(s)" else "Create Report")
  )
}

#' downloadHandler UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
mod_downloadHandler_filetype_ui <- function(id){ 
  # will want to change this to input_UI so we can include additional items
  # such as "Include comments" checkboxes for summary, maintmetrics, comm usage, and overall comments
  ns <- NS(id)
  tagList(
    div(style = 'width:230px;',
      selectInput(ns("report_format"), "Select Format", c("html", "docx", "pdf"))
    )
  )
}

#' downloadHandler Inlcude UI Function
#'
#' @description A shiny Module.
#'
#' @param id Internal parameters for {shiny}.
#'
#' @noRd 
mod_downloadHandler_include_ui <- function(id){
  # will want to change this to input_UI so we can include additional items
  # such as "Include comments" check boxes for summary, maint-metrics, comm usage, and overall comments
  uiOutput(NS(id, "mod_downloadHandler_incl_output"))
}
#' downloadHandler Include Server Function
#'
#' @description A shiny Module.
#'
#' @param id Internal parameters for {shiny}.
#' 
#' @importFrom shiny showModal modalDialog
#' @importFrom glue glue
#' @importFrom shinyWidgets prettyCheckboxGroup updatePrettyCheckboxGroup
#'
#' @noRd 
mod_downloadHandler_include_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$mod_downloadHandler_incl_output <- renderUI({
      div(
        strong(p("Elements to include:")),
        div(align = 'left', class = 'twocol', style = 'margin-top: 0px;',
            shinyWidgets::prettyCheckboxGroup(
              ns("report_includes"), label = NULL, inline = FALSE,
              choices = rpt_choices, selected = isolate(session$userData$user_report$report_includes) %||% rpt_choices
            )
        )
      )
    })
    
    # save user selections and notify user
    observeEvent(input$store_prefs, {
      writeLines(
        session$userData$user_report$report_includes, 
        session$userData$user_report$user_file
      )

      shiny::showModal(shiny::modalDialog(title = "User preferences saved",
                                          "Report preferences stored for user", 
                                          footer = modalButton("Dismiss"), 
                                          easyClose = TRUE))
    }, ignoreInit = TRUE)
    
    observeEvent(session$userData$trigger_events$update_report_pref_inclusions, {
      shinyWidgets::updatePrettyCheckboxGroup(
        session,
        "report_includes",
        selected = session$userData$user_report$report_includes
      )
    })
    
    observeEvent(input$report_includes, {
      session$userData$user_report$report_includes <- input$report_includes %||% ""
    }, ignoreNULL = FALSE, ignoreInit = TRUE)
    
    return(reactive(input$report_includes))
  })
}
  
#' downloadHandler Server Functions
#'
#' @noRd 
mod_downloadHandler_server <- function(id, pkgs, user, metric_weights){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    observe({
      if(isTruthy(pkgs())) {
        shinyjs::enable("create_reports")
      } else {
        shinyjs::disable("create_reports")
      }
    })
    download_file <- reactiveValues()
    
    observeEvent(input$create_reports, {
      
      n_pkgs <- length(pkgs())
      req(n_pkgs > 0)
      
      download_file$filename <- {
        if (n_pkgs > 1) {
          report_datetime <- stringr::str_replace_all(stringr::str_replace(get_time(), " ", "_"), ":", "-")
          glue::glue('RiskAssessment-Report-{report_datetime}.zip')
        } else {
          pkg_ver <- dbSelect("SELECT version FROM package WHERE name = {pkgs()}")
          glue::glue('{pkgs()}_{pkg_ver}_Risk_Assessment.{input$report_format}')
        }
      }
      
      # progress <- shiny::Progress$new(max = n_pkgs + 2)
      # progress$set(message = glue::glue('Downloading {ifelse(n_pkgs > 1, paste0(n_pkgs, " "), "")}Report{ifelse(n_pkgs > 1, "s", paste0(": ", pkgs()))}'),
      #              value = 0)
      # on.exit(progress$close())
      # 
      # updateProgress <- function(amount = 1, detail = NULL) {
      #   progress$inc(amount = amount, detail = detail)
      # }
      # 
      # download_file$filepath <- 
      #   report_creation(pkgs(), metric_weights(), input$report_format, input$report_includes, reactiveValuesToList(user), updateProgress)
      
      download_file$background <-
        callr::r_bg(
          report_creation,
          list(pkg_lst = pkgs(), metric_weights = metric_weights(),
               report_format = input$report_format, report_includes = input$report_includes,
               user = reactiveValuesToList(user), 
               db_name = golem::get_golem_options('assessment_db_name'), my_tempdir = tempdir()),
          user_profile = FALSE,
          package = "riskassessment"
        )
    })
    
    observe({
      req(download_file$background)
      
      if (download_file$background$is_alive()) {
        invalidateLater(1000, session)
      } else {
        browser()
        download_file$filepath <- download_file$background$get_result()
      }
      cat("triggered\n")
    })
    
    observeEvent(download_file$filepath,{
      showNotification("Reports created",
                       action = downloadLink(ns("download_reports")),
                       duration = NULL)
    })
    
    output$download_reports <- downloadHandler(
      filename = function() {
        basename(download_file$filename)
      },
      content = function(file) {
        file.copy(
          from = download_file$filepath,
          to = file
        )
      }
    )
  })
}

## To be copied in the UI
# mod_downloadHandler_button_ui("downloadHandler_1")

## To be copied in the UI
# mod_downloadHandler_filetype_ui("downloadHandler_1")
    
## To be copied in the server
# mod_downloadHandler_server("downloadHandler_1")
