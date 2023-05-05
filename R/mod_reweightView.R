#' UI for the 'Re-weight View' module
#' 
#' @param id the module id
#' @keywords internal
#' 
reweightViewUI <- function(id) {
  tagList(
    uiOutput(NS(id, "reweights_view"))
  )
}

#' Server logic for the 'Re-weight View' module
#' 
#' 
#' @param id the module id
#' @param user the user name
#' @param decision_list the list containing the decision automation criteria
#' 
#' 
#' @import dplyr
#' @importFrom DT datatable formatStyle styleEqual renderDataTable
#' @importFrom shinyjs enable disable delay
#' @importFrom shinydashboard box
#' @importFrom DBI dbConnect dbDisconnect
#' @importFrom RSQLite SQLite sqliteCopyDatabase
#' 
#' @keywords internal
reweightViewServer <- function(id, user, decision_list) {
  moduleServer(id, function(input, output, session) {
    
    exportTestValues(
      save = {
        reactiveValuesToList(save)
      },
      curr_new_wts = {
        curr_new_wts()
      }
    )
    
    save <- reactiveValues(data=NULL)
    
    curr_new_wts <- reactiveVal(
      get_metric_weights() %>%
        dplyr::mutate(new_weight = weight) %>%
        dplyr::mutate(weight = ifelse(name == "covr_coverage", 0, weight)))
    
    observeEvent(input$update_weight, {
      req(user$role == "admin")
      curr_new_wts(save$data %>%
                     dplyr::mutate(new_weight = ifelse(name == isolate(input$metric_name),
                                                       isolate(input$metric_weight), new_weight)))
    })
    
    observeEvent(curr_new_wts(), {
      save$data <- isolate(curr_new_wts())
    })
    
    output$weights_table <- DT::renderDataTable({
      
      all_names <- unique(curr_new_wts()$name)
      chgd_wt_names <- curr_new_wts() %>% dplyr::filter(weight != new_weight) %>% dplyr::pull(name)
      my_colors <- ifelse(all_names %in% chgd_wt_names,'#FFEB9C', '#FFFFFF')
      
      DT::datatable(
        curr_new_wts(),
        selection = list(mode = 'single'),
        colnames = c("Name", "Current Weight", "New Weight"),
        rownames = FALSE,
        options = list(
          dom = "t",
          searching = FALSE,
          lengthChange = FALSE,
          pageLength = -1,
          ordering = FALSE,
          columnDefs = list(list(className = 'dt-center', targets = 1:2))
        )
      ) %>%
        DT::formatStyle(names(curr_new_wts()),lineHeight='80%') %>%
        DT::formatStyle(columns =  "name", target = 'row',
                        backgroundColor = DT::styleEqual(all_names, my_colors))
    })
    
    # Section displayed only for authorized users.
    output$reweights_view <- renderUI({
      tagList(
        tags$section(
          br(), br(),
          shinydashboard::box(width = 12, status = "primary",
              title = h2("View/Change Weights", style = "margin-top: 5px", align = "center"),
              solidHeader = TRUE,
              br(),
              fluidRow(
                column(width = 5, offset = 5, align = "left",
                       h3("Set new weights:"),
                )),
              fluidRow(
                column(width = 2, offset = 5, align = "left",
                       selectInput(NS(id, "metric_name"), "Select metric", curr_new_wts()$name, selected = curr_new_wts()$name[1]) ),
                column(width = 2, align = "left",
                       numericInput(NS(id, "metric_weight"), "Choose new weight", min = 0, value = curr_new_wts()$new_weight[1]) ),
                column(width = 1,
                       br(),
                       actionButton(NS(id, "update_weight"), "Update weight", class = "btn-secondary") ) ),
              br(), br(), 
              fluidRow(
                column(width = 3, offset = 1, align = "center",
                       
                       br(), br(), br(), 
                       tags$hr(class = "hr_sep"),
                       br(), br(),
                       
                       h3("Download database"),
                       downloadButton(NS(id, "download_database_btn"),
                                      "Download",
                                      class = "btn-secondary"),
                       
                       br(), br(), br(), 
                       tags$hr(class = "hr_sep"),
                       br(), br(),
                       
                       h3("Apply new weights and re-calculate risk for each package"),
                       actionButton(NS(id, "update_pkg_risk"), "Re-calculate", class = "btn-secondary")
                       
                ),
                column(width = 6, style = "border: 1px solid rgb(77, 141, 201)",
                       offset = 1,
                       h3("Current Risk Score Weights by Metric", align = "center"),
                       DT::dataTableOutput(NS(id, "weights_table")))
              ),
              br(), br(), br(),
              conditionalPanel("input.metric_name === 'covr_coverage'",
                               ns = NS(id),
                               fluidRow(
                                 column(1),
                                 column(width = 10, h5(em("Note: the 'covr_coverage' metric is currently disabled (weight = 0) until the 'riskmetric' package returns a non-NA value for this metric. 
               "), style = "color: red;"), align = "center"),
                                 column(1)
                               ),
                               br()
              ),
              fluidRow(
                column(width = 1),
                column(width = 10,
                       h5(em("Note: Changing the weights of the metrics will not update the
               risk of the packages on the database until 'Re-calculate' button is selected.
               ")), align = "center"),
                column(width = 1)
              ),
              br(), br()
          )
        )
      )
    })
    
    metric_weight <- debounce(reactive(input$metric_weight), 500)
    
    observeEvent(input$metric_weight, {
      shinyjs::disable("update_weight")
    })
    observeEvent(metric_weight(), {
      req(input$metric_name)
      
      if (input$metric_name == "covr_coverage" && (is.na(metric_weight()) || metric_weight() != 0)) {
        updateNumericInput(session, "metric_weight", value = 0)
      } else if (is.na(metric_weight()) || metric_weight() < 0) {
        updateNumericInput(session, "metric_weight", value = 0)
      } else if (metric_weight() != curr_new_wts() %>%
                 dplyr::filter(name == input$metric_name) %>%
                 dplyr::pull(new_weight)){
        shinyjs::enable("update_weight")
      }
    })
    
    # make sure "Re-calculate" button is disabled if no weights have changed. Need to make
    # sure renderUI exists, so we put a req() on metric_name input and also a .5 second delay
    # on the disable/ enable functions to give renderUI enough time to re-render
    n_wts_chngd <- reactive({
      req(input$metric_weight)
      
      curr_new_wts() %>%
        dplyr::filter(weight != new_weight) %>%
        nrow()
    })
    
    observe({
      if(n_wts_chngd() > 0){
        shinyjs::delay(500, shinyjs::enable("update_pkg_risk"))
      } else {
        shinyjs::delay(500, shinyjs::disable("update_pkg_risk"))
      }
    })

    
    # Update metric weight dropdown so that it matches the metric name.
    observeEvent(input$metric_name, {
      req(user$role == "admin")
      
      if(input$metric_name == "covr_coverage"){
        # set to zero, don't allow change until riskmetric fixes this assessment
        updateNumericInput(session, "metric_weight",
                           value = 0, min = 0, max = 0)
      } else {
        updateNumericInput(session, "metric_weight",
                           value = curr_new_wts() %>%
                             dplyr::filter(name == input$metric_name) %>%
                             dplyr::select(new_weight) %>% # new weight
                             dplyr::pull())
      }
      
    })
    
    
    # Update metric name dropdown based on the selected row on the table.
    # Note that another of the observeEvents will update the metric weight after
    # the selected metric name is updated.
    observeEvent(input$weights_table_rows_selected, {
      req(user$role == "admin")
      updateSelectInput(session, "metric_name",
                        selected = curr_new_wts()$name[input$weights_table_rows_selected])
    })
    
    # Save new weight into db.
    observeEvent(input$update_pkg_risk, {
      req(user$role == "admin")
      
      # if you the user goes input$back2dash, then when they return to the 
      if(n_wts_chngd() == 0){
        showModal(modalDialog(
          size = "l",
          title = h2("No Weights changed", class = "mb-0 mt-0 txt-color"),
          h3("Risk scores already calculated with current weights. To change, please select & alter a weight for at least one metric on the right.")
        ))
      } else {
        showModal(tags$div(
          id = "confirmation_id",
          modalDialog(
            size = "l",
            title = h2("Confirm Decision", class = "mb-0 mt-0 txt-color"),
            h3("Once you push the submit button:",
               tags$ul(
                 tags$li("The new package weights will be applied and risk metric scores re-calculated."),
                 tags$li("The risk re-calculation will be logged as a comment for each package."),
                 tags$li("Previously finalized decisions & overall comments will be dropped for re-evaluation.")
               )
            ),
            h3(strong("Note:"), "Updating the risk metrics cannot be reverted.", class = "mt-25 mb-0"),
            h3("Its strongly recommended to 'Download database' for backup purposes before re-calculating risk."),
            footer = tagList(
              actionButton(NS(id, "confirm_update_risk"), "Submit",
                           class = "submit_confirmed_decision_class btn-secondary"),
              modalButton("Cancel")
            )
          )
        ))
      }
      
    }, ignoreInit = TRUE)
    
    # Upon confirming the risk re-calculation
    observeEvent(input$confirm_update_risk, {
      req(user$role == "admin")
      removeModal()
      
      # Update the weights in the `metric` table to reflect recent changes
      # First, which weights are different than the originals?
      wt_chgd_df <- 
        curr_new_wts() %>%
        dplyr::filter(weight != new_weight)
      
      wt_chgd_metric <- wt_chgd_df %>% dplyr::select(name) %>% dplyr::pull()
      wt_chgd_wt <- wt_chgd_df %>% dplyr::select(new_weight) %>% dplyr::pull()
      rlang::inform("Metrics & Weights changed...")
      rlang::inform(paste(wt_chgd_metric, ": ", wt_chgd_wt))
      purrr::walk2(wt_chgd_metric, wt_chgd_wt, ~update_metric_weight(.x, .y))
      
      # update for each package
      all_pkgs <- dbSelect("SELECT DISTINCT name AS pkg_name FROM package")
      cmt_or_dec_pkgs <- unique(dplyr::bind_rows(
        dbSelect("SELECT DISTINCT id AS pkg_name FROM comments where comment_type = 'o'"),
        dbSelect("SELECT DISTINCT name AS pkg_name FROM package where decision_id IS NOT NULL")
      ))
      
      cmt_or_dec_dropped_cmt <- " Since they may no longer be applicable, the final decision & comment have been dropped to allow for re-evaluation."
      
      # clear out any prior overall comments
      dbUpdate("DELETE FROM comments WHERE comment_type = 'o'")
      
      for (i in 1:nrow(all_pkgs)) {
        # insert comment for both mm and cum tabs
        for (typ in c("mm","cum")) {
          dbUpdate(
            "INSERT INTO comments VALUES({all_pkgs$pkg_name[i]}, {user$name},
            {user$role}, {paste0(weight_risk_comment(all_pkgs$pkg_name[i]), 
                          ifelse(all_pkgs$pkg_name[i] %in% cmt_or_dec_pkgs$pkg_name, cmt_or_dec_dropped_cmt, ''))},
            {typ}, {getTimeStamp()})"
          )
        }
      }
      
      # Reset any decisions made prior to this.
      pkg <- dbSelect("SELECT DISTINCT name AS pkg_name FROM package WHERE decision_id IS NOT NULL")
      if (nrow(pkg) > 0) {
        for (i in 1:nrow(pkg)) {
          dbUpdate("UPDATE package SET decision_id = NULL where name = {pkg$pkg_name[i]}")
        }
      }
      
      #	Write to the log file
      loggit::loggit("INFO", paste("package weights and risk metric scores will be updated for all packages"))
      
      # update for each package
      pkg <- dbSelect("SELECT DISTINCT name AS pkg_name FROM package")
      
      withProgress(message = "Applying weights and updating risk scores:", value = 0, {
        shinyjs::runjs("$('.shiny-notification').css('width', '450px');")
        shinyjs::runjs("$('<br>').insertAfter('.progress-message');")
        for (i in 1:nrow(pkg)) {
          incProgress(1 / (nrow(pkg) + 1), detail = pkg$pkg_name[i])
          dbUpdate(
            "DELETE FROM package_metrics WHERE package_id = 
            (SELECT id FROM package WHERE name = {pkg$pkg_name[i]})") 
          # metric_mm_tm_Info_upload_to_DB(pkg$pkg_name[i])
          insert_riskmetric_to_db(pkg$pkg_name[i])
          if (!rlang::is_empty(decision_list())) {
              assign_decisions(decision_list(), pkg$pkg_name[i])
          }
        }
      })
      
      showNotification(id = "show_notification_id", "Updates completed", type = "message", duration = 1)
      shinyjs::runjs("$('.shiny-notification').css('width', '450px');")
      
      curr_new_wts(
        get_metric_weights() %>%
          dplyr::mutate(new_weight = weight) %>%
          dplyr::mutate(weight = ifelse(name == "covr_coverage", 0, weight)))
      
      user$metrics_reweighted <- user$metrics_reweighted + 1
    }, ignoreInit = TRUE)
    
    
    # Download the database
    output$download_database_btn <- downloadHandler(
      
      filename = function() {
        glue::glue("datase_backup-{Sys.Date()}.sqlite")
      },
      content = function(file) {
        con <- DBI::dbConnect(RSQLite::SQLite(), golem::get_golem_options('assessment_db_name'))
        cbk <- DBI::dbConnect(RSQLite::SQLite(), file)
        RSQLite::sqliteCopyDatabase(con, cbk)
        DBI::dbDisconnect(con)
        DBI::dbDisconnect(cbk)
        
        showModal(tags$div(
          id = "confirmation_id",
          modalDialog(
            size = "l",
            title = h2("Database downloaded", class = "mb-0 mt-0 txt-color"),
            h3("The database has been downloaded as datase_backup-[date].sqlite"))))
      }
    )
    
    # Return metric weights. Doing so guarantees that when a report is
    # downloaded, it would have the latest metric weights.
    return(reactive(curr_new_wts() %>% dplyr::select(-new_weight)))
  })
}
