###############################################################################
# db_dash_screen - Server functions for the database dashboard.
# Author: Aaron Clark
# Date: September 22nd, 2020
# License: MIT License
###############################################################################


# Create table for the db dashboard.
output$db_pkgs <- DT::renderDataTable({
  
  values$db_pkg_overview <- update_db_dash() %>%
    mutate(last_comment = as.character(as_datetime(last_comment))) %>%
    mutate(last_comment = ifelse(is.na(last_comment), "-", last_comment)) %>%
    mutate(decision = ifelse(decision != "", paste(decision, "Risk"), "-")) %>%
    mutate(was_decision_made = ifelse(decision != "-", TRUE, FALSE)) %>%
    select(name, version, score, was_decision_made, decision, last_comment)
  
  as.datatable(
    formattable(
      values$db_pkg_overview,
      list(
        score = formatter(
          "span",
          style = x ~ style(display = "block",
                            "border-radius" = "4px",
                            "padding-right" = "4px",
                            "font-weight" = "bold",
                            "color" = "white",
                            "order" = x,
                            "background-color" = csscolor(
                              colfunc(100)[round(as.numeric(x)*100)]))),
        decision = formatter(
          "span",
          style = x ~ style(display = "block",
                            "border-radius" = "4px",
                            "padding-right" = "4px",
                            "font-weight" = "bold",
                            "color" = "white",
                            "background-color" = 
                              ifelse(x == "High Risk", high_risk_color,
                                     ifelse(x == "Medium Risk", med_risk_color,
                                            ifelse(x == "Low Risk", low_risk_color, "transparent"))))),
        was_decision_made = formatter("span",
                                      style = x ~ style(color = ifelse(x, "#0668A3", "gray")),
                                      x ~ icontext(ifelse(x, "ok", "remove"), ifelse(x, "Yes", "No")))
      )),
    selection = list(mode = 'multiple'),
    colnames = c("Package", "Version", "Score", "Decision Made?", "Decision", "Last Comment"),
    rownames = FALSE,
    options = list(
      searching = FALSE,
      lengthChange = FALSE,
      #dom = 'Blftpr',
      pageLength = 15,
      lengthMenu = list(c(15, 60, 120, -1), c('15', '60', '120', "All")),
      columnDefs = list(list(className = 'dt-center'))
    )
  ) %>%
    formatStyle(names(values$db_pkg_overview), textAlign = 'center')
})

# Enable the download button when a package is selected.
observe({
  if(!is.null(input$db_pkgs_rows_selected)) {
    shinyjs::enable("dwnld_sel_db_pkgs_btn")
  } else {
    shinyjs::disable("dwnld_sel_db_pkgs_btn")
  }
})


# Download handler to create a report for each package selected.
values$cwd <- getwd()
output$dwnld_sel_db_pkgs_btn <- downloadHandler(
  filename = function() {
    paste(
      "RiskAsses_PkgDB_Dwnld",
      str_replace_all(
        str_replace(Sys.time(), " ", "_"), ":", "-"), ".zip", sep = "_")
  },
  content = function(file) {
    these_pkgs <- values$db_pkg_overview %>% slice(input$db_pkgs_rows_selected)
    n_pkgs <- nrow(these_pkgs)
    req(n_pkgs > 0)
    shiny::withProgress(
      message = paste0("Downloading ", n_pkgs, " Report", ifelse(n_pkgs > 1, "s", "")),
      value = 0,
      max = n_pkgs + 2, # Tell the progress bar the total number of events.
      {
        shiny::incProgress(1)
        
        my_dir <- tempdir()
        if (input$report_formats == "html") {
          Report <- file.path(my_dir, "Report_html.Rmd")
          file.copy("Reports/Report_html.Rmd", Report, overwrite = TRUE)
        } else {
          Report <- file.path(my_dir, "Report_doc.Rmd")
          file.copy("Reports/Report_doc.Rmd", Report, overwrite = TRUE)
        }
        fs <- c()
        for (i in 1:n_pkgs) {
          # Grab package name and version, then create filename and path.
          this_pkg <- these_pkgs$name[i]
          this_ver <- these_pkgs$version[i]
          file_named <- paste0(this_pkg,"_",this_ver,"_Risk_Assessment.",input$report_formats)
          path <- file.path(my_dir, file_named)
          # Render the report, passing parameters to the rmd file.
          rmarkdown::render(
            input = Report,
            output_file = path,
            params = list(package = this_pkg,
                          version = this_ver,
                          cwd = values$cwd)
          )
          fs <- c(fs, path)  # Save all the reports/
          shiny::incProgress(1) # Increment progress bar.
        }
        # Zip all the files up. -j retains just the files in zip file.
        zip(zipfile = file, files = fs, extras = "-j")
        shiny::incProgress(1) # Icrement progress bar.
      })
  },
  contentType = "application/zip"
)


# Bring back user to the main dashboard.
observeEvent(input$back2dash, {
  values$current_screen <- "dashboard_screen"
  updateSelectizeInput(session, "select_pack", selected=input$select_pack)
  updateSelectizeInput(session, "select_ver" , selected=input$select_ver)
})


# Manage admin adjusting weights and recaluclating risk scores
# initialize temporary df that keeps track of the current and new weights exactly once
values$curr_new_wts <- get_metric_weights()

observeEvent(input$update_weight, {
  values$curr_new_wts <-
    values$curr_new_wts %>%
    mutate(new_weight = ifelse(name == isolate(input$metric_name),
                               isolate(input$metric_weight), new_weight))
})

output$weights_table <- DT::renderDataTable({
  
  all_names <- unique(values$curr_new_wts$name)
  chgd_wt_names <- values$curr_new_wts %>% filter(weight != new_weight) %>% pull(name)
  my_colors <- ifelse(all_names %in% chgd_wt_names,'#FFEB9C','#FFFFFF')
  
  DT::datatable(
    values$curr_new_wts,
    selection = list(mode = 'single'),
    colnames = c("Name", "Current Weight", "New Weight"),
    rownames = FALSE,
    options = list(
      searching = FALSE,
      lengthChange = FALSE,
      pageLength = -1,
      columnDefs = list(list(className = 'dt-center', targets = 1:2))
    )
  ) %>%
  DT::formatStyle(names(values$curr_new_wts),lineHeight='80%') %>%
  formatStyle(columns =  "name", target = 'row',
              backgroundColor = styleEqual(all_names, my_colors))
  
})


# Section displayed only for authorized users.
output$admins_view <- renderUI({
  req(res_auth$admin == TRUE)  # show this only if user is an admin
  tagList(
    tags$section(
      br(), br(),
      box(width = 12, collapsible = TRUE, status = "primary",
          title = h3("View/Change Weights", style = "margin-top: 5px"),
          solidHeader = TRUE,
          br(),
          fluidRow(
            column(width = 5, offset = 5, align = "left",
                   h3("Set new weights:"),
            )),
          fluidRow(
            column(width = 2, offset = 5, align = "left",
                   selectInput("metric_name", "Select metric", values$curr_new_wts$name, selected = values$curr_new_wts$name[1]) ),
            column(width = 2, align = "left",
                   numericInput("metric_weight", "Choose new weight", min = 0, value = values$curr_new_wts$weight[1]) ),
            column(width = 1,
                   br(),
                   actionButton("update_weight", "Update weight", class = "btn-secondary") ) ),
          br(), br(), 
          fluidRow(
            column(width = 3, offset = 1, align = "center",
                   
                   br(), br(), br(), 
                   tags$hr(class = "hr_sep"),
                   br(), br(),
                   
                   h3("Download database"),
                   downloadButton("download_database_btn",
                                  "Download",
                                  class = "btn-secondary"),
                   
                   br(), br(), br(), 
                   tags$hr(class = "hr_sep"),
                   br(), br(),
                   
                   h3("Apply new weights and re-calculate risk for each package"),
                   actionButton("update_pkg_risk", "Re-calculate", class = "btn-secondary")
                   
            ),
            column(width = 6, style = "border: 1px solid rgb(77, 141, 201)",
                   offset = 1,
                   h3("Current Risk Score Weights by Metric"),
                   dataTableOutput("weights_table"))
          ),
          br(), br(), br(),
          fluidRow(
            column(width = 12,
                   h5(em("Note: Changing the weights of the metrics will not update the
               risk of the packages on the database until 'Re-calculate' button is selected.
               ")))
          )
      )
    )
  )
})


# make sure "Re-calculate" button is disbaled if no weights have changed. Need to make
# sure renderUI exists, so we put a req() on metric_name input and also a .5 second delay
# on the disable/ enable functions to give renderUI enough time to re-render
n_wts_chngd <- reactive({
  req(input$metric_weight)
  
  values$curr_new_wts %>%
    filter(weight != new_weight) %>%
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
  # Display the weight of the selected metric.
  updateNumericInput(session, "metric_weight",
                     value = values$curr_new_wts %>%
                       filter(name == input$metric_name) %>%
                       select(weight) %>% # current weight
                       pull())
})


# Update metric name dropdown based on the selected row on the table.
# Note that another of the observeEvents will update the metric weight after
# the selected metric name is updated.
observeEvent(input$weights_table_rows_selected, {
  updateSelectInput(session, "metric_name",
                     selected = values$curr_new_wts$name[input$weights_table_rows_selected])
})





# Save new weight into db.
observeEvent(input$update_pkg_risk, { 
  
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
          actionButton("confirm_update_risk", "Submit",
                       class = "submit_confirmed_decision_class btn-secondary"),
          actionButton("edit", "Cancel", class = "edit_class btn-unsuccess")
        )
      )
    ))
  }
  
}, ignoreInit = TRUE)

# Upon confirming the risk re-calculation
observeEvent(input$confirm_update_risk, {
  removeModal()

  # Update the weights in the `metric` table to reflect recent changes
  # First, which weights are different than the originals?
  wt_chgd_df <- 
    values$curr_new_wts %>%
    filter(weight != new_weight)
  
  wt_chgd_metric <- wt_chgd_df %>% select(name) %>% pull()
  wt_chgd_wt <- wt_chgd_df %>% select(new_weight) %>% pull()
  message("Metrics & Weights changed...")
  message(paste(wt_chgd_metric, ": ", wt_chgd_wt))
  purrr::walk2(wt_chgd_metric, wt_chgd_wt, update_metric_weight)
  values$curr_new_wts <- get_metric_weights() # reset the current and new wts from the database

  
  # update for each package
  all_pkgs <- db_fun("select distinct name as pkg_name from package")
  cmt_or_dec_pkgs <- unique(bind_rows(
    db_fun("select distinct comm_id as pkg_name from Comments where comment_type = 'o'"),
    db_fun("select distinct name as pkg_name from package where decision != ''")
    ))
  
  cmt_or_dec_dropped_cmt <- " Since they may no longer be applicable, the final decision & comment have been dropped to allow for re-evaluation."
  
  # clear out any prior overall comments
  db_ins("delete from Comments where comment_type = 'o'")
  
  for (i in 1:nrow(all_pkgs)) {
  # insert comment for both mm and cum tabs
    for (typ in c("mm","cum")) {
      db_ins(
        paste0(
          "INSERT INTO Comments values('", all_pkgs$pkg_name[i], "',",
          "'", values$name, "'," ,
          "'", values$role, "',",
          "'", paste0(weight_risk_comment(all_pkgs$pkg_name[i]), 
                 ifelse(all_pkgs$pkg_name[i] %in% cmt_or_dec_pkgs$pkg_name, cmt_or_dec_dropped_cmt, "")), "',",
          "'", typ, "',",
          "'", TimeStamp(), "'" ,
          ")"
          )
       )
    }
  }
  
  # Reset any decisions made prior to this.
  pkg <- db_fun("select distinct name as pkg_name from package where decision != ''")
  for (i in 1:nrow(pkg)) {
    db_ins(paste0("UPDATE package SET decision = '' where name = '",pkg$pkg_name[i],"'"))
  }
  
  # want to update db_dash_screen after creating automated comments and
  # resetting final decisions. This allows us to capture old risk score in
  # database and the timestamp of the last automated comment that was just added
  values$db_pkg_overview <- update_db_dash()
  
  #	Write to the log file
  loggit("INFO", paste("package weights and risk metric scores will be updated for all packages"))

  # update for each package
  pkg <- db_fun("select distinct name as pkg_name from package")
  
  withProgress(message = "Applying weights and updating risk scores \n", value = 0, {
  for (i in 1:nrow(pkg)) {
    incProgress(1 / (nrow(pkg) + 1), detail = pkg$pkg_name[i])
    db_ins(paste0("delete from package_metrics where package_id = ", 
                  "(select id from package where name = ","'", pkg$pkg_name[i], "')") )
    metric_mm_tm_Info_upload_to_DB(pkg$pkg_name[i])
  }
  })

  showNotification(id = "show_notification_id", "Updates completed", type = "message")
  
  
}, ignoreInit = TRUE)



# Download the database
output$download_database_btn <- downloadHandler(
  
  filename = function() {
    paste0("datase_backup-", Sys.Date(), ".sqlite")
  },
  content = function(file) {
    con <- dbConnect(RSQLite::SQLite(), database_name)
    cbk <- dbConnect(RSQLite::SQLite(), file)
    RSQLite::sqliteCopyDatabase(con, cbk)
    dbDisconnect(con)
    dbDisconnect(cbk)
    
    showModal(tags$div(
      id = "confirmation_id",
      modalDialog(
        size = "l",
        title = h2("Database downloaded", class = "mb-0 mt-0 txt-color"),
        h3("The database as been downloaded as datase_backup-[date].sqlite"))))
  }
)
