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
  if(!is.null(input$db_pkgs_rows_selected))
    shinyjs::enable("dwnld_sel_db_pkgs_btn")
  else
    shinyjs::disable("dwnld_sel_db_pkgs_btn")
})


values$cwd <- getwd()


# Download handler to create a report for each package selected.
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

metrics_weight <- reactive({
  input$update_weight
  get_metric_weights()
})

output$weights_table <- DT::renderDataTable({
  
  as.datatable(
    formattable(metrics_weight()),
    selection = list(mode = 'single'),
    colnames = c("Name", "Weight"),
    rownames = FALSE,
    options = list(
      searching = FALSE,
      lengthChange = FALSE,
      pageLength = 10,
      columnDefs = list(list(className = 'dt-center'))
    )
  )
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
          br(), br(), br(),
          fluidRow(
            column(width = 4,
                   h3("Update weights"),
                   selectInput("metric_name", "Select metric", metrics_weight()$name, selected = metrics_weight()$name[1]),
                   numericInput("metric_weight", "Choose new weight", min = 0, value = metrics_weight()$weight[1]),
                   actionButton("update_weight", "Update weight"),
                   br(), br(), br(), br(), br(), br(),
                   h3("Update risk and weights for all packages"),
                   actionButton("update_pkgwt", "Update"),
                   br(), br(), br(), br(), br(), br(),
                   downloadButton("dwnld_package_db_btn",
                                  "Download packages snapshot?",
                                  class = "download_report_btn_class btn-secondary")
                   ),
            column(width = 8,
                   h3("View/select metrics"),
                   dataTableOutput("weights_table"))
          ),
          br(),
          fluidRow(
            column(width = 12, 
                   h5(em("Note: Changing the weights of the metrics will not update the
               risk of the packages on the database. Assessments of 
               future packages will use these new weights.
               ")))
          )
      )
    )
  )
})

# Update metric weight dropdown so that it matches the metric name.
observeEvent(input$metric_name, {
  # Display the weight of the selected metric.
  updateNumericInput(session, "metric_weight",
                     value = metrics_weight() %>%
                       filter(name == input$metric_name) %>%
                       select(weight) %>%
                       pull())
})

# Update metric name dropdown based on the selected row on the table.
# Note that another of the observeEvents will update the metric weight after
# the selected metric name is updated.
observeEvent(input$weights_table_rows_selected, {
  updateSelectInput(session, "metric_name",
                     selected = metrics_weight()$name[input$weights_table_rows_selected])
})



# Save new weight into db.
observeEvent(input$update_weight, {
  validate(
    need(is.numeric(input$metric_weight), "Please select a valid numeric weight.")
  )
  
  update_metric_weight(input$metric_name, input$metric_weight)
})

# Save new weight into db.
observeEvent(input$update_pkgwt, {

  showModal(tags$div(
    id = "confirmation_id",
    modalDialog(
      title = h2("Confirm Decision", class = "mb-0 mt-0 txt-color"),
      h2("Please confirm your decision", class = "mt-0"),
      h3(strong("Note:"), "Updating the package weights and risk metrics cannot be reverted.", class = "mt-25 mb-0"),
      footer = tagList(
        actionButton("confirm_update_weights", "Submit",
                     class = "submit_confirmed_decision_class btn-secondary"),
        actionButton("edit", "Cancel", class = "edit_class btn-unsuccess")
      )
      )
  ))
  
}, ignoreInit = TRUE)

observeEvent(input$confirm_update_weights, {
  removeModal()

  # Reset any decisions made prior to this.
  db_ins(paste0("UPDATE package SET decision = ''"))
  
  values$db_pkg_overview <- update_db_dash()

  # add a comment on every tab saying how the risk and weights
  # changed, and that the comments, final decision may no longer be 
  # applicable. 
  overall_comments <- paste("Since the package weights and risk have changed",
        "the overall comments and final decision may no longer be applicable")

  cmts_db <- db_fun("select distinct comm_id as package_name from Comments")
  
  # clear out any prior overall comments
  db_ins("delete from Comments where comment_type = 'o'")
  
  # insert new overall comments
  for (i in 1:nrow(cmts_db)) {
  db_ins(
    paste0(
      "INSERT INTO Comments values('", cmts_db$package_name[i], "',",
      "'", values$name, "'," ,
      "'", values$role, "',",
      "'", overall_comments, "',",
      "'o',",
      "'", TimeStamp(), "'" ,
      ")"
    )
  )
  }

  #	Write to the log file
  loggit("INFO", paste("package weights and risks will be updated for all packages"))
  
  # update for each package
  pkg <- db_fun("select distinct name as package_name from package")
  
  withProgress(message = "Updating package weights and scores \n", value = 0, {
  for (i in 1:nrow(pkg)) {
    incProgress(1 / (nrow(pkg) + 1), detail = pkg$package_name[i])
    db_ins(paste0("delete from package_metrics where package_id = ", 
                  "(select id from package where name = ","'", pkg$package_name[i], "')") )
    metric_mm_tm_Info_upload_to_DB(pkg$package_name[i])
   }
  })
  showNotification(id = "show_notification_id", "Updates completed", type = "message")
  
}, ignoreInit = TRUE)

output$dwnld_package_db_btn <- downloadHandler(
  
    filename = function() {
      paste("package-table-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      packagedb <- db_fun("select * from package")
      write.csv(packagedb, file)
    }
)
