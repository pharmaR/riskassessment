
# IntroJS.
introJSServer(id = "upload_pkg_introJS", text = upload_pkg)

# Load the columns from DB into reactive values.
observeEvent(list(input$total_new_undis_dup,input$uploaded_file), {
  req(values$upload_complete == "upload_complete")
  
  # After upload complete, update db dash screen with new package(s)
  values$db_pkg_overview <- update_db_dash()
  
  if (input$total_new_undis_dup == "All") {
    values$Total_New_Undis_Dup <- values$Total
  } else if (input$total_new_undis_dup == "New") {
    values$Total_New_Undis_Dup <- values$New
  } else if (input$total_new_undis_dup == "Undiscovered") {
    values$Total_New_Undis_Dup <- values$Undis
  } else if (input$total_new_undis_dup == "Duplicates") {
    values$Total_New_Undis_Dup <- values$Dup
  } 
}, ignoreInit = TRUE)  # End of the observe.

# 2. Observe to disable the input widgets while the packages uploading into DB.

observeEvent(input$uploaded_file, {
  # req(input$uploaded_file)
  values$uploaded_file_status <- file_upload_error_handling(input$uploaded_file)
  if (values$uploaded_file_status != "no_error") {
    shinyjs::hide("upload_summary_text")
    shinyjs::hide("upload_summary_select")
    shinyjs::hide("total_new_undis_dup_table")
    reset("uploaded_file") 
    return()
  } else{
    shinyjs::show("upload_summary_text")
    shinyjs::show("upload_summary_select")
    shinyjs::show("total_new_undis_dup_table")
  }
  file_to_read <- input$uploaded_file
  pkgs_file <-
    read.csv(file_to_read$datapath,
             sep = ",",
             stringsAsFactors = FALSE)
  names(pkgs_file) <- tolower(names(pkgs_file))
  pkgs_file$package <- trimws(pkgs_file$package)
  pkgs_file$version <- trimws(pkgs_file$version)
  
  values$Total <- pkgs_file
  pkgs_db1 <- db_fun("SELECT name FROM package")
  values$Dup <- filter(values$Total, values$Total$package %in% pkgs_db1$name)
  values$New <- filter(values$Total, !(values$Total$package %in% pkgs_db1$name))
  withProgress(message = "Uploading Packages to DB:", value = 0, {
    if (nrow(values$New) != 0) {
      for (i in 1:nrow(values$New)) {
        incProgress(1 / (nrow(values$New) + 1), detail = values$New[i, 1])
        new_package<-values$New$package[i]
        get_packages_info_from_web(new_package)
        metric_mm_tm_Info_upload_to_DB(new_package)
        metric_cum_Info_upload_to_DB(new_package)
      }
    }
  })
  
  pkgs_db2 <- db_fun("SELECT name FROM package")
  
  values$Undis <-
    filter(values$New, !(values$New$package %in% pkgs_db2$name))
  values$packsDB <- pkgs_db2
  
  updateSelectizeInput(
    session,
    "select_pack",
    choices = c("Select", values$packsDB$name),
    selected = "Select"
  )
  
  showNotification(id = "show_notification_id", "Upload completed", type = "message")
  values$upload_complete <- "upload_complete"
  
  # Show the download reports buttons after all the packages have been loaded
  # and the information extracted.
  loggit("INFO", paste("Summary of the uploaded file:",input$uploaded_file$name, 
                       "Total Packages:", nrow(values$Total),
                       "New Packages:", nrow(values$New),
                       "Undiscovered Packages:", nrow(values$Undis),
                       "Duplicate Packages:", nrow(values$Dup)), echo = FALSE)
}, ignoreInit = TRUE)  # End of the Observe.

# 1. Render Output to download the sample format dataset.
output$upload_format_download <- downloadHandler(
  filename = function() {
    paste("Upload_file_structure", ".csv", sep = "")
  },
  content = function(file) {
    write.csv(read_csv(file.path("Data", "upload_format.csv")), file, row.names = F)
  }
)

# 2. Render Output to show the summary of the uploaded csv into application.

output$upload_summary_text <- renderText({
  if (values$upload_complete == "upload_complete") {
    as.character(tagList(
      br(), br(),
      hr(),
      h5("Summary of uploaded package(s)"),
      br(),
      p(tags$b("Total Packages: "), nrow(values$Total)),
      p(tags$b("New Packages: "), nrow(values$New)),
      p(tags$b("Undiscovered Packages: "), nrow(values$Undis)),
      p(tags$b("Duplicate Packages: "), nrow(values$Dup)),
      p("Note: The assessment will be performed on the latest version of each
        package, irrespective of the uploaded version.")
    ))
  }
})

# 3. Render Output to show the select input to select the choices to display the table.

output$upload_summary_select <- renderUI({
  if (values$upload_complete == "upload_complete") {
    removeUI(selector = "#Upload")
    selectInput(
      "total_new_undis_dup",
      "",
      choices = c("All", "New", "Undiscovered", "Duplicates")
    )
  } 
})  # End of the render Output.

# 4. Render Output to show the data table of uploaded csv.

output$total_new_undis_dup_table <- DT::renderDataTable({
  if (values$upload_complete == "upload_complete") {
    datatable(
      values$Total_New_Undis_Dup,
      escape = FALSE,
      class = "cell-border",
      selection = 'none',
      extensions = 'Buttons',
      options = list(
        searching = FALSE,
        sScrollX = "100%",
        lengthChange = FALSE,
        aLengthMenu = list(c(5, 10, 20, 100,-1), list('5', '10', '20', '100', 'All')),
        iDisplayLength = 5
      )
    )
  }
}) # End of the render Output 
# End of the Render Output's'.

# View sample dataset
observeEvent(input$upload_format, {
  dataTableOutput("sampletable")
  
  showModal(modalDialog(
    output$sampletable <- DT::renderDataTable(
      datatable(
        read_csv(file.path("Data", "upload_format.csv")),
        escape = FALSE,
        class = "cell-border",
        editable = FALSE,
        filter = "none",
        selection = 'none',
        extensions = 'Buttons',
        options = list(
          sScrollX = "100%",
          aLengthMenu = list(c(5, 10, 20, 100, -1), list('5', '10', '20', '100', 'All')),
          iDisplayLength = 5,
          dom = 't'
        )
      )
    ),
    downloadButton("upload_format_download", "Download")
  ))
})
