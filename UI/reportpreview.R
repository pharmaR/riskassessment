# Render Output UI for Report Preview.
output$report_preview <- renderUI({
  req(selected_pkg$name())
  
  if(selected_pkg$name() == "-")
    showSelectPackageMessage()
  
  else {
    removeUI(selector = "#Upload")
    
    shiny::tagList(
      br(),
      introJSUI("report_introJS"),
      br(), br(),
      fluidRow(
        fluidRow(
          id = "dwnld_rp",
          tags$div(
            selectInput("report_format", "Select Format", c("html", "docx")),  # Select input to select the format for report.
          ),
          tags$div(
            downloadButton("download_report_btn", "Download Report", class = "download_report_btn_class btn-secondary"),  # Download button to export the report.
          ),
        ),
        fluidRow(
          id = "rep_prev",
          column(
            width = 12,
            htmlOutput("gen_info"),  # Display General Information of the selected Package.
            htmlOutput("decision_display"),  # Display the status of the Decision of a selected Package.
            h3(tags$b(paste0('Overall Comments(',nrow(values$comment_o2),'):'))),
            fluidRow(
              column(
                width = 12,
                align = "left",
                htmlOutput("overall_comments")  # Display the overall comment for selected Package. 
              )
            ),
            source(file.path("UI", "mm_report.R"), local = TRUE)$value,
            source(file.path("UI", "cum_report.R"), local = TRUE)$value,
            # source(file.path("UI", "tm_report.R"), local = TRUE)$value
          )
        )
      )
    )
  }
})