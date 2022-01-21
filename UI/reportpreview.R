# Render Output UI for Report Preview.
output$report_preview<-renderUI({
  Sys.sleep(0.1)
  if (!is.null(values$packsDB$name) &&
      !identical(values$packsDB$name, character(0))) {
    if (input$select_pack != "-") {
      removeUI(selector = "#Upload")
      
      shiny::tagList(
        br(),
        div(actionBttn("help_rp", "Need help?", color = "primary",
                       icon = icon("far fa-star"),
                       block = FALSE, style = "simple", size = "sm")),
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
    # Show the select the package message if user not selected any package from dropdown in the application.
    else{
      fluidRow(
        class = "",
        id = "Upload",
        column(
          width = 12,
          align = "center",
          class = "",
          h1("Please select a package")
        )
      )
    }
  }
  # Show the upload a list of R packages message if application not loaded the pacakges from DB.
  else{
    fluidRow(
      class = "",
      id = "Upload",
      column(
        width = 12,
        align = "center",
        class = "",
        h1("Please upload a list of R packages to proceed")
      )
    )
  }
})