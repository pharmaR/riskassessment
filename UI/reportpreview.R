#####################################################################################################################
# reportpreview.R - Report Preview to display the information of the selected package. 
# Author: K Aravind Reddy
# Date: July 13th, 2020
# License: MIT License
#####################################################################################################################

# Start of the report Preview Source file for UI Module.

# Render Output UI for Report Preview.

output$report_preview<-renderUI({
 Sys.sleep(0.1)
  if (!is.null(values$packsDB$package) &&
      !identical(values$packsDB$package, character(0))) {
  if (input$select_pack != "Select") {
    removeUI(selector = "#Upload")
    fluidRow(
      class = "mt-4 r_p_main_row",
      
      fluidRow(
        class="float-right r_p_format_row",
        tags$div(
          class="col-sm W-40 text-left float-right",
          selectInput("report_format", "Select Format", c("html", "docx")),  # Select input to select the format for report.
        ),
        tags$div(
          class="col-sm float-right",
          downloadButton("download_report_btn", "Download Report", class = "download_report_btn_class btn-secondary"),  # Download button to export the report.
        ),
      ),
      fluidRow(
        column(
          width = 12,
          class = "text-left",
          htmlOutput("gen_info"),  # Display General Information of the selected Package.
          htmlOutput("decision_display"),  # Display the status of the Decision of a selected Package.
          h3(tags$b(paste0('Overall Comments(',nrow(values$comment_o2),'):'))),
          fluidRow(
            class = "overall-comments-row",
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
  }
   # Show the select the package message if user not selected any package from dropdown in the application. 
   
   else{
    fluidRow(
      div(style = "height:150px;"),
      class = "",
      id = "Upload",
      column(
        width = 12,
        align = "center",
        class = "",
        h1("Please select the Package")
      )
    )
  }
 }
  # Show the upload a list of R packages message if application not loaded the pacakges from DB.

  else{
  fluidRow(
    div(style = "height:150px;"),
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
})  # End of the Render Output

# End of the Report Preview Source file for UI Module.
