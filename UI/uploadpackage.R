#####################################################################################################################
# uploadpackage.R - Upload Package to choose the csv file from local and display the upload csv summary
#                   and show the tables for new,duplicate and total observations from DB.
# Author: K Aravind Reddy
# Date: July 13th, 2020
# License: MIT License
#####################################################################################################################

# Start of the upload package Source file for UI Module.

# Render Output UI for upload package.

output$upload_package <- renderUI({
 fluidRow(
  class = "u_p_main_row",
  tags$div(class = "row col-sm-12 u_p_heading_row",
           tags$h2("Upload list of R Packages")),
  fluidRow(
    style = "text-align: left",
    column(
      width = 4,
      tags$h4("Choose a CSV File", class = "chooseCSVtext"),
      
      # file input to choose the csv from local to application.
      fileInput(
        "uploaded_file",
        "",
        accept = ".csv",
        placeholder = "No file selected"
      ),

      # Click on Button to view the sample format dataset to upload.
      actionLink("upload_format", "View Sample Dataset", class = "sample_dataset_link")
    )
  ),
  
  # Display the summary information of the uploaded csv.
  htmlOutput("upload_summary_text"),
  
  fluidRow(
    column(width = 6,
           div(class = "row col-sm-12 mb-4 u_p_dropdown_row",
               uiOutput("upload_summary_select")),
           # Display the table with total rows in the DB.
           dataTableOutput("total_new_undis_dup_table"))
  ),
  
  fluidRow(
    column(width = 6,
           br(),
           div(
             class="col-sm W-40 text-left float-right",
             shinyjs::hidden(
               selectInput("all_reports_format", "Select Format", c("html", "docx")))),
           div(
             class="col-sm float-right",
             shinyjs::hidden(
               downloadButton("dwnld_all_reports_btn", "Download All Reports",
                              class = "download_report_btn_class btn-secondary")))
    )
  )
 )
})

# End of the upload pacakge Source file for UI module.