# Upload package.
output$upload_package <- renderUI({
  fluidPage(
    br(), br(),
    
    addHelpButton("help"),
    
    fluidRow(
      style = "text-align: left;",
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
        actionLink("upload_format", "View Sample Dataset",
                   class = "sample_dataset_link")
      )
    ),
    
    # Display the summary information of the uploaded csv.
    htmlOutput("upload_summary_text"),
    
    fluidRow(
      style = "padding-left: 50px; padding-bottom: 10px",
      column(width = 6,
             div(id = "upload_summary",
                 div(class = "row col-sm-12 mb-4 u_p_dropdown_row",
                     uiOutput("upload_summary_select")),
                 # Display the table with total rows in the DB.
                 dataTableOutput("total_new_undis_dup_table"))
      )
    )
  )
})