# Upload package.
output$upload_package <- renderUI({
  fluidPage(
    br(), br(),
    
    introJSUI("upload_pkg_introJS"),
    
    fluidRow(
      column(
        width = 4,
        fileInput(
          inputId = "uploaded_file",
          label = "Choose a CSV file",
          accept = ".csv",
          placeholder = "No file selected"),
        
        actionLink("upload_format", "View Sample Dataset")
      )
    ),
    
    # Display the summary information of the uploaded csv.
    fluidRow(column(width = 12, htmlOutput("upload_summary_text"))),
    
    fluidRow(
      column(width = 6, uiOutput("upload_summary_select")),
      # Display the table with total rows in the DB.
      column(width = 6, dataTableOutput("total_new_undis_dup_table"))
    )
  )
})