# Upload package.
output$upload_package <- renderUI({
  fluidPage(
    br(), br(),
    
    introJSUI("upload_pkg_introJS"),
    
    tags$head(tags$style(".shiny-notification {font-size:30px; color:darkblue; position: fixed; width:500px; height: 150px; top: 75% ;right: 10%;")),

    fluidRow(
      column(
        width = 4,
        div(id = "upload-file-grp",
          fileInput(
            inputId = "uploaded_file",
            label = "Choose a CSV file",
            accept = ".csv",
            placeholder = "No file selected"
          )
        ),
        actionLink("upload_format", "View Sample Dataset")
      )
    ),
    
    # Display the summary information of the uploaded csv.
    fluidRow(column(width = 12, htmlOutput("upload_summary_text"))),
    
    # Summary of packages uploaded.
    fluidRow(column(width = 12, dataTableOutput("upload_pkgs_table")))
  )
})