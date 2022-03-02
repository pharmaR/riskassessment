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
    
    uiOutput("upload_summary")
  )
})