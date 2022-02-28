# IntroJS.
introJSServer(id = "upload_pkg_introJS", text = upload_pkg)

upload_complete <- reactiveVal(FALSE)

#' Save all the uploaded packages, marking them as 'new', 'not found', or
#' 'duplicate'.
uploaded_pkgs <- reactive({
  req(input$uploaded_file)
  
  upload_complete(FALSE)
  shinyjs::disable("uploaded_file")
  
  if(is.null(input$uploaded_file$datapath))
    validate('Please upload a nonempty CSV file.')
  
  uploaded_pkgs <- read_csv(input$uploaded_file$datapath)
  template <- read_csv(file.path('Data', 'upload_format.csv'))
  
  if(nrow(uploaded_pkgs) == 0)
    validate('Please upload a nonempty CSV file.')
  
  if(!all(colnames(packages) == colnames(template)))
    validate("Please upload a CSV with a valid format.")
  
  uploaded_pkgs <- callr::r_bg(
    func = asynch_upload,
    args = list(uploaded_pkgs),
    supervise = TRUE
  )
  
  uploaded_pkgs
})

upload_check <- reactive({
  req(uploaded_pkgs(), upload_complete() == FALSE)
  invalidateLater(1000, session)
  
  if (!uploaded_pkgs()$is_alive())
    upload_complete(TRUE)

  !uploaded_pkgs()$is_alive()
})

observe({
  upload_complete()
  if (upload_complete())
    cat("Uploading packages complete!\n")
})

observe({
  upload_check()
  if (!upload_check())
    cat("Uploading packages in the background...\n")
})

observeEvent(req(upload_complete()), {
  
  loggit("INFO",
         paste("Uploaded file:", input$uploaded_file$name,
               "Total Packages:", nrow(uploaded_pkgs()$get_result()$package),
               "New Packages:", sum(uploaded_pkgs()$get_result()$status == 'new'),
               "Undiscovered Packages:", sum(uploaded_pkgs()$get_result()$status == 'not found'),
               "Duplicate Packages:", sum(uploaded_pkgs()$get_result()$status == 'duplicate')),
         echo = FALSE)
  
  shinyjs::enable("uploaded_file")
})

# Download the sample dataset.
output$download_sample <- downloadHandler(
  filename = function() {
    paste("template", ".csv", sep = "")
  },
  content = function(file) {
    write.csv(read_csv(file.path("Data", "upload_format.csv")), file, row.names = F)
  }
)

# Uploaded packages summary.
output$upload_summary_text <- renderText({
  req(upload_complete())

  as.character(tagList(
    br(), br(),
    hr(),
    h5("Summary of uploaded package (s)"),
    br(),
    p(tags$b("Total Packages: "), nrow(uploaded_pkgs()$get_result())),
    p(tags$b("New Packages: "), sum(uploaded_pkgs()$get_result()$status == 'new')),
    p(tags$b("Undiscovered Packages: "), sum(uploaded_pkgs()$get_result()$status == 'not found')),
    p(tags$b("Duplicate Packages: "), sum(uploaded_pkgs()$get_result()$status == 'duplicate')),
    p("Note: The assessment will be performed on the latest version of each
        package, irrespective of the uploaded version.")
  ))
})

# Uploaded packages table.
output$upload_pkgs_table <- DT::renderDataTable({
  req(upload_complete())
  
  datatable(
    uploaded_pkgs()$get_result(),
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
})

# View sample dataset.
observeEvent(input$upload_format, {
  dataTableOutput("sampletable")
  
  showModal(modalDialog(
    size = "l",
    easyClose = TRUE,
    footer = "",
    h5("Sample Dataset", style = 'text-align: center !important'),
    hr(),
    br(),
    fluidRow(
      column(
        width = 12,
        output$sampletable <- DT::renderDataTable(
          datatable(
            read_csv(file.path("Data", "upload_format.csv")),
            escape = FALSE,
            editable = FALSE,
            filter = 'none',
            selection = 'none',
            extensions = 'Buttons',
            options = list(
              sScrollX = "100%",
              aLengthMenu = list(c(5, 10, 20, 100, -1), list('5', '10', '20', '100', 'All')),
              iDisplayLength = 5,
              dom = 't'
            )
          )))
    ),
    br(),
    fluidRow(column(align = 'center', width = 12, downloadButton("download_sample", "Download")))
  ))
})
