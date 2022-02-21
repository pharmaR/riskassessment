# IntroJS.
introJSServer(id = "upload_pkg_introJS", text = upload_pkg)

#' Save all the uploaded packages, marking them as 'new', 'not found', or
#' 'duplicate'.
uploaded_pkgs <- reactive({
  req(input$uploaded_file)
  
  if(is.null(input$uploaded_file$datapath))
    validate('Please upload a nonempty CSV file.')
  
  uploaded_pkgs <- read_csv(input$uploaded_file$datapath)
  template <- read_csv(file.path('Data', 'upload_format.csv'))
  
  if(nrow(uploaded_pkgs) == 0)
    validate('Please upload a nonempty CSV file.')
  
  if(!all(colnames(packages) == colnames(template)))
    validate("Please upload a CSV with a valid format.")
  
  waitress <- waiter::Waitress$new(
    max = 3*nrow(uploaded_pkgs) + 4,
    theme = 'overlay-percent')
  on.exit(waitress$close())
  
  waitress$inc(1)
  
  names(uploaded_pkgs) <- tolower(names(uploaded_pkgs))
  uploaded_pkgs$package <- trimws(uploaded_pkgs$package)
  uploaded_pkgs$version <- trimws(uploaded_pkgs$version)
  
  waitress$inc(2)
  
  # Current packages on the db.
  curr_pkgs <- dbSelect("SELECT name FROM package")
  
  waitress$inc(1)
  
  # Save the uploaded packages that were not in the db.
  new_pkgs <- uploaded_pkgs %>% filter(!(package %in% curr_pkgs$name))
  
  if(nrow(new_pkgs) != 0){
    for (pkg in new_pkgs$package) {
      # Get and upload pkg general info to db.
      insert_pkg_info_to_db(pkg)
      waitress$inc(1)
      # Get and upload maintenance metrics to db.
      insert_maintenance_metrics_to_db(pkg)
      waitress$inc(1)
      # Get and upload community metrics to db.
      insert_community_metrics_to_db(pkg)
      waitress$inc(1)
    }
  }
  
  all_pkgs <- dbSelect("SELECT name FROM package")
  
  # Data frame indicating which packages where duplicate, new, and not found.
  uploaded_pkgs <- uploaded_pkgs %>%
    mutate(status = case_when(
      !(package %in% all_pkgs$name) ~ 'not found',
      package %in% curr_pkgs$name ~ 'duplicate',
      TRUE ~ 'new')
    )
  
  loggit("INFO",
         paste("Uploaded file:", input$uploaded_file$name, 
               "Total Packages:", nrow(uploaded_pkgs$package),
               "New Packages:", sum(uploaded_pkgs$status == 'new'),
               "Undiscovered Packages:", sum(uploaded_pkgs$status == 'not found'),
               "Duplicate Packages:", sum(uploaded_pkgs$status == 'duplicate')),
         echo = FALSE)
  
  uploaded_pkgs
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
  req(uploaded_pkgs())
  as.character(tagList(
    br(), br(),
    hr(),
    h5("Summary of uploaded package (s)"),
    br(),
    p(tags$b("Total Packages: "), nrow(uploaded_pkgs())),
    p(tags$b("New Packages: "), sum(uploaded_pkgs()$status == 'new')),
    p(tags$b("Undiscovered Packages: "), sum(uploaded_pkgs()$status == 'not found')),
    p(tags$b("Duplicate Packages: "), sum(uploaded_pkgs()$status == 'duplicate')),
    p("Note: The assessment will be performed on the latest version of each
        package, irrespective of the uploaded version.")
  ))
})

# Uploaded packages table.
output$upload_pkgs_table <- DT::renderDataTable({
  req(uploaded_pkgs())
  
  datatable(
    uploaded_pkgs(),
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
