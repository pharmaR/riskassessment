# Module to upload package.
uploadPackageUI <- function(id) {
  fluidPage(
    br(), br(),
    
    introJSUI(NS(id, "upload_pkg_introJS")),
    
    fluidRow(
      column(
        width = 4,
        div(id = "upload-file-grp",
            fileInput(
              inputId = NS(id, "uploaded_file"),
              label = "Choose a CSV file",
              accept = ".csv",
              placeholder = "No file selected"
            )
        ),
        actionLink(NS(id, "upload_format"), "View Sample Dataset")
      )
    ),
    
    # Display the summary information of the uploaded csv.
    fluidRow(column(width = 12, htmlOutput(NS(id, "upload_summary_text")))),
    
    # Summary of packages uploaded.
    fluidRow(column(width = 12, dataTableOutput(NS(id, "upload_pkgs_table"))))
  )
}

uploadPackageServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Determine which guide to use for IntroJS.
    upload_pkg_txt <- reactive({
      req(uploaded_pkgs())
      
      if(nrow(uploaded_pkgs()) > 0) 
        upload_pkg_complete 
      else 
        upload_pkg
    })
    
    # Start introjs when help button is pressed. Had to do this outside of
    # a module in order to take a reactive data frame of steps
    observeEvent(
      input[["upload_pkg_introJS-help"]], # notice input contains "id-help"
      introjs(session,
              options = list(
                steps = 
                  upload_pkg_txt() %>%
                  union(sidebar_steps),
                "nextLabel" = "Next",
                "prevLabel" = "Previous"
              )
      )
    )
    
    #' Save all the uploaded packages, marking them as 'new', 'not found', or
    #' 'duplicate'.
    uploaded_pkgs <- reactive({
      req(input$uploaded_file)
      
      if(is.null(input$uploaded_file$datapath))
        validate('Please upload a nonempty CSV file.')
      
      uploaded_packages <- read_csv(input$uploaded_file$datapath, show_col_types = FALSE)
      
      if(nrow(uploaded_packages) == 0)
        validate('Please upload a nonempty CSV file.')
      
      template <- read_csv(file.path('Data', 'upload_format.csv'), show_col_types = FALSE)
      
      if(!all(colnames(uploaded_packages) == colnames(template)))
        validate("Please upload a CSV with a valid format.")
      
      waitress <- waiter::Waitress$new(
        max = 3*nrow(uploaded_packages) + 1,
        theme = 'overlay-percent')
      on.exit(waitress$close())
      
      waitress$inc(1)
      
      # Add status/source columns and remove white space around package names.
      uploaded_packages <- uploaded_packages %>%
        mutate(
          status = rep('', nrow(uploaded_packages)),
          source = rep('', nrow(uploaded_packages))
        ) %>%
        mutate(package = trimws(package))
      
      for (i in 1:nrow(uploaded_packages)) {
        waitress$inc(1)
        ref <- riskmetric::pkg_ref(uploaded_packages$package[i])
        
        if (ref$source == "pkg_missing"){
          uploaded_packages$status[i] <- 'not found'
          
          loggit('WARNING',
                 glue('Package {ref$name} was flagged by riskmetric as {ref$source}.'))
          
          waitress$inc(1)
        }
        else {
          # Save version and source.
          uploaded_packages$version[i] <- as.character(ref$version)
          uploaded_packages$source[i] <- as.character(ref$source)
          
          found <- nrow(dbSelect(glue(
            "SELECT name
            FROM package
            WHERE name = '{uploaded_packages$package[i]}'")))
          
          uploaded_packages$status[i] <- ifelse(found == 0, 'new', 'duplicate')
          
          # Add package and metrics to the db if package is not in the db.
          if(!found) {
            # Get and upload pkg general info to db.
            insert_pkg_info_to_db(uploaded_packages$package[i])
            waitress$inc(1)
            # Get and upload maintenance metrics to db.
            insert_maintenance_metrics_to_db(uploaded_packages$package[i])
            waitress$inc(1)
            # Get and upload community metrics to db.
            insert_community_metrics_to_db(uploaded_packages$package[i])
          }
        }
      }
      
      uploaded_packages
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
      req(uploaded_pkgs)
      req(nrow(uploaded_pkgs()) > 0)
      
      loggit("INFO",
             paste("Uploaded file:", input$uploaded_file$name, 
                   "Total Packages:", nrow(uploaded_pkgs()),
                   "New Packages:", sum(uploaded_pkgs()$status == 'new'),
                   "Undiscovered Packages:", sum(uploaded_pkgs()$status == 'not found'),
                   "Duplicate Packages:", sum(uploaded_pkgs()$status == 'duplicate')),
             echo = FALSE)
      
      as.character(tagList(
        br(), br(),
        hr(),
        h5("Summary of uploaded package(s)"),
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
      req(nrow(uploaded_pkgs()) > 0)
      
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
          aLengthMenu = list(c(5, 10, 20, 100, -1), list('5', '10', '20', '100', 'All')),
          iDisplayLength = 5
        )
      )
    })
    
    # View sample dataset.
    observeEvent(input$upload_format, {
      dataTableOutput(NS(id, "sampletable"))
      
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
        fluidRow(column(align = 'center', width = 12,
                        downloadButton(NS(id, "download_sample"), "Download")))
      ))
    })
    
    list(
      names = uploaded_pkgs
    )
  })
}