
# Determine which guide to use for IntroJS.
upload_pkg_txt <- reactive({
  req(values$uploaded_pkgs)
  if(nrow(values$uploaded_pkgs) > 0) upload_pkg_complete else upload_pkg
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

#' Save all the uploaded packages, 
#' marking them as 'new', 'not found', or 'duplicate'.
observeEvent(input$uploaded_file, {

  if(is.null(input$uploaded_file$datapath))
    validate('Please upload a nonempty CSV file.')
  

  uploaded_packages <- read_csv(input$uploaded_file$datapath, show_col_types = FALSE)
  template <- read_csv(file.path('Data', 'upload_format.csv'), show_col_types = FALSE)

  
  if(nrow(uploaded_packages) == 0)
    validate('Please upload a nonempty CSV file.')
  
  if(!all(colnames(packages) == colnames(template)))
    validate("Please upload a CSV with a valid format.")
  
  names(uploaded_packages) <- tolower(names(uploaded_packages))
  uploaded_packages$package <- trimws(uploaded_packages$package)
  
  # use the same url dbupload.R is using to get community usage metrics
  url <- "https://cran.r-project.org/web/packages/available_packages_by_name.html"
  # open page
  con <- url(url, "rb")
  webpage = rvest::read_html(con)
  
  pkgnames <- webpage %>% 
    html_nodes("a") %>%
    html_text() %>% 
    stringr::str_replace_all(.,"/","")
  
  # Drop First 26 values on webpage since they are "A" through "Z"
  CRAN_avail_pkgs <- pkgnames[27:length(pkgnames)] # Drop A-Z
  close(con)
  rm(webpage, pkgnames)

  j <- rep(TRUE, nrow(uploaded_packages))
  
  for (i in seq_along(uploaded_packages$package)) {
    ref <- riskmetric::pkg_ref(uploaded_packages$package[i])
    # add version and source
    suppressWarnings(uploaded_packages$version[i] <- as.character(ref$version))
    suppressWarnings(uploaded_packages$source[i]  <- ref$source)
    if (ref$source == "pkg_missing") {
      rlang::inform(glue("NOTE: package ", {ref$name}, 
                         " was flagged by riskmetric as '",{ref$source},"' and will be removed. Did you misspell it?"))
      j[i] <- FALSE
    } else {
    if (!ref$name %in% CRAN_avail_pkgs) {
      rlang::inform(glue("NOTE: package ", {ref$name},
                         " was not found on the CRAN so it will be removed from upload list."))
      j[i] <- FALSE
     }
    }
  }

  # drop the non-existent packages
  uploaded_packages <- uploaded_packages[which(j),]
  # drop large string
  rm(CRAN_avail_pkgs)

  
  # Current packages on the db.
  curr_pkgs <- dbSelect("SELECT name FROM package")
  
  # Save the uploaded packages that were not in the db.
  new_pkgs <- uploaded_packages %>% filter(!(package %in% curr_pkgs$name))

  withProgress(message = "Uploading Packages to DB:", detail = "go get some coffee!", value = 0, {
  if(nrow(new_pkgs) != 0){
    for (i in seq_along(new_pkgs$package)) {
      pkg <- new_pkgs$package[i]
      incProgress(1 / (nrow(new_pkgs) + 1), detail = paste("package", pkg, "version", new_pkgs$version[i]))
      # Get and upload pkg general info to db.
      insert_pkg_info_to_db(pkg)
      # Get and upload maintenance metrics to db.
      insert_maintenance_metrics_to_db(pkg)
      # Get and upload community metrics to db.
      insert_community_metrics_to_db(pkg)
    }
  }
    setProgress(value = 1, detail = "finished uploading packages")
    Sys.sleep(0.25)
  })
  
  all_pkgs <- dbSelect("SELECT name FROM package")
  
  # Data frame indicating which packages where duplicate, new, and not found.
  uploaded_packages <- uploaded_packages %>%
    mutate(status = case_when(
      !(package %in% all_pkgs$name) ~ 'not found',
      package %in% curr_pkgs$name ~ 'duplicate',
      TRUE ~ 'new')
    )
  
  loggit("INFO",
         paste("Uploaded file:", input$uploaded_file$name, 
               "Total Packages:", nrow(uploaded_packages$package),
               "New Packages:", sum(uploaded_packages$status == 'new'),
               "Undiscovered Packages:", sum(uploaded_packages$status == 'not found'),
               "Duplicate Packages:", sum(uploaded_packages$status == 'duplicate')),
         echo = FALSE)
  
  values$uploaded_pkgs <- uploaded_packages
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
  req(nrow(values$uploaded_pkgs) > 0)
  as.character(tagList(
    br(), br(),
    hr(),
    h5("Summary of uploaded package(s)"),
    br(),
    p(tags$b("Total Packages: "), nrow(values$uploaded_pkgs)),
    p(tags$b("New Packages: "), sum(values$uploaded_pkgs$status == 'new')),
    p(tags$b("Undiscovered Packages: "), sum(values$uploaded_pkgs$status == 'not found')),
    p(tags$b("Duplicate Packages: "), sum(values$uploaded_pkgs$status == 'duplicate')),
    p("Note: The assessment will be performed on the latest version of each
        package, irrespective of the uploaded version.")
  ))
})

# Uploaded packages table.
output$upload_pkgs_table <- DT::renderDataTable({
  req(nrow(values$uploaded_pkgs) > 0)
  
  datatable(
    values$uploaded_pkgs,
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
