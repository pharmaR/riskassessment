#' 'Upload Package' UI
#' 
#' @param id a module id
#' 
#' @import shiny
#' @importFrom DT dataTableOutput
#' 
uploadPackageUI <- function(id) {
  fluidPage(
    br(), br(),
    
    introJSUI(NS(id, "introJS")),
    
    tags$head(tags$style(".shiny-notification {font-size:30px; color:darkblue; position: fixed; width:415px; height: 150px; top: 75% ;right: 10%;")),
    
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
    fluidRow(column(width = 12, DT::dataTableOutput(NS(id, "upload_pkgs_table"))))
  )
}


#' Server logic for the 'Upload Package' module
#'
#' @param id a module id
#' 
#' @importFrom riskmetric pkg_ref
#' @importFrom rintrojs introjs
#' @importFrom readr read_csv
#' 
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
      input[["introJS-help"]], # notice input contains "id-help"
      rintrojs::introjs(session,
                        options = list(
                          steps = 
                            upload_pkg_txt() %>%
                            union(sidebar_steps),
                          "nextLabel" = "Next",
                          "prevLabel" = "Previous"
                        )
      )
    )
    
    # Save all the uploaded packages, marking them as 'new', 'not found', or
    # 'duplicate'.
    uploaded_pkgs <- reactive({
      
      # Return an empty data.frame when no file is uploaded.
      # This is to allow for the database view method to update when a package
      # is uploaded without failing.
      if(is.null(input$uploaded_file))
        return(data.frame())
      
      req(input$uploaded_file)
      
      if(is.null(input$uploaded_file$datapath))
        validate('Please upload a nonempty CSV file.')
      
      uploaded_packages <- readr::read_csv(input$uploaded_file$datapath, show_col_types = FALSE)
      np <- nrow(uploaded_packages)
      if(np == 0)
        validate('Please upload a nonempty CSV file.')
      
      if(!all(colnames(uploaded_packages) == colnames(template)))
        validate("Please upload a CSV with a valid format.")
      
      # Add status column and remove white space around package names.
      uploaded_packages <- uploaded_packages %>%
        dplyr::mutate(
          status = rep('', np),
          package = trimws(package),
          version = trimws(version)
        )
      
      # read website to create vector of available packages by name
      website <- "https://cran.rstudio.com/web/packages/available_packages_by_name.html"
      con <- url(website, open = "rb")
      namelst <- rvest::read_html(con)
      close(con)
      
      pkgnames <- namelst %>% 
        html_nodes("a") %>%
        html_text() 
      
      # Drop A-Z
      CRAN_arch <- pkgnames[27:length(pkgnames)]
      rm(namelst, pkgnames)

      # Start progress bar. Need to establish a maximum increment
      # value based on the number of packages, np, and the number of
      # incProgress() function calls in the loop, plus one to show
      # the incProgress() that the process is completed.
      withProgress(
        max = (np * 5) + 1, value = 0,
        message = "Uploading Packages to DB:", {
          
          for (i in 1:np) {
            
            user_ver <- uploaded_packages$version[i]
            incProgress(1, detail = glue::glue("{uploaded_packages$package[i]} {user_ver}"))
            
            # run pkg_ref() to get pkg version and source info
            ref <- riskmetric::pkg_ref(uploaded_packages$package[i])
            
            if (ref$source == "pkg_missing"){
              incProgress(1, detail = 'Package {uploaded_packages$package[i]} not found')
              
              uploaded_packages$status[i] <- 'not found'
              
              # Suggest alternative spellings using utils::adist() function
              v <- utils::adist(uploaded_packages$package[i], CRAN_arch, ignore.case = FALSE)
              rlang::inform(paste("Package name",uploaded_packages$package[i],"was not found."))
              rlang::inform(paste("suggested package names:",paste(head(CRAN_arch[which(v == min(v))], 10),collapse = ", ")))
              
              loggit::loggit('WARN',
                             glue::glue('Package {ref$name} was flagged by riskmetric as {ref$source}.'))
              
              next
            }
            
            ref_ver <- as.character(ref$version)
            
            if(user_ver == ref_ver) ver_msg <- ref_ver
            else ver_msg <- glue::glue("{ref_ver}, not '{user_ver}'")
            
            as.character(ref$version)
            deets <- glue::glue("{uploaded_packages$package[i]} {ver_msg}")
            
            # Save version.
            incProgress(1, detail = deets)
            uploaded_packages$version[i] <- as.character(ref$version)
            
            found <- nrow(dbSelect(glue::glue(
              "SELECT name
              FROM package
              WHERE name = '{uploaded_packages$package[i]}'")))
            
            uploaded_packages$status[i] <- ifelse(found == 0, 'new', 'duplicate')
            
            # Add package and metrics to the db if package is not in the db.
            if(!found) {
              # Get and upload pkg general info to db.
              incProgress(1, detail = deets)
              insert_pkg_info_to_db(uploaded_packages$package[i])
              # Get and upload maintenance metrics to db.
              incProgress(1, detail = deets)
              insert_maintenance_metrics_to_db(uploaded_packages$package[i])
              # Get and upload community metrics to db.
              incProgress(1, detail = deets)
              insert_community_metrics_to_db(uploaded_packages$package[i])
            }
          }
          
          incProgress(1, detail = "   **Completed Pkg Uploads**")
          Sys.sleep(0.25)
          
        }) #withProgress
      
      uploaded_packages
    })
    
    # Download the sample dataset.
    output$download_sample <- downloadHandler(
      filename = function() {
        paste("template", ".csv", sep = "")
      },
      content = function(file) {
        write.csv(template, file, row.names = F)
      }
    )
    
    # Uploaded packages summary.
    output$upload_summary_text <- renderText({
      req(uploaded_pkgs)
      req(nrow(uploaded_pkgs()) > 0)
      
      loggit::loggit("INFO",
                     paste("Uploaded file:", input$uploaded_file$name, 
                           "Total Packages:", nrow(uploaded_pkgs()),
                           "New Packages:", sum(uploaded_pkgs()$status == 'new'),
                           "Undiscovered Packages:", sum(uploaded_pkgs()$status == 'not found'),
                           "Duplicate Packages:", sum(uploaded_pkgs()$status == 'duplicate')),
                     echo = FALSE)
      
      as.character(tagList(
        br(), br(),
        hr(),
        div(id = "upload_summary_div",
            h5("Summary of uploaded package(s)"),
            br(),
            p(tags$b("Total Packages: "), nrow(uploaded_pkgs())),
            p(tags$b("New Packages: "), sum(uploaded_pkgs()$status == 'new')),
            p(tags$b("Undiscovered Packages: "), sum(uploaded_pkgs()$status == 'not found')),
            p(tags$b("Duplicate Packages: "), sum(uploaded_pkgs()$status == 'duplicate')),
            p("Note: The assessment will be performed on the latest version of each
          package, irrespective of the uploaded version.")
        )
      ))
    })
    
    # Uploaded packages table.
    output$upload_pkgs_table <- DT::renderDataTable({
      req(nrow(uploaded_pkgs()) > 0)
      
      DT::datatable(
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
      DT::dataTableOutput(NS(id, "sampletable"))
      
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
              DT::datatable(
                template,
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
