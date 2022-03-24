# Risk color palettes.
# https://www.rapidtables.com/web/color/html-color-codes.html
low_risk_color  <- "#228B22"  # forest green
med_risk_color  <- "#d1b000"  # dark gold
high_risk_color <- "#B22222"  # firebrick
setColorPalette <- colorRampPalette(c(low_risk_color, med_risk_color, high_risk_color))

databaseViewUI <- function(id) {
  fluidPage(
    fluidRow(
      column(
        width = 8, offset = 2, align = "center",
        br(),
        h4("Database Overview"),
        hr(),
        tags$section(
          br(), br(),
          box(width = 12,
              title = h5("Uploaded Packages", style = "margin-top: 5px"),
              DT::dataTableOutput(NS(id, "packages_table")),
              br(),
              fluidRow(
                column(
                  width = 6,
                  style = "margin: auto;",
                  downloadButton(NS(id, "download_reports"), "Download Report(s)")),
                column(
                  width = 6,
                  selectInput(NS(id, "report_formats"), "Select Format", c("html", "docx"))
                )
              )))
      ))
  )
}

databaseViewServer <- function(id, user, uploaded_pkgs) {
  moduleServer(id, function(input, output, session) {
    
    # Update table_data if a package has been uploaded
    table_data <- eventReactive(uploaded_pkgs(), {
      
      db_pkg_overview <- dbSelect(
        'SELECT pi.name, pi.version, pi.score, pi.decision, c.last_comment
        FROM package as pi
        LEFT JOIN (
            SELECT id, max(added_on) as last_comment FROM comments GROUP BY id)
        AS c ON c.id = pi.name
        ORDER BY 1 DESC'
      )
      
      db_pkg_overview %>%
        mutate(last_comment = as.character(as_datetime(last_comment))) %>%
        mutate(last_comment = ifelse(is.na(last_comment), "-", last_comment)) %>%
        mutate(decision = ifelse(decision != "", paste(decision, "Risk"), "-")) %>%
        mutate(was_decision_made = ifelse(decision != "-", TRUE, FALSE)) %>%
        select(name, version, score, was_decision_made, decision, last_comment)
    })
    
    # Create table for the db dashboard.
    output$packages_table <- DT::renderDataTable({
      
      as.datatable(
        formattable(
          table_data(),
          list(
            score = formatter(
              "span",
              style = x ~ style(display = "block",
                                "border-radius" = "4px",
                                "padding-right" = "4px",
                                "font-weight" = "bold",
                                "color" = "white",
                                "order" = x,
                                "background-color" = csscolor(
                                  setColorPalette(100)[round(as.numeric(x)*100)]))),
            decision = formatter(
              "span",
              style = x ~ style(display = "block",
                                "border-radius" = "4px",
                                "padding-right" = "4px",
                                "font-weight" = "bold",
                                "color" = "white",
                                "background-color" = 
                                  ifelse(x == "High Risk", high_risk_color,
                                         ifelse(x == "Medium Risk", med_risk_color,
                                                ifelse(x == "Low Risk", low_risk_color, "transparent"))))),
            was_decision_made = formatter("span",
                                          style = x ~ style(color = ifelse(x, "#0668A3", "gray")),
                                          x ~ icontext(ifelse(x, "ok", "remove"), ifelse(x, "Yes", "No")))
          )),
        selection = list(mode = 'multiple'),
        colnames = c("Package", "Version", "Score", "Decision Made?", "Decision", "Last Comment"),
        rownames = FALSE,
        options = list(
          searching = FALSE,
          lengthChange = FALSE,
          #dom = 'Blftpr',
          pageLength = 15,
          lengthMenu = list(c(15, 60, 120, -1), c('15', '60', '120', "All")),
          columnDefs = list(list(className = 'dt-center'))
        )
      ) %>%
        formatStyle(names(table_data()), textAlign = 'center')
    })
    
    # Enable the download button when a package is selected.
    observe({
      if(!is.null(input$packages_table_rows_selected)) {
        shinyjs::enable("download_reports")
      } else {
        shinyjs::disable("download_reports")
      }
    })
    
    output$download_reports <- downloadHandler(
      filename = function() {
        report_datetime <- str_replace_all(str_replace(Sys.time(), " ", "_"), ":", "-")
        glue('RiskAssessment-Report-{report_datetime}.zip')
      },
      content = function(file) {

        selected_pkgs <- table_data() %>%
          slice(input$packages_table_rows_selected)
        
        n_pkgs <- nrow(selected_pkgs)
        
        req(n_pkgs > 0)
        
        shiny::withProgress(
          message = glue('Downloading {n_pkgs} Report{ifelse(n_pkgs > 1, "s", "")}'),
          value = 0,
          max = n_pkgs + 2, # Tell the progress bar the total number of events.
          {
            shiny::incProgress(1)
            
            my_dir <- tempdir()
            if (input$report_formats == "html") {
              Report <- file.path(my_dir, "Report_html.Rmd")
              file.copy("Reports/Report_html.Rmd", Report, overwrite = TRUE)
            } else {
              Report <- file.path(my_dir, "Report_doc.Rmd")
              file.copy("Reports/Report_doc.Rmd", Report, overwrite = TRUE)
            }
            
            fs <- c()
            for (i in 1:n_pkgs) {
              # Grab package name and version, then create filename and path.
              this_pkg <- selected_pkgs$name[i]
              this_ver <- selected_pkgs$version[i]
              file_named <- glue('{this_pkg}_{this_ver}_Risk_Assessment.{input$report_formats}')
              
              path <- file.path(my_dir, file_named)
              # Render the report, passing parameters to the rmd file.
              rmarkdown::render(
                input = Report,
                output_file = path,
                params = list(package = this_pkg,
                              riskmetric_version = packageVersion("riskmetric"),
                              cwd = getwd(),
                              username = user$name,
                              user_role = user$role)
              )
              fs <- c(fs, path)  # Save all the reports/
              shiny::incProgress(1) # Increment progress bar.
            }
            # Zip all the files up. -j retains just the files in zip file.
            zip(zipfile = file, files = fs, extras = "-j")
            shiny::incProgress(1) # Increment progress bar.
          })
      },
      contentType = "application/zip"
    )
  })
}