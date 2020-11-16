###############################################################################
# db_dash_screen - Server functions for the database dashboard.
# Author: Aaron Clark
# Date: September 22nd, 2020
# License: MIT License
###############################################################################

# Create table for the db dashboard.
output$db_pkgs <- DT::renderDataTable({
  values$db_pkg_overview <- db_fun(paste0("
      SELECT case when package = '", input$select_pack,
      "' then 1 else 0 end as Selected
      , pi.package
      , pi.version
      , pi.score
      , pi.decision
      , c.last_comment
      FROM Packageinfo as pi
      LEFT JOIN (
        SELECT comm_id
             , max(added_on) as last_comment
        FROM Comments
        GROUP BY comm_id
      ) as c
      on c.comm_id = pi.package
      ORDER BY 1 DESC
    "
  ))
  
  DT::datatable(
    values$db_pkg_overview,
    selection = list(mode = 'multiple'),
    extensions = "Buttons",
    colnames = c("Selected", "Package", "Version",
                 "Score", "Decision", "Last Comment"),
    options = list(
      dom = 'Blftpr',
      pageLength = 10,
      lengthMenu = list(c(10, 50, 100, -1), c('15', '50', '100', "All")),
      columnDefs = list(
        list(targets = 1, visible = FALSE),
        list(className = 'dt-center')
      ),
      buttons = list(list(
        extend = "excel", 
        filename = paste(
          sep = "_", 
          "RiskAsses_PkgDB_Dwnld",
          str_replace_all(str_replace(Sys.time(), " ", "_"), ":", "-"))
      ))
    )
  ) %>%
  formatStyle(names(values$db_pkg_overview), textAlign = 'center')
})


# Enable the download button when a package is selected.
observe({
  if(!is.null(input$db_pkgs_rows_selected))
    shinyjs::enable("dwnld_sel_db_pkgs_btn")
  else
    shinyjs::disable("dwnld_sel_db_pkgs_btn")
})

values$cwd <- getwd()

# Download handler to create a report for each package selected.
output$dwnld_sel_db_pkgs_btn <- downloadHandler(
  filename = function() {
    paste("RiskAsses_PkgDB_Dwnld",
          str_replace_all(
            str_replace(Sys.time(), " ", "_"), ":", "-"),".zip", sep = "_")
  },
  content = function(file) {
    these_pkgs <- values$db_pkg_overview %>% slice(input$db_pkgs_rows_selected)
    n_pkgs <- nrow(these_pkgs)
    req(n_pkgs > 0)
    shiny::withProgress(
      message = paste0("Downloading ", n_pkgs, " Report", ifelse(n_pkgs > 1, "s", "")),
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
          this_pkg <- these_pkgs$package[i]
          this_ver <- these_pkgs$version[i]
          file_named <- paste0(this_pkg,"_",this_ver,"_Risk_Assessment.",input$report_formats)
          path <- file.path(my_dir, file_named)
          # Render the report, passing parameters to the rmd file.
          rmarkdown::render(
            input = Report,
            output_file = path,
            params = list(package = this_pkg,
                          version = this_ver,
                          cwd = values$cwd)
          )
          fs <- c(fs, path)  # Save all the reports/
          shiny::incProgress(1) # Increment progress bar.
        }
        # Zip all the files up. -j retains just the files in zip file.
        zip(zipfile = file, files = fs, extras = "-j")
        shiny::incProgress(1) # Icrement progress bar.
      })
  },
  contentType = "application/zip"
)


# Bring back user to the main dashboard.
observeEvent(input$back2dash, {
  values$current_screen <- "dashboard_screen"
})
