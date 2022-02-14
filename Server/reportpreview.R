# IntroJS.
introJSServer(id = "report_introJS", text = rp_steps)

# View comments.
viewCommentsServer(id = "view_cum_comments_for_report",
                   comment_added = cum_comment_added,
                   pkg_name = selected_pkg$name,
                   comment_type = 'cum',
                   label = 'Community Usage Metrics Comments')

# View comments.
viewCommentsServer(id = "view_mm_comments_for_report",
                   comment_added = mm_comment_added,
                   pkg_name = selected_pkg$name,
                   comment_type = 'mm',
                   label = 'Maintainance Metrics Comments')

# View comments.
viewCommentsServer(id = "view_overall_comments_for_report",
                   comment_added = selected_pkg$overall_comment_added,
                   pkg_name = selected_pkg$name,
                   comment_type = 'o',
                   label = 'Overall Comments')

metricGridServer("report_mm_metricGrid", metrics = maint_metrics)

# Display general information of the selected package.
output$pkg_overview <- renderUI({
  req(selected_pkg$name())
  
  tagList(
    h5('Package:'), selected_pkg$name(),
    h5('Version:'), selected_pkg$version(),
    h5('Title:'), selected_pkg$title(),
    h5('Description:'), selected_pkg$description(),
    h5('Author:'), selected_pkg$author(),
    h5('Maintainer:'), selected_pkg$maintainer(),
    h5('License:'), selected_pkg$license(),
    h5('Published:'), selected_pkg$published()
  )
})

# Display the decision status of the selected package.
output$decision_display <- renderUI({
  tagList(
    h5('Overall risk:'),
    ifelse(selected_pkg$decision() == '', 
           'Pending',
           selected_pkg$decision()))
})

# Create report.
output$download_report_btn <- downloadHandler(
  filename = function() {
    paste0(selected_pkg$name(), "_", selected_pkg$version(), "_Risk_Assessment.",
           switch(input$report_format, "docx" = "docx", "html" = "html"))
  },
  content = function(file) {
    shiny::withProgress(
      message = paste0("Downloading ", input$dataset, " Report"),
      value = 0,
      {
        shiny::incProgress(1 / 10)
        shiny::incProgress(5 / 10)
        if (input$report_format == "html") {
          Report <- file.path(tempdir(), "Report_html.Rmd")
          file.copy("Reports/Report_html.Rmd", Report, overwrite = TRUE)
        } else {
          Report <- file.path(tempdir(), "Report_doc.Rmd")
          file.copy("Reports/Report_doc.Rmd", Report, overwrite = TRUE)
        }
        
        rmarkdown::render(
          Report,
          output_file = file,
          params = list(package = selected_pkg$name(),
                        riskmetric_version = packageVersion("riskmetric"),
                        cwd = getwd(),
                        username = values$name,
                        user_role = values$role)
        )
      })
  }
)