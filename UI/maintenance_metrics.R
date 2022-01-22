# Render Output UI for Maintenance Metrics.
output$maintenance_metrics <- renderUI({
  req(selected_pkg$name())
  
  if(selected_pkg$name() == "-")
    showSelectPackageMessage()
  
  else {
    shiny::tagList(
      br(),
      div(addHelpButton("help_mm")),
      br(), br(),
      fluidRow(
        div(style = "height:25px;"),
        class = "mm-main-row",
        div(id = "mm_infoboxes", 
            fluidRow(
              class = "mm-row-1",
              infoBoxOutput("has_vignettes"),  # Info box for 'has_vignettes' metric.
              infoBoxOutput("has_website"),  # Info box for 'has_website' metric.
              infoBoxOutput("has_news"),  # Info box for 'has_news' metric.
            ),
            fluidRow(
              class = "mm-row-2",
              infoBoxOutput("news_current"),  # Info box for 'news_current' metric.
              infoBoxOutput("has_bug_reports_url"),  # Info box for 'has_bug_reports_url' metric.
              infoBoxOutput("bugs_status"),  # Info box for 'bugs_status' metric.
            ),
            fluidRow(
              class = "mm-row-3",
              infoBoxOutput("export_help"),  # Info box for 'export_help' metric.
              infoBoxOutput("has_source_control"),  # Info box for 'has_source_control' metric.
              infoBoxOutput("has_maintainer"),  # Info box for 'has_maintainer' metric.
            )
        ),
        fluidRow(
          column(
            width = 8,
            textAreaInput(
              "add_comment",
              h5("Add Comment for Maintenance Metrics"),
              width = "100%",
              rows = 4,
              placeholder = paste("Commenting as", values$name, "(", values$role, ")")
            ),
            actionButton("submit_comment", "Submit")
          )
        ),
        fluidRow(
          style = "margin-top: 50px",
          column(
            width = 12,
            align = "left",
            h5('Current Comments', style = "padding-bottom:10px;"),
            wellPanel(htmlOutput("view_comments"))
          )
        )
      )
    )
  }
})
