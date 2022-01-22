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
          id = "mm_add_comment",
          class = "mm-row-comments-box",
          column(
            width = 8,
            class = "mb-4 label-float-left",
            # Text input box to leave the Maintenance Metrics Comments.
            textAreaInput(
              "mm_comment",
              h3(tags$b("Leave Your Comment for Maintenance Metrics:")),
              width = "100%",
              rows = 4,
              placeholder = paste("Commenting as", values$name, "(", values$role, ")")
            ) %>%
              shiny::tagAppendAttributes(style = 'width: 100%;'),
            # Action button to submit the comment.
            actionButton("submit_mm_comment", class = "submit_mm_comment_class btn-secondary", "Submit")
          )
        ),
        fluidRow(
          id = "mm_prev_comments",
          class = "mm-row-comments",
          column(
            width = 12,
            align = "left",
            h3(tags$b(paste0('Comments(',nrow(values$comment_mm2),'):'))),
            htmlOutput("mm_commented")  # html output to show the comments on application.
          )
        )
      )
    )
  }
})
