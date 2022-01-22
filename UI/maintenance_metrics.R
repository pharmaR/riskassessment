# Render Output UI for Maintenance Metrics.
output$maintenance_metrics <- renderUI({
  req(selected_pkg$name())
  
  fluidPage(
    
    if(selected_pkg$name() == "-")
      showSelectPackageMessage()
    
    else {
      shiny::tagList(
        br(),
        introJSUI("mm_introJS"),
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
          addCommentUI("add_comment_for_mm"),
          viewCommentsUI("view_mm_comments")
        )
      )
      
    }
  )
})
