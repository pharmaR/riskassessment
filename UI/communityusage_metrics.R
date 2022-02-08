# Render Output UI for Community Usage Metrics.
output$community_usage_metrics <- renderUI({
  
  # Lets the user know that a package needs to be selected.
  if(identical(selected_pkg$name(), character(0)))
    showHelperMessage()
  
  else {
    fluidPage(
      
      tagList(
        br(),
        introJSUI("cum_introJS"),
        h4("Community Usage Metrics", style = "text-align: center;"),
        br(), br(),
        # TODO: change this for a grid.
        fluidRow(
          column(width = 4, metricBoxUI('time_since_first_version')),
          column(width = 4, metricBoxUI("time_since_latest_version")),
          column(width = 4, metricBoxUI('downloads_last_year'))
        ),
        br(), br(),
        fluidRow(
          column(width = 12, style = 'padding-left: 20px; padding-right: 20px;',
                 plotlyOutput("downloads_plot"))),
        br(), br(),
        fluidRow(
          addCommentUI(id = "add_comment_for_cum"),
          viewCommentsUI(id = "view_cum_comments"))
      )
    )
  }
})
