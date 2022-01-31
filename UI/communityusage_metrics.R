# Render Output UI for Community Usage Metrics.
output$community_usage_metrics <- renderUI({
  req(selected_pkg$name())
  
  if(selected_pkg$name() == "-")
    showSelectPackageMessage()
  
  else {
    shiny::tagList(
      br(),
      introJSUI("cum_introJS"),
      br(), br(),
      fluidRow(
        fluidRow(
          column(width = 4, metricBoxUI('time_since_first_version')),
          column(width = 4, metricBoxUI("time_since_latest_version")),
          column(width = 4, metricBoxUI('downloads_last_year'))
        ),
        fluidRow(
          # column(width = 10,
          #        class = "w-90",
          #        plotly::plotlyOutput("no_of_downloads")
          # )
        ),
        
        addCommentUI(id = "add_comment_for_cum"),
        viewCommentsUI(id = "view_cum_comments")
      )
    )
  } 
})
