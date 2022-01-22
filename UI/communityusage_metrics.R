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
        div(style = "height:25px;"),
        class = "c_u_m_row_main",
        fluidRow(
          id = "cum_infoboxes",
          class = "c_u_m_row_1",
          infoBoxOutput("time_since_first_release", width = 4),  # Info box to show the time since First release.
          infoBoxOutput("time_since_version_release", width = 4),  # Info box to show the time since version release.
          infoBoxOutput("dwnlds_last_yr", width = 4)  # Info box to show the total # of Downloads in the last year.
        ),
        fluidRow(
          id = "cum_plot",
          class = "c_u_m_row_graph",
          column(width = 1, ),
          column(width = 10,
                 class = "w-90",
                 plotly::plotlyOutput("no_of_downloads")
          ),
          column(width = 1, )
        ),
        
        addCommentUI(id = "add_comment_for_cum"),
        viewCommentsUI(id = "view_cum_comments")
      )
    )
  } 
})
