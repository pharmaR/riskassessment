# Render Output UI for Maintenance Metrics.
output$maintenance_metrics <- renderUI({

  # Lets the user know that a package needs to be selected.
  if(identical(selected_pkg$name(), character(0)))
    showHelperMessage()
  
  else {
    fluidPage(
      
      tagList(
        br(),
        introJSUI("mm_introJS"),
        h4("Maintenance Metrics", style = "text-align: center;"),
        br(), br(),
        metricGridUI("mm_metricGrid"),
        br(), br(),
        fluidRow(div(id = "comments_for_mm",
            addCommentUI("add_comment_for_mm"),
            viewCommentsUI("view_mm_comments")
          )
        )
      )
    )
  }
})
