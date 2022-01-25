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
        br(), br(), br(),
        metricGridUI("mm_metricGrid"),
        br(), br(),
        fluidRow(
          addCommentUI("add_comment_for_mm"),
          viewCommentsUI("view_mm_comments")
        )
      )
    }
  )
})
