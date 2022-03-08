# Maintenance Metrics module.
maintenanceMetricsUI <- function(id) {
  uiOutput(NS(id, 'maintenance_metrics_ui'))
}

maintenanceMetricsServer <- function(id, selected_pkg, maint_metrics, user) {
  moduleServer(id, function(input, output, session) {
    # Render Output UI for Maintenance Metrics.
    output$maintenance_metrics_ui <- renderUI({
      
      browser()
      
      # Lets the user know that a package needs to be selected.
      if(identical(selected_pkg$name(), character(0)))
        showHelperMessage()
      
      else {
        fluidPage(
          
          tagList(
            br(),
            introJSUI(NS(id, 'introJS')),
            h4("Maintenance Metrics", style = "text-align: center;"),
            br(), br(),
            metricGridUI('metricGrid'),
            br(), br(),
            fluidRow(div(id = "comments_for_mm",
                         addCommentUI(NS(id, 'add_comment')),
                         viewCommentsUI(NS(id, 'view_comments')))
            )
          )
        )
      }
    })
    
    # IntroJS.
    introJSServer(id = "introJS", text = mm_steps)
    
    # Call module that creates section to add comments.
    comment_added <- addCommentServer(id = "add_comment",
                                         metric_abrv = 'mm',
                                         user_name = reactive(user$name),
                                         user_role = reactive(user$role),
                                         pkg_name = selected_pkg$name)
    
    # Call module that creates comments view.
    viewCommentsServer(id = "view_comments",
                       comment_added = comment_added,
                       pkg_name = selected_pkg$name,
                       comment_type = 'mm')
    
    metricGridServer(id = "metricGrid",
                     metrics = maint_metrics)
    
    list(
      mm_comment_added = comment_added
    )
  })
}