# Maintenance Metrics module.
maintenanceMetricsUI <- function(id) {
  uiOutput(NS(id, 'maintenance_metrics_ui'))
}

maintenanceMetricsServer <- function(id, selected_pkg, maint_metrics, user) {
  moduleServer(id, function(input, output, session) {
    # Render Output UI for Maintenance Metrics.
    output$maintenance_metrics_ui <- renderUI({
      
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
            metricGridUI(NS(id, 'metricGrid')),
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
    
    comments <- eventReactive(comment_added(), {
      dbSelect(
        glue(
        "SELECT user_name, user_role, comment, added_on
        FROM comments
        WHERE id = '{selected_pkg$name()}' AND comment_type = 'mm'"
        )
      ) %>%
        map(rev)
    })
    
    # Call module that creates comments view.
    viewCommentsServer(id = "view_comments",
                       comments = comments,
                       pkg_name = selected_pkg$name)
    
    metricGridServer(id = "metricGrid",
                     metrics = maint_metrics)
    
    list(
      comments = comments,
      comment_added = comment_added
    )
  })
}