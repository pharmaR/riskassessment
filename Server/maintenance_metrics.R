# Start introjs when help button is pressed.
observeEvent(input$help_mm,
             introjs(session,
                     options = list(
                       steps = 
                         mm_steps %>%
                         union(sidebar_steps),
                       "nextLabel" = "Next",
                       "prevLabel" = "Previous",
                       "skipLabel" = "Close"
                     )
             )
)

# Save each metric information into variables.
observe({
  req(selected_pkg$name())
  
  if(selected_pkg$name() != "-"){
    
    # Collect all the metric names and values associated to package_id.
    values$riskmetrics_mm <- db_fun(glue(
      "SELECT metric.name, package_metrics.value
        FROM metric
        INNER JOIN package_metrics ON metric.id = package_metrics.metric_id
        WHERE package_metrics.package_id = '{selected_pkg$id()}' AND 
        metric.class = 'maintenance' ;"))
    
    for(i in 1:nrow(values$riskmetrics_mm))
      values[[values$riskmetrics_mm$name[i]]] <- values$riskmetrics_mm$value[i]
  }
})

# Render infobox for has_vignettes metric.
output$has_vignettes <- renderInfoBox({
  has_vignettes_infobox(values)
})

# Render infobox for has_website metric.
output$has_website <- renderInfoBox({
  has_website_infobox(values)
})

# Render infobox for has_news metric.
output$has_news <- renderInfoBox({
  has_news_infobox(values)
})

# Render infobox for news_current metric.
output$news_current <- renderInfoBox({
  news_current_infobox(values)
})

# Render infobox for has_bug_reports_url metric.
output$has_bug_reports_url <- renderInfoBox({
  has_bug_reports_url_infobox(values)
})

# Render infobox for bugs_status metric.
output$bugs_status <- renderInfoBox({
  bugs_status_infobox(values)
})

# Render infobox for export_help metric.
output$export_help <- renderInfoBox({
  export_help_infobox(values)
})

# Render infobox for has_source_control metric.
output$has_source_control <- renderInfoBox({
  has_source_control_infobox(values)
})

# Render infobox for has_maintainer metric.
output$has_maintainer <- renderInfoBox({
  has_maintainer_infobox(values)
})

# Show the comments on the package.
output$view_comments <- renderText({
  showComments(pkg_name = selected_pkg$name(), comment_type = 'mm')
})

# Observe event for submit button.
observeEvent(input$submit_comment, {
  if (trimws(input$add_comment) != "") {
    db_ins(glue(
      "INSERT INTO comment values('{selected_pkg$name()}', '{values$name}', 
      '{values$role}', '{input$add_comment}', 'mm', '{TimeStamp()}')"
    )
    )
    values$mm_comment_submitted <- "yes"
    updateTextAreaInput(session, "add_comment", value = "")
    # After comment added to Comments table, update db dash
    values$db_pkg_overview <- update_db_dash()
  }
})
