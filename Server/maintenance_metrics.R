# IntroJS.
introJSServer(id = "mm_introJS", text = mm_steps)


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

mm_comment_added <- addCommentServer(id = "add_comment_for_mm",
                                     metric_abrv = 'mm',
                                     user_name = reactive(user$name),
                                     user_role = reactive(user$role),
                                     pkg_name = selected_pkg$name)

viewCommentsServer(id = "view_mm_comments",
                   comment_added = mm_comment_added,
                   pkg_name = selected_pkg$name,
                   comment_type = 'mm')



