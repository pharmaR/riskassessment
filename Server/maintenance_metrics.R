#####################################################################################################################
# login_screen.R - Maintenance_Metrics Source file for Server Module.
# 
# Author: Aravind
# Date: June 13th, 2020
#####################################################################################################################


# Save each metric information into variables.
observe({
  req(input$select_pack)
  if(input$tabs == "mm_tab_value"){
    if(input$select_pack != "Select"){
      
      package_id <- db_fun(paste0("SELECT id
                                  FROM package
                                  WHERE name = ", "'", input$select_pack, "';"))
      
      # Leave method if package not found.
      # TODO: save this to the json file.
      if(nrow(package_id) == 0){
        print("PACKAGE NOT FOUND.")
        return()
      }
      
      # Collect all the metric names and values associated to package_id.
      values$riskmetrics_mm <- db_fun(paste0(
        "SELECT metric.name, package_metrics.value
        FROM metric
        INNER JOIN package_metrics ON metric.id = package_metrics.metric_id
        WHERE package_metrics.package_id = ", "'", package_id, "'", " AND ",
        "metric.class = 'maintenance' ;"))

      runjs("setTimeout(function(){ capturingSizeOfInfoBoxes(); }, 500);")
      
      for(i in 1:nrow(values$riskmetrics_mm))
        values[[values$riskmetrics_mm$name[i]]] <- values$riskmetrics_mm$value[i]
      
      if (values$selected_pkg$decision != "") {
        runjs("setTimeout(function(){disableUI('mm_comment')}, 500);")
        runjs("setTimeout(function(){disableUI('submit_mm_comment')}, 500);")
      }
    }
  }
})

# Render infobox for has_vignettes metric.
output$has_vignettes <- renderInfoBox({
  req(values$has_vignettes)
  
  has_metric <- !(values$has_vignettes %in% c("NA", "pkg_metric_error"))
  
  # Total number of vignettes.
  value <- as.numeric(values$has_vignettes)
  
  infoBox(
    width = 3,
    fill = TRUE,
    title = "Presence of vignettes?",
    icon = icon(
      ifelse(has_metric && value >= 1, "thumbs-up", "thumbs-down"),
      lib = "glyphicon"
    ),
    color = ifelse(has_metric && value >= 1, "green", "red"),
    
    # Output
    #   YES (if metric has value),
    #   NO (if metric doesnt have any value),
    #   or NA (if metric equals NA or pkg_metric_error).
    if(!has_metric){"NA"}
    else if(value == 0){"NO"}
    else{"YES"},
    
    # Output metric value if it exists or
    #   a generic message if NA or error occurred.
    if(!has_metric) {"Metric is not applicable for this source of package"}
    else{
      paste(
        "The package has", value, if(value == 1) "vignette" else "vignettes")},
  )
})

output$website <- renderInfoBox({
  req(values$has_website)
  infoBox(
    title = "Associated website URL?",
    if(values$has_website[1] == 1){"YES"}
    else if(values$has_website[2] == -1){"NA"}
    else{"NO"},
    width = 3,
    if(values$has_website[2] == -1){"Metric is not applicable for this source of package"}
    else{ ifelse(values$has_website[1] == 1, paste("Website:",values$has_website[2]), "The package does not have an associated website URL")},
    icon = icon(
      ifelse(values$has_website[1] == 1, "thumbs-up", "thumbs-down"),
      lib = "glyphicon"
    ),
    color = ifelse(values$has_website[1] == 1, "green", "red"),
    fill = TRUE
  )
})  # End of the render Output.
# 3. Render Output Info box to show the Package Has News? Content.

output$hasnews <- renderInfoBox({
  req(values$has_news)
  infoBox(
    title = "NEWS?",
    if(values$has_news[1] == 1){"YES"}
    else if(values$has_news[2] == -1){"NA"}
    else{"NO"},
    width = 3,
    if(values$has_news[2] == -1){"Metric is not applicable for this source of package"}
    else{ ifelse(values$has_news[1] == 1, "The package has a NEWS file.", "The package does not have a NEWS file")},
    icon = icon(
      ifelse(values$has_news[1] == 1, "thumbs-up", "thumbs-down"),
      lib = "glyphicon"
    ),
    color = ifelse(values$has_news[1] == 1, "green", "red"),
    fill = TRUE
  )
})  # End of the render Output.
# 4. Render Output Info box to show the information for News is Current?

output$newscurrent <- renderInfoBox({
  req(values$news_current)
  infoBox(
    title = "News is current?",
    if(values$news_current[1] == 1){"YES"}
    else if(values$news_current[2] == -1){"NA"}
    else{"NO"},
    width = 3,
    if(values$news_current[2] == -1){"Metric is not applicable for this source of package"}
    else{ ifelse(values$news_current[1] == 1, "NEWS file contains entry for current version number", "NEWS file does not contains entry for current version number")},
    icon = icon(
      ifelse(values$news_current[1] == 1, "thumbs-up", "thumbs-down"),
      lib = "glyphicon"
    ),
    color = ifelse(values$news_current[1] == 1, "green", "red"),
    fill = TRUE
  )
})  # End of the render Output.

# 5. Render Output  Info box to show the information for Does the package have Bug Report?

output$bugtrack <- renderInfoBox({
  req(values$has_bug_reports_url)
  infoBox(
    title = "Bugs publicly documented?",
    if(values$has_bug_reports_url[1] == 1){"YES"}
    else if(values$has_bug_reports_url[2] == -1){"NA"}
    else{"NO"},
    width = 3,
    if(values$has_bug_reports_url[2] == -1){"Metric is not applicable for this source of package"}
    else{ ifelse(values$has_bug_reports_url[1] == 1, paste("Bug reports URL:", values$has_bug_reports_url[2]), "The Bugs are not publicly documented")},
    icon = icon(
      ifelse(values$has_bug_reports_url[1] == 1, "thumbs-up", "thumbs-down"),
      lib = "glyphicon"
    ),
    color = ifelse(values$has_bug_reports_url[1] == 1, "green", "red"),
    fill = TRUE
  )
})  # End of the render Output.

# 6. Render Output Info box to show the information on Bugs Status.

output$bugstatus <- renderInfoBox({
  req(values$bugs_status)
  infoBox(
    title = "Bug closure",
    if(values$bugs_status[2] == -1){"NA"}
    else{paste0(values$bugs_status[1],"%")},
    subtitle = if(values$bugs_status[2] == -1){"Metric is not applicable for this source of package"}
    else{"Percentage of last 30 bugs closed"},
    width = 3,
    fill = TRUE
  )
})  # End of the render Output.

# 7. Render Output Info box to show the information on Export help.

output$exporthelp <- renderInfoBox({
  req(values$export_help)
  infoBox(
    title = "Documentation",
    if(values$export_help[2] == -1){"NA"}
    else{paste0(values$export_help[1],"%")},
    subtitle = if(values$export_help[2] == -1){"Metric is not applicable for this source of package"}
    else{"Proportion of exported objects documented"},
    width = 3,
    fill = TRUE
  )
}) # End of the render Output.

# 8. Render Output Info box to show the information on source code is public?.

output$source_pub <- renderInfoBox({
  req(values$has_source_control)
  infoBox(
    title = "Source code public?",
    if(values$has_source_control[1] == 1){"YES"}
    else if(values$has_source_control[2] == -1){"NA"}
    else{"NO"},
    width = 3,
    if(values$has_source_control[2] == -1){"Metric is not applicable for this soucre of package"}
    else{ ifelse(values$has_source_control[1] == 1, paste("Source code URL:", values$has_source_control[2]), "Package does not have a Source code URL")},
    icon = icon(
      ifelse(values$has_source_control[1] == 1, "thumbs-up", "thumbs-down"),
      lib = "glyphicon"
    ),
    color = ifelse(values$has_source_control[1] == 1, "green", "red"),
    fill = TRUE
  )
})  # End of the Render Output.

# 9. Render Output Info box to show the information on Has a package maintainer?.

output$pack_maint <- renderInfoBox({
  req(values$has_maintainer)
  infoBox(
    title = "Has a maintainer?",
    if(values$has_maintainer[1] == 1){"YES"}
    else if(values$has_maintainer[2] == -1){"NA"}
    else{"NO"},
    width = 3,
    if(values$has_maintainer[2] == -1){"Metric is not applicable for this soucre of package"}
    else{ ifelse(values$has_maintainer[1] == 1, values$has_maintainer[2], "Package does not have a Maintainer")},
    icon = icon(
      ifelse(
        values$has_maintainer[1] == 1, "thumbs-up", "thumbs-down"),
        lib = "glyphicon"
    ),
    color = ifelse(values$has_maintainer[1] == 1, "green", "red"),
    fill = TRUE
  )
})  # End of the render Output.

# 10. Render Output to show the comments on the application.

output$mm_commented <- renderText({
  if (values$mm_comment_submitted == "yes" ||
      values$mm_comment_submitted == "no") {
    values$comment_mm1 <-
      db_fun(
        paste0(
          "SELECT user_name, user_role, comment, added_on  FROM Comments WHERE comm_id = '",
          input$select_pack,
          "' AND comment_type = 'mm'"
        )
      )
    values$comment_mm2 <- data.frame(values$comment_mm1 %>% map(rev))
    req(values$comment_mm2$comment)
    values$mm_comment_submitted <- "no"
    paste(
      "<div class='col-sm-12 comment-border-bottom single-comment-div'><i class='fa fa-user-tie fa-4x'></i><h3 class='ml-3'><b class='user-name-color'>",
      values$comment_mm2$user_name,
      "(",
      values$comment_mm2$user_role,
      ")",
      "</b><sub>",
      values$comment_mm2$added_on,
      "</sub></h3><h4 class='ml-3 lh-4'>",
      values$comment_mm2$comment,
      "</h4></div>"
    )
  }
})  # End of the render Output.

# End of the Render Output's'.

values$mm_comment_submitted <- "no"

# Observe event for submit button.

observeEvent(input$submit_mm_comment, {
  if (trimws(input$mm_comment) != "") {
    db_ins(
      paste0(
        "INSERT INTO Comments values('",
        input$select_pack,
        "',",
        "'",
        values$name,
        "'," ,
        "'",
        values$role,
        "',",
        "'",
        input$mm_comment,
        "',",
        "'mm'," ,
        "'",
        TimeStamp(),
        "'"  ,
        ")"
      )
    )
    values$mm_comment_submitted <- "yes"
    updateTextAreaInput(session, "mm_comment", value = "")
    # After comment added to Comments table, update db dash
    values$db_pkg_overview <- update_db_dash()
  }
})  # End of the Observe Event.


# End of the Maintenance_Metrics Source file for Server Module.
