#####################################################################################################################
# mm_report.R - Maintenance_Metrics Source file for Server Module for Report Preview section.
# Author: K Aravind Reddy
# Date: July 13th, 2020
# License: MIT License
#####################################################################################################################

# 1. Observe to get the info box information from the risk metric package.
observe({
  req(input$select_pack)
  if(input$tabs == "reportPreview_tab_value"){
    if(input$select_pack != "Select"){
    
      values$riskmetrics_mm <-
        db_fun(
          paste0(
            "SELECT * FROM MaintenanceMetrics WHERE MaintenanceMetrics.mm_id ='",
            input$select_pack,
            "'"
          )
        )  
      
      values$has_vignettes <- c(strsplit(values$riskmetrics_mm$has_vignettes,",")[[1]][1], strsplit(values$riskmetrics_mm$has_vignettes,",")[[1]][2])
      values$has_website <- c(strsplit(values$riskmetrics_mm$has_website,",")[[1]][1], strsplit(values$riskmetrics_mm$has_website,",")[[1]][2])
      values$has_news <- c(strsplit(values$riskmetrics_mm$has_news,",")[[1]][1], strsplit(values$riskmetrics_mm$has_news,",")[[1]][2])
      values$news_current <- c(strsplit(values$riskmetrics_mm$news_current,",")[[1]][1], strsplit(values$riskmetrics_mm$news_current,",")[[1]][2])
      values$has_bug_reports_url <- c(strsplit(values$riskmetrics_mm$has_bug_reports_url,",")[[1]][1], strsplit(values$riskmetrics_mm$has_bug_reports_url,",")[[1]][2])
      values$bugs_status <- c(strsplit(values$riskmetrics_mm$bugs_status,",")[[1]][1], strsplit(values$riskmetrics_mm$bugs_status,",")[[1]][2])
      values$export_help <- c(strsplit(values$riskmetrics_mm$export_help,",")[[1]][1], strsplit(values$riskmetrics_mm$export_help,",")[[1]][2])
      values$has_source_control <- c(strsplit(values$riskmetrics_mm$has_source_control,",")[[1]][1], strsplit(values$riskmetrics_mm$has_source_control,",")[[1]][2])
      values$has_maintainer <- c(strsplit(values$riskmetrics_mm$has_maintainer,",")[[1]][1], strsplit(values$riskmetrics_mm$has_maintainer,",")[[1]][2])
    
      runjs("setTimeout(function(){ capturingSizeOfInfoBoxes(); }, 500);")
      req(values$riskmetrics_mm)
      if(values$has_vignettes[2] == -1){ runjs( "setTimeout(function(){ updateInfoBoxesWhenNA('vignette1');}, 3000);" ) }
      if(values$has_website[2] == -1){ runjs( "setTimeout(function(){ updateInfoBoxesWhenNA('website1');}, 3000);" ) }
      if(values$has_news[2] == -1){ runjs( "setTimeout(function(){ updateInfoBoxesWhenNA('hasnews1');}, 3000);" ) }
      if(values$news_current[2] == -1){ runjs( "setTimeout(function(){ updateInfoBoxesWhenNA('newscurrent1');}, 3000);" ) }
      if(values$has_bug_reports_url[2] == -1){ runjs( "setTimeout(function(){ updateInfoBoxesWhenNA('bugtrack1');}, 3000);" ) }
      if(values$bugs_status[2] == -1){ runjs( "setTimeout(function(){ updateInfoBoxesColorWhenNA('bugstatus1');}, 3000);" ) }
      if(values$export_help[2] == -1){ runjs( "setTimeout(function(){ updateInfoBoxesColorWhenNA('exporthelp1');}, 3000);" ) }
      if(values$has_source_control[2] == -1){ runjs( "setTimeout(function(){ updateInfoBoxesWhenNA('source_pub1');}, 3000);" ) }
      if(values$has_maintainer[2] == -1){ runjs( "setTimeout(function(){ updateInfoBoxesWhenNA('pack_maint1');}, 3000);" ) }
    }
  }
})  # End of the observe.

# End of the observe's'

# Start of the render Output's'

# 1. Render Output Info box to show the information on VIGNETTE Content.

output$vignette1 <- renderInfoBox({
  req(values$has_vignettes)
  infoBox(
    title = "Presence of vignettes?",
    if(values$has_vignettes[1] == 1){"YES"}
    else if(values$has_vignettes[2] == -1){"NA"}
    else{"NO"},
    width = 3,
    if(values$has_vignettes[2] == -1){"Metric is not applicable for this source of package"}
    else{paste("The package has", values$has_vignettes[2], "Vignettes")},
    icon = icon(
      ifelse(values$has_vignettes[1] == 1, "thumbs-up", "thumbs-down"),
      lib = "glyphicon"
    ),
    color = ifelse(values$has_vignettes[1] == 1, "green", "red"),
    fill = TRUE
  )
})  # End of the render Output.

# 2. Render Output Info box to show the information on Package Has Website.

output$website1 <- renderInfoBox({
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

output$hasnews1 <- renderInfoBox({
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

output$newscurrent1 <- renderInfoBox({
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

output$bugtrack1 <- renderInfoBox({
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

output$bugstatus1 <- renderInfoBox({
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

output$exporthelp1 <- renderInfoBox({
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

output$source_pub1 <- renderInfoBox({
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

output$pack_maint1 <- renderInfoBox({
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

output$mm_commented1 <- renderText({
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

# End of the Maintenance_Metrics Source file for Server Module.
