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
      
      values$package_has_vignettes <- c(strsplit(values$riskmetrics_mm$package_has_vignettes,",")[[1]][1], strsplit(values$riskmetrics_mm$package_has_vignettes,",")[[1]][2])
      values$package_has_website <- c(strsplit(values$riskmetrics_mm$package_has_website,",")[[1]][1], strsplit(values$riskmetrics_mm$package_has_website,",")[[1]][2])
      values$package_has_news <- c(strsplit(values$riskmetrics_mm$package_has_news,",")[[1]][1], strsplit(values$riskmetrics_mm$package_has_news,",")[[1]][2])
      values$news_is_current <- c(strsplit(values$riskmetrics_mm$news_is_current,",")[[1]][1], strsplit(values$riskmetrics_mm$news_is_current,",")[[1]][2])
      values$has_bug_reports <- c(strsplit(values$riskmetrics_mm$has_bug_reports,",")[[1]][1], strsplit(values$riskmetrics_mm$has_bug_reports,",")[[1]][2])
      values$status_of_last_30_reported_bugs <- c(strsplit(values$riskmetrics_mm$status_of_last_30_reported_bugs,",")[[1]][1], strsplit(values$riskmetrics_mm$status_of_last_30_reported_bugs,",")[[1]][2])
      values$exported_objects_with_documentation <- c(strsplit(values$riskmetrics_mm$exported_objects_with_documentation,",")[[1]][1], strsplit(values$riskmetrics_mm$exported_objects_with_documentation,",")[[1]][2])
      values$source_code_is_public <- c(strsplit(values$riskmetrics_mm$source_code_is_public,",")[[1]][1], strsplit(values$riskmetrics_mm$source_code_is_public,",")[[1]][2])
      values$has_a_package_maintainer <- c(strsplit(values$riskmetrics_mm$has_a_package_maintainer,",")[[1]][1], strsplit(values$riskmetrics_mm$has_a_package_maintainer,",")[[1]][2])
    
      runjs("setTimeout(function(){ capturingSizeOfInfoBoxes(); }, 500);")
      req(values$riskmetrics_mm)
      if(values$package_has_vignettes[2] == -1){ runjs( "setTimeout(function(){ updateInfoBoxesWhenNA('vignette1');}, 3000);" ) }
      if(values$package_has_website[2] == -1){ runjs( "setTimeout(function(){ updateInfoBoxesWhenNA('website1');}, 3000);" ) }
      if(values$package_has_news[2] == -1){ runjs( "setTimeout(function(){ updateInfoBoxesWhenNA('hasnews1');}, 3000);" ) }
      if(values$news_is_current[2] == -1){ runjs( "setTimeout(function(){ updateInfoBoxesWhenNA('newscurrent1');}, 3000);" ) }
      if(values$has_bug_reports[2] == -1){ runjs( "setTimeout(function(){ updateInfoBoxesWhenNA('bugtrack1');}, 3000);" ) }
      if(values$status_of_last_30_reported_bugs[2] == -1){ runjs( "setTimeout(function(){ updateInfoBoxesColorWhenNA('bugstatus1');}, 3000);" ) }
      if(values$exported_objects_with_documentation[2] == -1){ runjs( "setTimeout(function(){ updateInfoBoxesColorWhenNA('exporthelp1');}, 3000);" ) }
      if(values$source_code_is_public[2] == -1){ runjs( "setTimeout(function(){ updateInfoBoxesWhenNA('source_pub1');}, 3000);" ) }
      if(values$has_a_package_maintainer[2] == -1){ runjs( "setTimeout(function(){ updateInfoBoxesWhenNA('pack_maint1');}, 3000);" ) }
    }
  }
})  # End of the observe.

# End of the observe's'

# Start of the render Output's'

# 1. Render Output Info box to show the information on VIGNETTE Content.

output$vignette1 <- renderInfoBox({
  req(values$package_has_vignettes)
  infoBox(
    title = "Presence of vignettes?",
    if(values$package_has_vignettes[1] == 1){"YES"}
    else if(values$package_has_vignettes[2] == -1){"NA"}
    else{"NO"},
    width = 3,
    if(values$package_has_vignettes[2] == -1){"Metric is not applicable for this source of package"}
    else{paste("The package has", values$package_has_vignettes[2], "Vignettes")},
    icon = icon(
      ifelse(values$package_has_vignettes[1] == 1, "thumbs-up", "thumbs-down"),
      lib = "glyphicon"
    ),
    color = ifelse(values$package_has_vignettes[1] == 1, "green", "red"),
    fill = TRUE
  )
})  # End of the render Output.

# 2. Render Output Info box to show the information on Package Has Website.

output$website1 <- renderInfoBox({
  req(values$package_has_website)
  infoBox(
    title = "Associated website URL?",
    if(values$package_has_website[1] == 1){"YES"}
    else if(values$package_has_website[2] == -1){"NA"}
    else{"NO"},
    width = 3,
    if(values$package_has_website[2] == -1){"Metric is not applicable for this source of package"}
    else{ ifelse(values$package_has_website[1] == 1, paste("Website:",values$package_has_website[2]), "The package does not have an associated website URL")},
    icon = icon(
      ifelse(values$package_has_website[1] == 1, "thumbs-up", "thumbs-down"),
      lib = "glyphicon"
    ),
    color = ifelse(values$package_has_website[1] == 1, "green", "red"),
    fill = TRUE
  )
})  # End of the render Output.
# 3. Render Output Info box to show the Package Has News? Content.

output$hasnews1 <- renderInfoBox({
  req(values$package_has_news)
  infoBox(
    title = "NEWS?",
    if(values$package_has_news[1] == 1){"YES"}
    else if(values$package_has_news[2] == -1){"NA"}
    else{"NO"},
    width = 3,
    if(values$package_has_news[2] == -1){"Metric is not applicable for this source of package"}
    else{ ifelse(values$package_has_news[1] == 1, "The package has a NEWS file.", "The package does not have a NEWS file")},
    icon = icon(
      ifelse(values$package_has_news[1] == 1, "thumbs-up", "thumbs-down"),
      lib = "glyphicon"
    ),
    color = ifelse(values$package_has_news[1] == 1, "green", "red"),
    fill = TRUE
  )
})  # End of the render Output.
# 4. Render Output Info box to show the information for News is Current?

output$newscurrent1 <- renderInfoBox({
  req(values$news_is_current)
  infoBox(
    title = "News is current?",
    if(values$news_is_current[1] == 1){"YES"}
    else if(values$news_is_current[2] == -1){"NA"}
    else{"NO"},
    width = 3,
    if(values$news_is_current[2] == -1){"Metric is not applicable for this source of package"}
    else{ ifelse(values$news_is_current[1] == 1, "NEWS file contains entry for current version number", "NEWS file does not contains entry for current version number")},
    icon = icon(
      ifelse(values$news_is_current[1] == 1, "thumbs-up", "thumbs-down"),
      lib = "glyphicon"
    ),
    color = ifelse(values$news_is_current[1] == 1, "green", "red"),
    fill = TRUE
  )
})  # End of the render Output.

# 5. Render Output  Info box to show the information for Does the package have Bug Report?

output$bugtrack1 <- renderInfoBox({
  req(values$has_bug_reports)
  infoBox(
    title = "Bugs publicly documented?",
    if(values$has_bug_reports[1] == 1){"YES"}
    else if(values$has_bug_reports[2] == -1){"NA"}
    else{"NO"},
    width = 3,
    if(values$has_bug_reports[2] == -1){"Metric is not applicable for this source of package"}
    else{ ifelse(values$has_bug_reports[1] == 1, paste("Bug reports URL:", values$has_bug_reports[2]), "The Bugs are not publicly documented")},
    icon = icon(
      ifelse(values$has_bug_reports[1] == 1, "thumbs-up", "thumbs-down"),
      lib = "glyphicon"
    ),
    color = ifelse(values$has_bug_reports[1] == 1, "green", "red"),
    fill = TRUE
  )
})  # End of the render Output.

# 6. Render Output Info box to show the information on Bugs Status.

output$bugstatus1 <- renderInfoBox({
  req(values$status_of_last_30_reported_bugs)
  infoBox(
    title = "Bug closure",
    if(values$status_of_last_30_reported_bugs[2] == -1){"NA"}
    else{paste0(values$status_of_last_30_reported_bugs[1],"%")},
    subtitle = if(values$status_of_last_30_reported_bugs[2] == -1){"Metric is not applicable for this source of package"}
    else{"Percentage of last 30 bugs closed"},
    width = 3,
    fill = TRUE
  )
})  # End of the render Output.

# 7. Render Output Info box to show the information on Export help.

output$exporthelp1 <- renderInfoBox({
  req(values$exported_objects_with_documentation)
  infoBox(
    title = "Documentation",
    if(values$exported_objects_with_documentation[2] == -1){"NA"}
    else{paste0(values$exported_objects_with_documentation[1],"%")},
    subtitle = if(values$exported_objects_with_documentation[2] == -1){"Metric is not applicable for this source of package"}
    else{"Proportion of exported objects documented"},
    width = 3,
    fill = TRUE
  )
}) # End of the render Output.

# 8. Render Output Info box to show the information on source code is public?.

output$source_pub1 <- renderInfoBox({
  req(values$source_code_is_public)
  infoBox(
    title = "Source code public?",
    if(values$source_code_is_public[1] == 1){"YES"}
    else if(values$source_code_is_public[2] == -1){"NA"}
    else{"NO"},
    width = 3,
    if(values$source_code_is_public[2] == -1){"Metric is not applicable for this soucre of package"}
    else{ ifelse(values$source_code_is_public[1] == 1, paste("Source code URL:", values$source_code_is_public[2]), "Package does not have a Source code URL")},
    icon = icon(
      ifelse(values$source_code_is_public[1] == 1, "thumbs-up", "thumbs-down"),
      lib = "glyphicon"
    ),
    color = ifelse(values$source_code_is_public[1] == 1, "green", "red"),
    fill = TRUE
  )
})  # End of the Render Output.

# 9. Render Output Info box to show the information on Has a package maintainer?.

output$pack_maint1 <- renderInfoBox({
  req(values$has_a_package_maintainer)
  infoBox(
    title = "Has a maintainer?",
    if(values$has_a_package_maintainer[1] == 1){"YES"}
    else if(values$has_a_package_maintainer[2] == -1){"NA"}
    else{"NO"},
    width = 3,
    if(values$has_a_package_maintainer[2] == -1){"Metric is not applicable for this soucre of package"}
    else{ ifelse(values$has_a_package_maintainer[1] == 1, values$has_a_package_maintainer[2], "Package does not have a Maintainer")},
    icon = icon(
      ifelse(
        values$has_a_package_maintainer[1] == 1, "thumbs-up", "thumbs-down"),
      lib = "glyphicon"
    ),
    color = ifelse(values$has_a_package_maintainer[1] == 1, "green", "red"),
    fill = TRUE
  )
})  # End of the render Output.

# 10. Render Output to show the comments on the application.

output$mm_commented1 <- renderText({
  if (values$mm_comment_submitted == "yes" ||
      values$mm_comment_submitted == "no") {
    values$comment_mm2 <- select_comments(input$select_pack, "mm")
    req(values$comment_mm2$comment)
    values$mm_comment_submitted <- "no"
    display_comments(values$comment_mm2)
  }
})  # End of the render Output.

# End of the Render Output's'.

# End of the Maintenance_Metrics Source file for Server Module.
