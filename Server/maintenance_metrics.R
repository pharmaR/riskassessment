#####################################################################################################################
# login_screen.R - Maintenance_Metrics Source file for Server Module.
# 
# Author: Aravind
# Date: June 13th, 2020
#####################################################################################################################


# Start of the observe's'

# 1. Observe to load the columns from DB into below reactive values.

observe({
  req(input$select_pack)
  if(input$tabs == "mm_tab_value"){
    if(input$select_pack != "Select"){
      values$riskmetrics_mm <-
        db_fun(
          paste0(
            "SELECT * FROM MaintenanceMetrics WHERE MaintenanceMetrics.mm_id ='",
            input$select_pack,
            "'"
          )
        )
      
      risk_mm <- values$riskmetrics_mm
      values$vals <- map(risk_mm$mm_value, ~unlist(stringr::str_split(.x, ",",2)))

      # values$package_has_vignettes <- c(strsplit(values$riskmetrics_mm$package_has_vignettes,",")[[1]][1], strsplit(values$riskmetrics_mm$package_has_vignettes,",")[[1]][2])
      # values$package_has_website <- c(strsplit(values$riskmetrics_mm$package_has_website,",")[[1]][1], strsplit(values$riskmetrics_mm$package_has_website,",")[[1]][2])
      # values$package_has_news <- c(strsplit(values$riskmetrics_mm$package_has_news,",")[[1]][1], strsplit(values$riskmetrics_mm$package_has_news,",")[[1]][2])
      # values$news_is_current <- c(strsplit(values$riskmetrics_mm$news_is_current,",")[[1]][1], strsplit(values$riskmetrics_mm$news_is_current,",")[[1]][2])
      # values$has_bug_reports <- c(strsplit(values$riskmetrics_mm$has_bug_reports,",")[[1]][1], strsplit(values$riskmetrics_mm$has_bug_reports,",")[[1]][2])
      # values$status_of_last_30_reported_bugs <- c(strsplit(values$riskmetrics_mm$status_of_last_30_reported_bugs,",")[[1]][1], strsplit(values$riskmetrics_mm$status_of_last_30_reported_bugs,",")[[1]][2])
      # values$exported_objects_with_documentation <- c(strsplit(values$riskmetrics_mm$exported_objects_with_documentation,",")[[1]][1], strsplit(values$riskmetrics_mm$exported_objects_with_documentation,",")[[1]][2])
      # values$source_code_is_public <- c(strsplit(values$riskmetrics_mm$source_code_is_public,",")[[1]][1], strsplit(values$riskmetrics_mm$source_code_is_public,",")[[1]][2])
      # values$has_a_package_maintainer <- c(strsplit(values$riskmetrics_mm$has_a_package_maintainer,",")[[1]][1], strsplit(values$riskmetrics_mm$has_a_package_maintainer,",")[[1]][2])
  
      # runjs("setTimeout(function(){ capturingSizeOfInfoBoxes(); }, 500);")
      # if (!is.null(input$mm_comment)) {
      #     if(values$package_has_vignettes[2] == -1){ runjs( "setTimeout(function(){ updateInfoBoxesWhenNA('vignette');}, 500);" ) }
      #     if(values$package_has_website[2] == -1){ runjs( "setTimeout(function(){ updateInfoBoxesWhenNA('website');}, 500);" ) }
      #     if(values$package_has_news[2] == -1){ runjs( "setTimeout(function(){ updateInfoBoxesWhenNA('hasnews');}, 500);" ) }
      #     if(values$news_is_current[2] == -1){ runjs( "setTimeout(function(){ updateInfoBoxesWhenNA('newscurrent');}, 500);" ) }
      #     if(values$has_bug_reports[2] == -1){ runjs( "setTimeout(function(){ updateInfoBoxesWhenNA('bugtrack');}, 500);" ) }
      #     if(values$status_of_last_30_reported_bugs[2] == -1){ runjs( "setTimeout(function(){ updateInfoBoxesColorWhenNA('bugstatus');}, 500);" ) }
      #     if(values$exported_objects_with_documentation[2] == -1){ runjs( "setTimeout(function(){ updateInfoBoxesColorWhenNA('exporthelp');}, 500);" ) }
      #     if(values$source_code_is_public[2] == -1){ runjs( "setTimeout(function(){ updateInfoBoxesWhenNA('source_pub');}, 500);" ) }
      #     if(values$has_a_package_maintainer[2] == -1){ runjs( "setTimeout(function(){ updateInfoBoxesWhenNA('pack_maint');}, 500);" ) }
      #     req(values$selected_pkg$decision)
      #     if (values$selected_pkg$decision != "") {
      #       runjs("setTimeout(function(){disableUI('mm_comment')}, 500);")
      #       runjs("setTimeout(function(){disableUI('submit_mm_comment')}, 500);")
      #     }
      #  }
    }
  }
})  # End of the observe.

# End of the observe's'

# Start of the render Output's'

output$myboxes <- renderUI({
  boxes <- list()
  for (i in 1:length(values$riskmetrics_mm$mm_label)){
    if (grepl("\\?$",values$riskmetrics_mm$mm_label[[i]]) == FALSE) {
      boxes[[i]] <- info_percnt(values$riskmetrics_mm$mm_label[[i]], values$vals[[i]], 
                            eval(ifelse(values$vals[[i]][2] == -1, "Not Applicable", "Percentage of above")))
    } else {
      boxes[[i]] <- info_thumb(values$riskmetrics_mm$mm_label[[i]], values$vals[[i]], 
                           eval(ifelse(values$vals[[i]][2] == -1, "Not Applicable", 
                                       ifelse(values$vals[[i]][1] == 1, values$vals[[i]][2], "Nothing to see here.")))) 
    }
  }
  boxes
})




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
    db_fun(
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
  }
})  # End of the Observe Event.


# End of the Maintenance_Metrics Source file for Server Module.
