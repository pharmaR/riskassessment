#####################################################################################################################
# login_screen.R - Maintenance_Metrics Source file for Server Module.
# 
# Author: Aravind
# Date: June 13th, 2020
#####################################################################################################################


# Start of the observe's'

# 1. Observe to load the columns from DB into below reactive values.

observe({
  req(input$select_pack != "Select")
  if(input$tabs == "mm_tab_value"){
    if(input$select_pack != "Select"){
      risk_mm <-
        db_fun(
          paste0(
            "SELECT * FROM MaintenanceMetrics WHERE MaintenanceMetrics.mm_id ='",
            input$select_pack,
            "'"
          )
        )
      
      metrics_to_read <- file.path("Data", "metric.csv")
      maint_tbl <- readr::read_csv(metrics_to_read, col_types = cols(.default = "c", id = "i", weight = "n", is_thumb = "l"))
      
      # add labels
      values$risk_mm <- left_join(risk_mm, maint_tbl, by = c("mm_name" = "name")) %>% 
        filter(class == "maintenance") %>% 
        rename(mm_label = as_label, mm_detail = sc_descr)
      
    }
  }
})  # End of the observe.

# End of the observe's'

# Start of the render Output's'

output$myboxes <- renderUI({
  boxes <- list()
  vals <- map(values$risk_mm$mm_value, ~unlist(stringr::str_split(.x, ",",2)))
  
  for (i in 1:nrow(values$risk_mm)){
    if (values$risk_mm$is_thumb[[i]] == FALSE) {
      # if NA set to -1 for function to work properly
      vals[[i]][2] <- ifelse(vals[[i]][2] == "NA", -1, vals[[i]][2])
      # convert proportion to percentage
      vals[[i]][1] <- ifelse(vals[[i]][1] == "NA", NA, format(round(as.numeric(vals[[i]][1]) * 100, 2)))
      boxes[[i]] <- info_percnt(values$risk_mm$mm_name[[i]], vals[[i]], 
                                eval(ifelse(vals[[i]][2] == -1, "Metric is not applicable",
                                            values$risk_mm$mm_detail[[i]]))) 
    } else {
      # if we have a c() of text strings, take the first one.
      text <- str_split(vals[[i]][2],",")[[1]][1]
      text <- gsub('[\"]', '', text)
      text <- ifelse(str_detect(text,"[(]"),str_sub(text,3,nchar(text)),text)
      boxes[[i]] <- info_thumb(values$risk_mm$mm_name[[i]], vals[[i]], 
                               eval(ifelse(vals[[i]][2] == -1, "Metric is not applicable", 
                                           ifelse(vals[[i]][1] == 1, 
                                                  # if mm_label contains "vector" or "url" just show the text (the url)
                                                  ifelse(str_detect(values$risk_mm$mm_label[[i]],paste(c("vector","url"),collapse="|")), text,
                                                         # if this is a logical TRUE|FALSE just show the label
                                                         ifelse(vals[[i]][2] %in% c("TRUE","FALSE"), values$risk_mm$mm_label[[i]], 
                                                                # Number of...  
                                                                paste0(values$risk_mm$mm_label[[i]], ": ", vals[[i]][2])) ),
                                                  "Nothing to see here.")))) 
    }
  }
  boxes
})

# Render Output to show the comments on the application.

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
