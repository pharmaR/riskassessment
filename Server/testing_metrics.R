#####################################################################################################################
# testing_metrics.R - testing_metrics Source file for server Module.
# Author: K Aravind Reddy
# Date: July 13th, 2020
# License: MIT License
#####################################################################################################################

# Start of the observe's'

# 1. Observe to load the columns from DB into below reactive values.

observe({
  req(values$selected_pkg$package != "Select", values$selected_pkg$version != "Select")
  if (input$tabs == "tm_tab_value") {

      values$riskmetrics_tm <-
        db_fun(
          paste0(
            "SELECT * FROM TestMetrics WHERE TestMetrics.tm_id ='",
            values$selected_pkg$package,
            "'"," and tm_ver = '", values$selected_pkg$version, "'", ""
          )
        )
      values$test_coverage <- c(strsplit(values$riskmetrics_tm$test_coverage,",")[[1]][1], strsplit(values$riskmetrics_tm$test_coverage,",")[[1]][2])
      
    if (!is.null(input$tm_comment)) {
      if(values$test_coverage[2] == -1){ runjs( "setTimeout(function(){ addTextToGaugeSVG('test_coverage');}, 500);" ) }
      req(values$selected_pkg$decision)
      if (values$selected_pkg$decision != "") {
        runjs("setTimeout(function(){ var ele = document.getElementById('tm_comment'); ele.disabled = true; }, 500);")
        runjs("setTimeout(function(){ var ele = document.getElementById('submit_tm_comment'); ele.disabled = true; }, 500);")
      } 
     }
  }
})  # End of the observe.

# End of the observe's'

# Start of the render Output's'

# 1. Render Output to show the test converage gauage.

output$test_coverage <- renderAmCharts({
  bands = data.frame(
    start = c(0, 40, 80),
    end = c(40, 80, 100),
    color = ifelse(values$test_coverage[2] != -1, c("#ea3838", "#ffac29", "#00CC00"), c("#808080", "#808080", "#808080")),
    stringsAsFactors = FALSE
  )
  bands2 = data.frame(
    start = c(0, 40, 80),
    end = c(40, 80, 100),
    color = ifelse(values$test_coverage[2] != -1, c("#ea3838", "#ffac29", "#00CC00"), c("#808080", "#808080", "#808080")),
    stringsAsFactors = FALSE
  )
  amAngularGauge(
    x = 0, #as.numeric(values$test_coverage[1]),
    start = 0,
    end = 100,
    bands = bands,
    secondAxe = TRUE,
    start2 = 0,
    end2 = 100,
    bands2 = bands2
  )
})  # End of the render Output.

# 2. Render Output to show the comments for testing metrics on the application.

output$tm_commented <- renderText({
  if (values$tm_comment_submitted == "yes" ||
      values$tm_comment_submitted == "no") {
    values$comment_tm1 <-
      db_fun(
        paste0(
          "SELECT user_name, user_role, comment, added_on FROM Comments",
          " WHERE comm_id = '", input$select_pack, "'",
          " AND  comm_ver = '", input$select_ver,  "'", 
          " AND comment_type = 'tm'"
        )
      )
    values$comment_tm2 <- data.frame(values$comment_tm1 %>% map(rev))
    req(values$comment_tm2$comment)
    values$tm_comment_submitted <- "no"
    paste(
      "<div class='col-sm-12 comment-border-bottom'><i class='fa fa-user-tie fa-4x'></i><h3 class='ml-3'><b class='user-name-color'>",
      values$comment_tm2$user_name,
      "(",
      values$comment_tm2$user_role,
      ")",
      "</b><sub>",
      values$comment_tm2$added_on,
      "</sub></h3><h4 class='ml-3 lh-4'>",
      values$comment_tm2$comment,
      "</h4></div>"
    )
  }
})  # End of the render Output.

# Observe event for submit button to submit the comments for testing metrics.

values$tm_comment_submitted <- "no"
observeEvent(input$submit_tm_comment, {
  if (trimws(input$tm_comment) != "") {
    db_ins(
      paste0(
        "INSERT INTO Comments values('",
             input$select_pack, "',",
        "'", input$select_ver,  "',",
        "'", values$name,  "'," ,
        "'", values$role,  "',",
        "'", input$tm_comment,  "',",
        "'tm',",  
        "'", TimeStamp(), "'" ,
        ")" 
      )
    )
    values$tm_comment_submitted <- "yes"
    updateTextAreaInput(session, "tm_comment", value = "")
  }
})  # End of the observe event.


# End of the Testing_metrics Source file for Server Module.
