#####################################################################################################################
# tm_report.R - testing_metrics Source file for server Module for Report Preview section.
# Author: K Aravind Reddy
# Date: July 13th, 2020
# License: MIT License
#####################################################################################################################

# 1. Observe to check the report preview value
observeEvent(input$tabs, {
  req(input$select_pack)
  if (input$tabs == "reportPreview_tab_value") {
    if(input$select_pack != "Select"){
      
      package_id <- db_fun(paste0("SELECT id FROM package WHERE name = ", "'", input$select_pack, "'", ";"))
      metric_id <- db_fun(paste0("SELECT id FROM metric WHERE class = 'code_coverage';"))
      values$code_coverage <- db_fun(
        paste0("SELECT value FROM package_metrics WHERE ",
               "package_id = ", package_id,
               " AND ",
               "metric_id = ", metric_id,
               ";"
        )
      )
      values$covr_coverage <- values$covr_coverage$value
      
      req(values$covr_coverage)
      if(values$covr_coverage == "pkg_metric_error")
        runjs("setTimeout(function(){ addTextToGaugeSVG('test_coverage1');}, 5000);")
    }
  }
})  # End of the observe.

# End of the observe's'

# Start of the render Output's'

# 1. Render Output to show the test converage gauage.

output$test_coverage1 <- renderAmCharts({
  bands = data.frame(
    start = c(0, 40, 80),
    end = c(40, 80, 100),
    color = ifelse(values$covr_coverage != "pkg_metric_error",
                   c("#ea3838", "#ffac29", "#00CC00"),
                   c("#808080", "#808080", "#808080")),
    stringsAsFactors = FALSE
  )
  bands2 = data.frame(
    start = c(0, 40, 80),
    end = c(40, 80, 100),
    color = ifelse(values$covr_coverage != "pkg_metric_error",
                   c("#ea3838", "#ffac29", "#00CC00"),
                   c("#808080", "#808080", "#808080")),
    stringsAsFactors = FALSE
  )
  amAngularGauge(
    x = as.numeric(ifelse(values$covr_coverage == "NA", 0, values$covr_coverage)),
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

output$tm_commented1 <- renderText({
  if (values$tm_comment_submitted == "yes" ||
      values$tm_comment_submitted == "no") {
    values$comment_tm1 <-
      db_fun(
        paste0(
          "SELECT user_name, user_role, comment, added_on FROM Comments WHERE comm_id = '",
          input$select_pack,
          "' AND comment_type = 'tm'"
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
