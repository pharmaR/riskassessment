# Implement the intro logic. Sidebar steps are listed in global.r
# this dataset is also static... perhaps it should be sourced from global.r?
mm_steps <- reactive(
  data.frame(
    # Note that we access chooseCSVtext with '.' instead of '#', because we track its class and not its id.
    element = c("#mm_infoboxes", "#mm_add_comment", "#mm_prev_comments"),
    intro = c(
      "Several ways of measuring package maintenance best practices are assessed here. Please review!",
      "Have something to share within your organization? Add a comment.",
      "Keep track of the on-going conversation for this package's maintainence metrics"
    ),
    position = c(rep("top", 3))
  )
)


# Start introjs when help button is pressed.
observeEvent(input$help_mm,
   introjs(session,
     options = list(
       steps = 
         mm_steps() %>%
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
  if(input$tabs == "mm_tab_value"){
    if(selected_pkg$name() != "-"){
      
      package_id <- 
        db_fun(glue(
        "SELECT id
        FROM package
        WHERE name = '{selected_pkg$name()}';"))
      
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
      
      # if (values$selected_pkg$decision != "") {
      #   runjs("setTimeout(function(){disableUI('mm_comment')}, 500);")
      #   runjs("setTimeout(function(){disableUI('submit_mm_comment')}, 500);")
      # }
    }
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
output$mm_commented <- renderText({
  if (values$mm_comment_submitted == "yes" ||
      values$mm_comment_submitted == "no") {
    values$comment_mm1 <-
      db_fun(
        glue(
          "SELECT user_name, user_role, comment, added_on
          FROM Comments
          -WHERE comm_id = '{selected_pkg$name()}' AND comment_type = 'mm'"
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
    db_ins(glue(
      "INSERT INTO Comments values('{selected_pkg$name()}', '{values$name}', 
      '{values$role}', '{input$mm_comment}', 'mm', '{TimeStamp()}')"
      )
    )
    values$mm_comment_submitted <- "yes"
    updateTextAreaInput(session, "mm_comment", value = "")
    # After comment added to Comments table, update db dash
    values$db_pkg_overview <- update_db_dash()
  }
})  # End of the Observe Event.


# End of the Maintenance_Metrics Source file for Server Module.
