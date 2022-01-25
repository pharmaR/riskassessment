# IntroJS.
introJSServer(id = "cum_introJS", text = cum_steps)

# Load the columns from DB into reactive values.
observe({
  req(selected_pkg$name())
  if (input$tabs == "cum_tab_value") {
    if (selected_pkg$name() != "-") {
      
      # Load the columns into values$riskmetrics.
      pkgs_in_db <- db_fun(paste0("SELECT cum_id FROM community_usage_metrics"))
      
      if (selected_pkg$name() %in% pkgs_in_db$cum_id &&
          !identical(pkgs_in_db$cum_id, character(0))) {
        values$riskmetrics_cum <-
          db_fun(glue(
            "SELECT *
              FROM community_usage_metrics
              WHERE cum_id ='{selected_pkg$name()}'"
          )
          )
      } else{
        if (selected_pkg$name() != "-") {
          metric_cum_Info_upload_to_DB(selected_pkg$name())
          values$riskmetrics_cum <-
            db_fun(
              glue("SELECT *
                   FROM community_usage_metrics
                   WHERE cum_id ='{selected_pkg$name()}'"
              )
            )
        }
      }
      
      # Load the data table column into reactive variable for time since first release.
      values$time_since_first_release_info <-
        values$riskmetrics_cum$time_since_first_release[1]
      
      # Load the data table column into reactive variable for time since version release.
      values$time_since_version_release_info <-
        values$riskmetrics_cum$time_since_version_release[1]
      
      # Load the data table column into reactive variable for num dwnlds in year
      values$no_of_downloads_last_year_info <-
        values$riskmetrics_cum$no_of_downloads_last_year[1]
      
      runjs( "setTimeout(function(){ capturingSizeOfInfoBoxes(); }, 100);" )
      
      if (!is.null(input$cum_comment)) {
        if(values$time_since_version_release_info == "NA"){ runjs( "setTimeout(function(){ updateInfoBoxesColorWhenNA('time_since_version_release');}, 500);" ) }
        if(values$time_since_first_release_info == "NA"){ runjs( "setTimeout(function(){ updateInfoBoxesColorWhenNA('time_since_first_release');}, 500);" ) }
        if (values$riskmetrics_cum$no_of_downloads_last_year[1] == 0) { runjs("setTimeout(function(){ updateText('no_of_downloads');}, 500);") }
        req(values$selected_pkg$decision)
        if (values$selected_pkg$decision != "") {
          runjs("setTimeout(function(){ var ele = document.getElementById('cum_comment'); ele.disabled = true; }, 500);" )
          runjs("setTimeout(function(){ var ele = document.getElementById('submit_cum_comment'); ele.disabled = true; }, 500);")
        }
      }
    }
  }
})  # End of the observe.

# End of the observes.

# Start of the render Output's'

# 1. Render Output info box to show the content for time since first release.

output$time_since_first_release <- renderInfoBox({
  req(values$time_since_first_release_info)
  infoBox(
    title = "Package Maturity",
    values$time_since_first_release_info,
    subtitle = ifelse(values$time_since_first_release_info != "NA",
                      "Months since first release.",
                      "Metric is not applicable for this source of package."),
    icon = shiny::icon("calendar"),
    width = 3,
    fill = TRUE
  )
})  # End of the time since first release render Output.

# 2. Render Output info box to show the content for time since version release.

output$time_since_version_release <- renderInfoBox({
  req(values$time_since_version_release_info)
  infoBox(
    title = "Version Maturity",
    values$time_since_version_release_info,
    subtitle = ifelse(values$time_since_version_release_info != "NA", 
                      "Months since version release.",
                      "Metric is not applicable for this source of package."),
    icon = shiny::icon("calendar"),
    width = 3,
    fill = TRUE
  )
  
})  # End of the time since version release render Output.



# 2.5 Render Output info box to show the number of downloads last year

output$dwnlds_last_yr <- renderInfoBox({
  req(values$no_of_downloads_last_year_info)
  infoBox(
    title = "Download Count",
    formatC(values$no_of_downloads_last_year_info, format="f", big.mark=",", digits=0),
    subtitle = ifelse(values$no_of_downloads_last_year_info != "NA",
                      "Downloads in Last Year",
                      "Metric is not applicable for this source of package."),
    icon = shiny::icon("signal"),
    width = 3,
    fill = TRUE
  )
  
})  # End 


# 3. Render Output to show the plot for number of downloads on the application.
output$no_of_downloads <- 
  plotly::renderPlotly({
    num_dwnlds_plot(data = values$riskmetrics_cum,
                    input_select_pack = selected_pkg$name())
  })



cum_comment_added <- addCommentServer(id = "add_comment_for_cum",
                                      metric_abrv = 'cum',
                                      user_name = reactive(user$name),
                                      user_role = reactive(user$role),
                                      pkg_name = selected_pkg$name)

# View comments.
viewCommentsServer(id = "view_cum_comments",
                   comment_added = cum_comment_added,
                   pkg_name = selected_pkg$name,
                   comment_type = 'cum')


observeEvent(input$submit_cum_comment, {
  if (trimws(input$cum_comment) != "") {
    db_ins(
      paste0(
        "INSERT INTO Comments values('",
        selected_pkg$name(),
        "',",
        "'",
        values$name,
        "'," ,
        "'",
        values$role,
        "',",
        "'",
        input$cum_comment,
        "',",
        "'cum',",
        "'",
        TimeStamp(),
        "'"  ,
        ")" 
      )
    )
    updateTextAreaInput(session, "cum_comment", value = "")
    # After comment added to Comments table, update db dash
  }
}) 