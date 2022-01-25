# IntroJS.
introJSServer(id = "cum_introJS", text = cum_steps)

# Call module to create comments and save the output.
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