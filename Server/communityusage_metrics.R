#####################################################################################################################
# communityusage_metrics.R - Community Usage Metrics Source file for Server Module.  
# Author: K Aravind Reddy
# Date: July 13th, 2020
# License: MIT License
#####################################################################################################################


# Implement the intro logic. Sidebar steps are listed in global.r
# this dataset is also static... perhaps it should be sourced from global.r?
cum_steps <- reactive(
  data.frame(
    # Note that we access chooseCSVtext with '.' instead of '#', because we track its class and not its id.
    element = c("#cum_infoboxes", "#cum_plot", "#cum_add_comment", "#cum_prev_comments"),
    intro = c(
      "Several ways of measuring community usage assessed here. Please review!",
      "Digest downloads per month by selecting a pre-defined time periods or toggling the date slider at bottom of plot for custom date range",
      "Have something to share within your organization? Add a comment.",
      "Keep track of the on-going conversation for this package's community usage"
    ),
    position = c("bottom", rep("top", 3))
  )
)


# Start introjs when help button is pressed.
observeEvent(input$help_cum,
             rintrojs::introjs(session,
                     options = list(
                       steps = 
                         cum_steps() %>%
                         union(sidebar_steps),
                       "nextLabel" = "Next",
                       "prevLabel" = "Previous"
                     )
             )
)


# Start of the observe.

# 1. Observe to load the columns from DB into reactive values.
observe({
  req(input$select_pack)
  if (input$tabs == "cum_tab_value") {
    if (input$select_pack != "Select") {
    
      # Load the columns into values$riskmetrics.
      pkgs_in_db <- db_fun(paste0("SELECT cum_id FROM CommunityUsageMetrics"))
      
      if (input$select_pack %in% pkgs_in_db$cum_id &&
          !identical(pkgs_in_db$cum_id, character(0))) {
        values$riskmetrics_cum <-
          db_fun(
            paste0(
              "SELECT * FROM CommunityUsageMetrics WHERE cum_id ='",
              input$select_pack,
              "'"
            )
          )
      } else{
        if (input$select_pack != "Select") {
          metric_cum_Info_upload_to_DB(input$select_pack)
          values$riskmetrics_cum <-
            db_fun(
              paste0(
                "SELECT * FROM CommunityUsageMetrics WHERE cum_id ='",
                input$select_pack,
                "'"
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
      
      shinyjs::runjs( "setTimeout(function(){ capturingSizeOfInfoBoxes(); }, 100);" )
      
      if (!is.null(input$cum_comment)) {
        if(values$time_since_version_release_info == "NA"){ shinyjs::runjs( "setTimeout(function(){ updateInfoBoxesColorWhenNA('time_since_version_release');}, 500);" ) }
        if(values$time_since_first_release_info == "NA"){ shinyjs::runjs( "setTimeout(function(){ updateInfoBoxesColorWhenNA('time_since_first_release');}, 500);" ) }
        if (values$riskmetrics_cum$no_of_downloads_last_year[1] == 0) { shinyjs::runjs("setTimeout(function(){ updateText('no_of_downloads');}, 500);") }
        req(values$selected_pkg$decision)
        if (values$selected_pkg$decision != "") {
          shinyjs::runjs("setTimeout(function(){ var ele = document.getElementById('cum_comment'); ele.disabled = true; }, 500);" )
          shinyjs::runjs("setTimeout(function(){ var ele = document.getElementById('submit_cum_comment'); ele.disabled = true; }, 500);")
        }
      }
    }
  }
})  # End of the observe.

# End of the observes.

# Start of the render Output's'

# 1. Render Output info box to show the content for time since first release.

output$time_since_first_release <- shinydashboard::renderInfoBox({
  req(values$time_since_first_release_info)
  shinydashboard::infoBox(
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

output$time_since_version_release <- shinydashboard::renderInfoBox({
  req(values$time_since_version_release_info)
  shinydashboard::infoBox(
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

output$dwnlds_last_yr <- shinydashboard::renderInfoBox({
  req(values$no_of_downloads_last_year_info)
  shinydashboard::infoBox(
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
                    input_select_pack = input$select_pack)
})



# 4. Render output to show the comments.

output$cum_commented <- renderText({
  if (values$cum_comment_submitted == "yes" ||
      values$cum_comment_submitted == "no") {
    values$comment_cum1 <-
      db_fun(
        paste0(
          "SELECT user_name, user_role, comment, added_on  FROM Comments WHERE comm_id = '",
          input$select_pack,
          "' AND comment_type = 'cum'"
        )
      )
    values$comment_cum2 <- data.frame(values$comment_cum1 %>% purrr::map(rev))
    req(values$comment_cum2$comment)
    values$cum_comment_submitted <- "no"
    paste(
      "<div class='col-sm-12 comment-border-bottom'><i class='fa fa-user-tie fa-4x'></i><h3 class='ml-3'><b class='user-name-color'>",
      values$comment_cum2$user_name,
      "(",
      values$comment_cum2$user_role,
      ")",
      "</b><sub>",
      values$comment_cum2$added_on,
      "</sub></h3><h4 class='ml-3 lh-4'>",
      values$comment_cum2$comment,
      "</h4></div>"
    )
  }
})  # End of the render output for comments.

# End of the Render Output's'.

values$cum_comment_submitted <- "no"

# Start of the Observe Events.

# Observe event for cum comment submit button. 

observeEvent(input$submit_cum_comment, {
  if (trimws(input$cum_comment) != "") {
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
        input$cum_comment,
        "',",
        "'cum',",
        "'",
        TimeStamp(),
        "'"  ,
        ")" 
      )
    )
    values$cum_comment_submitted <- "yes"
    updateTextAreaInput(session, "cum_comment", value = "")
    # After comment added to Comments table, update db dash
    values$db_pkg_overview <- update_db_dash()
  }
})  # End of the submit button observe event.


# End of the Community Usage Metrics server source file.
