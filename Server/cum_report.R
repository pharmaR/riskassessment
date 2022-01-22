#####################################################################################################################
# com_report.R - Community Usage Metrics Source file for Server Module for Report Preview section.  
# Author: K Aravind Reddy
# Date: July 13th, 2020
# License: MIT License
#####################################################################################################################

# 1. Observe to load the columns to risk metric table.

observe({
  req(input$select_pack)
  
  if (input$tabs == "reportPreview_tab_value") {
    if(input$select_pack != "Select"){
    
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
      
      # Load the data table column into reactive variable for time sice first release.
      values$time_since_first_release_info <-
        values$riskmetrics_cum$time_since_first_release[1]
      
      # Load the data table column into reactive variable for time sice version release.
      values$time_since_version_release_info <-
        values$riskmetrics_cum$time_since_version_release[1]
      
      # Load the data table column into reactive variable for num dwnlds in year
      values$no_of_downloads_last_year_info <-
        values$riskmetrics_cum$no_of_downloads_last_year[1]
      
      runjs( "setTimeout(function(){ capturingSizeOfInfoBoxes(); }, 100);" )
      
      req(values$riskmetrics_cum)
      if(values$time_since_version_release_info == "NA"){ runjs( "setTimeout(function(){ updateInfoBoxesColorWhenNA('time_since_version_release1');}, 3000);" ) }
      if(values$time_since_first_release_info == "NA"){ runjs( "setTimeout(function(){ updateInfoBoxesColorWhenNA('time_since_first_release1');}, 3000);" ) }
      if (values$riskmetrics_cum$no_of_downloads_last_year[1] == 0) { runjs("setTimeout(function(){ updateText('no_of_downloads1');}, 3000);") }
    } 
  }
})  # End of the observe.

# End of the observe's'

# Start of the render Output's'

# 1. Render Output info box to show the content for time since first release.

output$time_since_first_release1 <- renderInfoBox({
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

output$time_since_version_release1 <- renderInfoBox({
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

output$dwnlds_last_yr1 <- renderInfoBox({
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
output$no_of_downloads1 <- 
  plotly::renderPlotly({
    num_dwnlds_plot(data = values$riskmetrics_cum,
                    input_select_pack = input$select_pack)
  })
  

# 4. Render output to show the comments.
output$cum_commented1 <- renderText({
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
    values$comment_cum2 <- data.frame(values$comment_cum1 %>% map(rev))
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