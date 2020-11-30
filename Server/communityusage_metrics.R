#####################################################################################################################
# communityusage_metrics.R - Community Usage Metrics Source file for Server Module.  
# Author: K Aravind Reddy
# Date: July 13th, 2020
# License: MIT License
#####################################################################################################################


# Start of the observe's'

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
      
      # Load the data table column into reactive variable for time sice first release.
      values$time_since_first_release_info <-
        values$riskmetrics_cum$time_since_first_release[1]
      
      # Load the data table column into reactive variable for time sice version release.
      values$time_since_version_release_info <-
        values$riskmetrics_cum$time_since_version_release[1]
      
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

# End of the observe's'

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

output$no_of_downloads_data <- renderTable({
  values$riskmetrics_cum
})

# 3. Render Output to show the highchart for number of downloads on the application.
output$no_of_downloads <- 
  plotly::renderPlotly({
  # renderHighchart({

  if (values$riskmetrics_cum$no_of_downloads_last_year[1] != 0) {
      # hc <- highchart() %>%
      #   hc_xAxis(categories = values$riskmetrics_cum$month) %>%
      #   hc_xAxis(
      #     title = list(text = "Months"),
      #     
      #     plotLines = map2(values$riskmetrics_cum$ver_release, values$riskmetrics_cum$position,
      #           function(.x, .y)  list(label = list(text = .x), color = "#FF0000", width = 2, value = .y) )
      #     
      #   ) %>%
      #   hc_add_series(
      #     name = input$select_pack,
      #     data = values$riskmetrics_cum$no_of_downloads,
      #     color = "blue"
      #   ) %>%
      #   hc_yAxis(title = list(text = "Downloads")) %>%
      #   hc_title(text = "NUMBER OF DOWNLOADS IN PREVIOUS 11 MONTHS") %>%
      #   hc_subtitle(
      #     text = paste(
      #       "Total Number of Downloads :",
      #       sum(values$riskmetrics_cum$no_of_downloads)
      #     ),
      #     align = "right", 
      #     style = list(color = "#2b908f", fontWeight = "bold")
      #   ) %>%
      #   hc_add_theme(hc_theme_elementary())
    swap <- data.frame(month_num = paste(1:12), month_name = month.name)
    plot_dat <- values$riskmetrics_cum %>%
      mutate(month_name = stringr::word(month),
             year = stringr::word(month, -1)) %>%
      left_join(swap) %>%
      mutate(month_date = as.Date(
        paste("01", month_num, year, sep = "-")
        , "%d-%m-%Y", origin = "1970-01-01")
      )
    
    fig <- plot_ly(plot_dat, x = ~month_date, y = ~no_of_downloads,
                   name = "# Downloads", type = 'scatter', 
                   mode = 'lines+markers', line = list(color = "blue"),
                   hoverinfo = "text",
                   text = ~paste0('# Dwnlds: ', formatC(no_of_downloads, format="f", big.mark=",", digits=0),
                                  '<br>', month)) %>%
      layout(title = ~paste("NUMBER OF DOWNLOADS BY MONTH:", input$select_pack),
             showlegend = FALSE,
             yaxis = list(title = "Downloads"),
             xaxis = list(title = "Month")
      ) %>%
      plotly::config(displaylogo = FALSE, 
                     modeBarButtonsToRemove= c('sendDataToCloud', 'hoverCompareCartesian','hoverClosestCartesian','autoScale2d'
                                               ,'select2d', 'lasso2d', 'toggleSpikelines'
                                               # , 'toImage', 'resetScale2d', 'zoomIn2d', 'zoomOut2d','zoom2d', 'pan2d'
                     ))
    fig <- fig %>% add_segments(x = ~if_else(ver_release != "NA", month_date, NA_Date_),
                                xend = ~if_else(ver_release != "NA", month_date, NA_Date_),
                                y = ~.98*min(no_of_downloads),
                                yend = ~1.02*max(no_of_downloads),
                                name = "Version Release",
                                hoverinfo = "text",
                                text = ~paste0('Version: ', ver_release, '<br>', month),
                                line = list(color = "#FF0000")
    )
    fig <- fig %>% layout(
      xaxis = list(
        rangeselector = list(
          buttons = list(
            list(
              count = 3,
              label = "3 mo",
              step = "month",
              stepmode = "backward"),
            list(
              count = 6,
              label = "6 mo",
              step = "month",
              stepmode = "backward"),
            list(
              count = 1,
              label = "1 yr",
              step = "year",
              stepmode = "backward"),
            list(
              count = 2,
              label = "2 yr",
              step = "year",
              stepmode = "year"),
            list(step = "all"))),
        
        rangeslider = list(type = "date"))
    )
    # reveal plot
    fig
    } else {
      stop("Plot for packages with 0 downloads in last year has not been set up yet")
      # hc <- highchart() %>%
      #   hc_xAxis(categories = NULL) %>%
      #   hc_xAxis(title = list(text = "Months")) %>%
      #   hc_add_series(name = input$select_pack, data = NULL) %>%
      #   hc_yAxis(title = list(text = "Downloads")) %>%
      #   hc_title(text = "NUMBER OF DOWNLOADS IN PREVIOUS 11 MONTHS") %>%
      #   hc_subtitle(
      #     text = paste("Number of Downloads in the 11 previous months are zero"),
      #     style = list(color = "#f44336", fontWeight = "bold")
      #   ) %>%
      #   hc_add_theme(hc_theme_elementary())
    }
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

values$cum_comment_submitted <- "no"

# Start of the Observe Events.

# Observe event for cum comment submit button. 

observeEvent(input$submit_cum_comment, {
  if (trimws(input$cum_comment) != "") {
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
  }
})  # End of the submit button observe event.


# End of the Community Usage Metrics server source file.
