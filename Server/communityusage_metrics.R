# IntroJS.
introJSServer(id = "cum_introJS", text = cum_steps)

#' Creates three metricBoxes: the time since first release, the time since
#' latest release, and the number of downloads since last year.
observeEvent(community_usage_metrics(), {

  # Get the first package release.
  first_version <- community_usage_metrics() %>%
    filter(year == min(year)) %>%
    filter(month == min(month)) %>%
    slice_head(n = 1)
  
  # Get difference between today and first release in years.
  time_diff_first_version <- year(Sys.Date()) - first_version$year
  first_version_label <- 'Years'
  if(time_diff_first_version == 0){
    # Get difference in months.
    time_diff_first_version <- month(Sys.Date()) - first_version$month
    first_version_label <- 'Months'
  }
  if(time_diff_first_version == 1)
    first_version_label <- str_remove(
      string = first_version_label, pattern = 's$')
  
  metricBoxServer(id = 'time_since_first_version',
                  title = 'First Version Release',
                  desc = 'Time passed since first version release',
                  value = glue('{time_diff_first_version}
                               {first_version_label} Ago'),
                  succ_icon = 'black-tie',
                  icon_class = "text-info")
  
  # Get the last package release.
  last_version <- community_usage_metrics() %>%
    filter(year == max(year)) %>%
    filter(month == max(month)) %>%
    slice_head(n = 1)
  
  # Get difference between today and latest release.
  time_diff_latest_version <- year(Sys.Date()) - last_version$year
  latest_version_label <- 'Years'
  if(time_diff_latest_version == 0) {
    # Get difference in months.
    time_diff_latest_version <- month(Sys.Date()) - last_version$month
    latest_version_label <- 'Months'
  }
  if(time_diff_latest_version == 1)
    latest_version_label <- str_remove(
      string = latest_version_label, pattern = 's$')
  
  metricBoxServer(id = 'time_since_latest_version',
                  title = 'Lastest Version Release',
                  desc = 'Time passed since latest version release',
                  value = glue('{time_diff_latest_version}
                               {latest_version_label} Ago'),
                  succ_icon = 'meteor',
                  icon_class = "text-info")
  
  downloads_last_year <- community_usage_metrics() %>%
    filter(year == year(Sys.Date()) - 1) %>%
    distinct(year, month, downloads)
  
  metricBoxServer(id = 'downloads_last_year',
                  title = 'Package Downloads',
                  desc = 'Number of downloads since last year',
                  value = format(sum(downloads_last_year$downloads), big.mark = ","),
                  succ_icon = 'box-open',
                  icon_class = "text-info")
})

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


output$downloads_plot <- plotly::renderPlotly({

  community_data <- community_usage_metrics() %>%
    mutate(day_month_year = glue('1-{month}-{year}')) %>%
    mutate(day_month_year = as.Date(day_month_year, "%d-%m-%Y")) %>%
    mutate(month_year = glue('{months(day_month_year)} {year}')) %>%
    mutate(month = month.name[month]) %>%
    arrange(day_month_year)
  
  downloads_data <- community_data %>%
    distinct(month, year, .keep_all = TRUE)
  
  downloads_data$version[is.na(downloads_data$version)] <- ""
  
  # Last day that appears on the community metrics.
  latest_date <- downloads_data %>%
    slice_max(day_month_year) %>%
    pull(day_month_year)
  
  # Last day associated with a version release.
  last_version_date <- downloads_data %>%
    filter(!(version %in% c('', 'NA'))) %>%
    slice_max(day_month_year) %>%
    pull(day_month_year)
  
  # First day associated with a version release.
  frst_version_date <- downloads_data %>%
    filter(!(version %in% c('', 'NA'))) %>%
    slice_min(day_month_year) %>%
    pull(day_month_year)
  
  # Get the difference in months.
  month_last <- interval(last_version_date, latest_date) %/% months(1)
  month_frst <- interval(frst_version_date, latest_date) %/% months(1)
  
  browser()
  
  plot_ly(downloads_data,
          x = ~day_month_year,
          y = ~downloads,
          name = "# Downloads", type = 'scatter', 
          mode = 'lines+markers', line = list(color = "blue"),
          hoverinfo = "text",
          text = ~glue('No. of Downloads: {format(downloads, big.mark = ",")} <br> {month} {year}')) %>%
    layout(title = glue('Number of Downloads by Month: {selected_pkg$name()}'),
           showlegend = FALSE,
           yaxis = list(title = "Downloads"),
           xaxis = list(title = "", type = 'date', tickformat = "%b %Y")
    ) %>% 
    add_segments(
      x = ~if_else(version %in% c("", "NA"), NA_Date_, day_month_year),
      xend = ~if_else(version %in% c("","NA"), NA_Date_, day_month_year),
      y = ~.98 * min(downloads),
      yend = ~1.02 * max(downloads),
      name = "Version Release",
      hoverinfo = "text",
      text = ~glue('Version {version}'),
      line = list(color = "#FF0000")
    ) %>% 
      add_annotations(
        yref = 'paper',
        xref = "x",
        y = .50,
        x = downloads_data$day_month_year,
        xanchor = 'left',
        showarrow = F,
        textangle = 270,
        font = list(size = 14, color = '#FF0000'),
        text = ~ifelse(downloads_data$version %in% c("", "NA"), "", downloads_data$version)
      ) %>%
  layout(
    xaxis = list(
      # range is min-7 days, max+7 days
      # Dates need to be transformed to milliseconds since epoch
      range = c( (as.numeric(min(downloads_data$day_month_year))-7) *86400000,
                 (as.numeric(max(downloads_data$day_month_year))+7) *86400000   ),
      rangeselector = list(
        buttons = list(
          list(
            count = 6+1,
            label = "6 mo",
            step = "month",
            stepmode = "backward"),
          list(
            count = 12+1,
            label = "1 yr",
            step = "month",
            stepmode = "backward"),
          list(
            count = 24+1,
            label = "2 yr",
            step = "month",
            stepmode = "backward"),
          list(count = month_last+1,
               label = "Last Release",
               step = "month",
               stepmode = "backward"),
         list(count = month_frst+1,
              label = "First Release",
              step = "month",
              stepmode = "todate")
         )),
      rangeslider = list(type = "date", visible = FALSE), 
      start = as.numeric(min(downloads_data$day_month_year)),
      end = as.numeric(max(downloads_data$day_month_year)))
  )
})