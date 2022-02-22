# IntroJS.
introJSServer(id = "cum_introJS", text = cum_steps)


#' Creates reactive table with the time since first release, the time since
#' latest release, and the number of downloads since last year.
com_usage_metrics <- reactive({
  
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
  
  downloads_last_year <- community_usage_metrics() %>%
    filter(year == year(Sys.Date()) - 1) %>%
    distinct(year, month, downloads)
  
  tibble(id = c('time_since_first_version', 'time_since_latest_version', 'downloads_last_year'),
         title = c('First Version Release', 'Lastest Version Release', 'Package Downloads'),
         desc = c('Time passed since first version release', 'Time passed since latest version release', 'Number of downloads since last year'),
         value = c(glue('{time_diff_first_version}{first_version_label} Ago'), glue('{time_diff_latest_version}{latest_version_label} Ago'), format(sum(downloads_last_year$downloads), big.mark = ",")),
         succ_icon = c('black-tie', 'meteor', 'box-open'),
         icon_class = c("text-info", "text-info", "text-info"))
})

#' Creates three metricBoxes: the time since first release, the time since
#' latest release, and the number of downloads since last year.
observeEvent(com_usage_metrics(), {
  pmap(com_usage_metrics(), metricBoxServer)
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
  
  # Last day that appears on the community metrics.
  latest_date <- downloads_data %>%
    slice_max(day_month_year) %>%
    pull(day_month_year)
  
  # Last day associated with a version release.
  last_version_date <- downloads_data %>%
    filter(!(version %in% c('', 'NA'))) %>%
    slice_max(day_month_year) %>%
    pull(day_month_year)
  
  # Get the difference in months.
  month_diff <- interval(last_version_date, latest_date) %/% months(1)
  
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
           xaxis = list(title = "Month")
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
    #   add_annotations(
    #     yref = 'paper', 
    #     xref = "x", 
    #     y = .93, 
    #     x = ver_dat$day_month_year,
    #     xanchor = 'left',
    #     showarrow = F,
    #     textangle = 90,
    #     font = list(size = 14, color = '#000000'),
    #     text = ver_dat$version
    #   ) %>% 
  layout(
    xaxis = list(
      rangeselector = list(
        buttons = list(
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
          list(count = month_diff,
               label = "Last Release",
               step = "month",
               stepmode = "backward"))),
      rangeslider = list(type = "date"))
  )
})