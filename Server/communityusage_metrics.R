# IntroJS.
introJSServer(id = "cum_introJS", text = cum_steps)

#' Creates three metricBoxes: the time since first release, the time since
#' latest release, and the number of downloads since last year.
observeEvent(community_usage_metrics(), {
  
  # Get the first package release.
  first_version <- community_usage_metrics() |>
    filter(year == min(year) & month == min(month)) |>
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
  last_version <- community_usage_metrics() |>
    filter(year == max(year) & month == max(month)) |>
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
  
  downloads_last_year <- community_usage_metrics() |>
    filter(year == year(Sys.Date()) - 1) |>
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


# 3. Render Output to show the plot for number of downloads on the application.
# output$no_of_downloads <- 
#   plotly::renderPlotly({
#     num_dwnlds_plot(data = values$riskmetrics_cum,
#                     input_select_pack = selected_pkg$name())
#   })