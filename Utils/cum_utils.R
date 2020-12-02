#################################################################################
# cum_utils.R - UI and Server utility functions for the community usage metric
# portion of the app
# Author: Aaron Clark
# Date: Dec 2nd, 2020
# License: MIT License
#################################################################################


# turn a date into a 'monthnumber' relative to an origin
monnb <- function(d) { 
  lt <- as.POSIXlt(as.Date(d, origin="1900-01-01"))
  lt$year*12 + lt$mon
} 

# compute a month difference as a difference between two monnb's
mondf <- function(d1, d2) { monnb(d2) - monnb(d1) }

num_dwnlds_plot <- function(data = values$riskmetrics_cum, 
                            input_select_pack
                            ){
  
  swap <- data.frame(month_num = paste(1:12), month_name = month.name)
  plot_dat <- data %>%
    mutate(month_name = stringr::word(month),
           year = stringr::word(month, -1)) %>%
    left_join(swap, by = "month_name") %>%
    mutate(month_date = as.Date(
      paste("01", month_num, year, sep = "-"), "%d-%m-%Y")
    )
  
  # how many months since the last release? + 1
  curr_mth <- plot_dat$month_date[nrow(plot_dat)]
  rel_mths <- plot_dat$month_date[!(plot_dat$ver_release %in% c("",'NA'))]
  lst_rel_mth <- rel_mths[length(rel_mths)]
  mnths2_lst_rel <- mondf(lst_rel_mth, curr_mth) + ifelse(lst_rel_mth == curr_mth, 2, 1)
  
  
  fig <- plot_ly(plot_dat, x = ~month_date, y = ~no_of_downloads,
                 name = "# Downloads", type = 'scatter', 
                 mode = 'lines+markers', line = list(color = "blue"),
                 hoverinfo = "text",
                 text = ~paste0('# Dwnlds: ', formatC(no_of_downloads, format="f", big.mark=",", digits=0),
                                '<br>', month)) %>%
    layout(title = ~paste(ifelse(!(data$no_of_downloads_last_year[1] %in% c(0, NA_integer_)),
                                 "Number of Downloads by Month:", "Zero Downloads for"),
                          input_select_pack),
           showlegend = FALSE,
           yaxis = list(title = "Downloads"),
           xaxis = list(title = "Month"),
           margin = list(t = 80)
    ) %>%
    plotly::config(displaylogo = FALSE, 
                   modeBarButtonsToRemove= c('sendDataToCloud', 'hoverCompareCartesian','hoverClosestCartesian','autoScale2d'
                                             ,'select2d', 'lasso2d', 'toggleSpikelines'
                                             # , 'toImage', 'resetScale2d', 'zoomIn2d', 'zoomOut2d','zoom2d', 'pan2d'
                   ))
  # any versions?
  ver_dat <- plot_dat %>% filter(!(ver_release %in% c("","NA")))
  any_ver_rel <- nrow(ver_dat) > 0
  # any_ver_rel <- any(!(plot_dat$ver_release %in% c("","NA")))
  if(any_ver_rel){
    fig <- fig %>% 
      add_segments(
        x = ~if_else(!(ver_release %in% c("","NA")), month_date, NA_Date_),
        xend = ~if_else(!(ver_release %in% c("","NA")), month_date, NA_Date_),
        y = ~.98*min(no_of_downloads),
        yend = ~1.02*max(no_of_downloads),
        name = "Version Release",
        hoverinfo = "text",
        text = ~paste0('Version ', ver_release, '<br>', month),
        line = list(color = "#FF0000")
      ) %>% 
      add_annotations(
        yref = 'paper', 
        xref = "x", 
        y = .93, 
        x = ver_dat$month_date,
        xanchor = 'left',
        showarrow = F,
        textangle = 90,
        font = list(size = 14, color = '#000000'),
        text = ver_dat$ver_release
      )
    fig <- fig %>% layout(
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
            list(step = "all", label = "All"),
            list(count = mnths2_lst_rel,
                 label = "Last Release",
                 step = "month",
                 stepmode = "backward"))),
        
        rangeslider = list(type = "date"))
    )
  } else { # no 'Last Release' option
    fig <- fig %>% layout(
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
            list(step = "all", label = "All"))),
        
        rangeslider = list(type = "date"))
    )
  }
  
  # reveal plot
  fig
}
# test output
num_dwnlds_plot(data = values$riskmetrics_cum, input_select_pack = "dplyr")
