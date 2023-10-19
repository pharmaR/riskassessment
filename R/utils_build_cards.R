#' Metric Gauge
#' 
#' An HTML meter element, used for displaying metric scores in the app
#' 
#' @param score a whole number between 0 and 100
#' @importFrom dplyr case_when
#' @return tagList object
#' @keywords internal
#'
metric_gauge <- function(score) { # could add id arg here
  
  tagList(
    div(style = "width: 78px; text-align:center;",
        div(
          tags$label(style = "font-size:32px", # `for` = id,
                     if(toupper(score) %in% c("NA", "NULL")) "NA" else {
                       # flip the label display of the score to mimic the package score...
                       HTML(case_when(
                         round(as.numeric(score), 2) == 0 ~ "1 <span style='color:#FF765B; font-family:FontAwesome; text-shadow:-1px 0 #777, 0 1px #777, 1px 0 #777, 0 -1px #777;'>&#10060;</span>",
                         round(as.numeric(score), 2) == 1 ~ "0 <span style='color:#9CFF94; font-family:FontAwesome; text-shadow:-1px 0 #777, 0 1px #777, 1px 0 #777, 0 -1px #777;'>&#10004;</span>",
                         TRUE ~ as.character(round(1 - as.numeric(score), 2))
                       ))
                     }
          )
        ),
        div(
          tags$meter( # id = id,
            min = 0,
            max = 1,
            optimum = 1,
            low = .3333,
            high = .6666,
            value = 
              if(toupper(score) %in% c("NA", "NULL")) 0 
            else ifelse(between(round(as.numeric(score), 2), 0, .08), .08, round(as.numeric(score), 2)),
            style = "height: 30px; width: 100%;"
          )
        )
    )
  ) 
}

#' The 'Build Community Cards' function
#' 
#' @param data a data.frame
#' 
#' @import dplyr
#' @importFrom lubridate interval make_date year
#' @importFrom glue glue
#' @importFrom stats lm
#' @keywords internal
#' 
build_comm_cards <- function(data){
  
  cards <- dplyr::tibble(
    name = character(),
    title = character(),
    desc = character(),
    value = character(),
    score = character(),
    succ_icon = character(),
    icon_class = character(),
    is_perc = numeric(),
    is_url = numeric(),
    type = "information"
  )
  
  if (nrow(data) == 0)
    return(cards)
  
  # Get the first package release.
  first_version <- data %>%
    dplyr::filter(year == min(year)) %>%
    dplyr::filter(month == min(month)) %>%
    dplyr::slice_head(n = 1) %>%
    dplyr::mutate(fake_rel_date = lubridate::make_date(year, month, 15))
  
  # get the time span in months or years depending on how much time
  # has elapsed
  time_diff_first_rel <- get_date_span(first_version$fake_rel_date)
  
  cards <- cards %>%
    dplyr::add_row(
      name = 'time_since_first_version',
      title = 'First Version Release',
      desc = 'Time passed since first version release',
      value = glue::glue('{time_diff_first_rel$value} {time_diff_first_rel$label} Ago'),
      score = "NULL",
      succ_icon = 'black-tie',
      icon_class = "text-info",
      is_perc = 0,
      is_url = 0
    )
  
  
  # Get the last package release's month and year, then
  # make add in the release date
  last_ver <- data %>%
    dplyr::filter(!(version %in% c('', 'NA'))) %>%
    dplyr::filter(year == max(year)) %>%
    dplyr::filter(month == max(month)) %>%
    dplyr::slice_head(n = 1) %>%
    dplyr::mutate(fake_rel_date = lubridate::make_date(year, month, 15))
  
  # get the time span in months or years depending on how much time
  # has elapsed
  time_diff_latest_rel <- get_date_span(last_ver$fake_rel_date)
  
  cards <- cards %>%
    dplyr::add_row(name = 'time_since_latest_version',
                   title = 'Latest Version Release',
                   desc = 'Time passed since latest version release',
                   value = glue::glue('{time_diff_latest_rel$value} {time_diff_latest_rel$label} Ago'),
                   score = "NULL",
                   succ_icon = 'meteor',
                   icon_class = "text-info",
                   is_perc = 0,
                   is_url = 0)
  
  
  # pull in some riskmetric data
  comm <- get_metric_data(data$id[1], metric_class = 'community', golem::get_golem_options('assessment_db_name'))
  
  
  # get downloads in the last year
  downloads_last_year <- data %>%
    dplyr::arrange(year, month) %>% # insurance
    dplyr::filter(row_number() >= (n() - 11)) %>%
    dplyr::distinct(year, month, downloads)
  
  
  # new
  comm_d1 <- comm %>% filter(name == "downloads_1yr")
  cards <- cards %>%
    dplyr::add_row(name = comm_d1[['name']],
                   title = comm_d1[['title']],
                   desc = comm_d1[['desc']],
                   value = format(as.numeric(comm_d1[['value']]), big.mark = ","),
                   # altneratively, last 12 months from plot (doesn't include current month)
                   # format(sum(downloads_last_year$downloads), big.mark = ","),
                   score = comm_d1[['score']],
                   succ_icon = comm_d1[['succ_icon']],
                   icon_class = comm_d1[['icon_class']],
                   is_perc = comm_d1[['is_perc']] == 1,
                   is_url = comm_d1[['is_url']] == 1)
  
  
  
  # get reverse dependency info
  rev_deps <- get_assess_blob(data$id[1])$reverse_dependencies[[1]]
  
  comm_rev <- comm %>% filter(name == "reverse_dependencies")
  # new
  cards <- cards %>%
    dplyr::add_row(name = comm_rev[['name']],
                   title = comm_rev[['title']],
                   desc = comm_rev[['desc']],
                   value = format(as.numeric(comm_rev[['value']]), big.mark = ","),
                   score = comm_rev[['score']],
                   succ_icon = comm_rev[['succ_icon']],
                   icon_class = comm_rev[['icon_class']],
                   is_perc = comm_rev[['is_perc']] == 1,
                   is_url = comm_rev[['is_url']] == 1)
  
  
  # Get Monthly download trend
  trend_downloads <- dplyr::as_tibble(data) %>% 
    dplyr::arrange(year, month) %>%
    dplyr::mutate(day_month_year = glue::glue('1-{month}-{year}')) %>%
    dplyr::mutate(day_month_year = as.Date(day_month_year, "%d-%m-%Y")) %>%
    dplyr::filter(
      day_month_year >= max(day_month_year) - lubridate::years(2) + lubridate::month(1)
    ) %>% 
    dplyr::mutate(row_n = row_number())
  
  amount_months <- max(trend_downloads$row_n)
  if (amount_months < 12) {
    return(cards)
  }
  
  model_result <- as.list(
    stats::lm(downloads ~ row_n, data = trend_downloads)$coefficients
  ) %>% 
    dplyr::as_tibble() %>% 
    dplyr::select(estimate = row_n) %>% 
    dplyr::mutate(estimate = round(estimate, 0)) %>% 
    dplyr::mutate(succ_icon = dplyr::case_when(
      estimate > 0 ~ "arrow-trend-up",
      estimate < 0 ~ "arrow-trend-down",
      TRUE ~ "bars"
    )
    ) %>%
    dplyr::mutate(type = dplyr::case_when(
      estimate > 0 ~ "information",
      estimate < 0 ~ "danger",
      TRUE ~ "information"
    )
    ) %>% 
    dplyr::select(estimate, succ_icon, type)
  
  cards <- cards %>%
    dplyr::add_row(
      name = 'downloads_trend',
      title = 'Monthly downloads trend',
      desc = glue::glue("Trend of downloads in last {amount_months} months"),
      value = format(model_result$estimate, big.mark = ","),
      score = "NULL",
      succ_icon = model_result$succ_icon,
      icon_class = "text-info",
      is_perc = 0,
      is_url = 0,
      type = model_result$type
    )
  
  cards
}


#' The 'Build Dependency Cards' function
#' 
#' @param data a data.frame
#' @param loaded a vector of package names loaded to db 
#' 
#' @import dplyr
#' @importFrom glue glue
#' @importFrom purrr map_df
#' @importFrom rlang is_empty
#' @keywords internal
#' 
build_dep_cards <- function(data, loaded, toggled){
  
  cards <- dplyr::tibble(
    name = character(),
    title = character(),
    desc = character(),
    value = character(),
    score = character(),
    succ_icon = character(),
    icon_class = character(),
    is_perc = numeric(),
    is_url = numeric()
  )
  
  deps <- data %>% 
    mutate(package = stringr::str_replace(package, "\n", "")) %>% 
    mutate(name = stringr::str_extract(package, "^((([[A-z]]|[.][._[A-z]])[._[A-z0-9]]*)|[.])")) %>% 
    mutate(base = if_else(name %in% c(rownames(installed.packages(priority = "base"))), "Base", "Tidyverse")) %>% 
    mutate(base = factor(base, levels = c("Base", "Tidyverse"), labels = c("Base", "Tidyverse"))) %>% 
    mutate(upld = if_else(name %in% loaded, 1, 0)) 
  
  if (toggled == 0L) {
    deps <- deps %>% 
      mutate(type = factor(type, levels = c("Imports", "Depends", "LinkingTo"), ordered = TRUE))  
  } else {
    deps <- deps %>% 
      mutate(type = factor(type, levels = c("Imports", "Depends", "LinkingTo", "Suggests"), ordered = TRUE)) 
  }
  
  upld_cat_rows <-
    deps %>%
    summarize(upld_cat_sum = sum(upld)) %>%
    mutate(upld_cat_pct  = 100 * (upld_cat_sum / nrow(deps))) %>% 
    mutate(upld_cat_disp = if_else(is.nan(upld_cat_pct),
                                   glue::glue('{upld_cat_sum} ( 0%)'),
                                   glue::glue('{upld_cat_sum} of {nrow(deps)} ({format(upld_cat_pct, digits = 1)}%)'))) %>% 
    pull(upld_cat_disp) 
  
  # Get the Number of uploaded dependencies in the db
  cards <- cards %>%
    dplyr::add_row(
      name = 'pkg_cnt',
      title = 'Dependencies Uploaded',
      desc = 'Number of dependencies uploaded',
      value = upld_cat_rows,
      score = "NULL",
      succ_icon = 'upload',
      icon_class = "text-info",
      is_perc = 0,
      is_url = 0
    )
  # base R replacement for tidyr::complete(type)
  x <- tibble("type" = levels(deps$type))
  y <- full_join(x, deps, by = "type") %>% 
    mutate(type = factor(type, ordered = TRUE))
  
  type_cat_rows <-
    y %>%
    mutate(cnt = ifelse(is.na(name), 0, 1)) %>%
    group_by(type) %>%
    summarize(type_cat_sum = sum(cnt)) %>%
    ungroup() %>%
    mutate(type_cat_pct  = 100 * (type_cat_sum / nrow(deps))) %>% 
    mutate(type_cat_disp = if_else(is.nan(type_cat_pct),
                                   glue::glue('{type}: {type_cat_sum} ( 0%)'),
                                   glue::glue('{type}: {type_cat_sum} ({format(type_cat_pct, digits = 1)}%)'))) %>% 
    arrange(type) %>%
    pull(type_cat_disp) %>%
    paste(., collapse = " \n")
  
  cards <- cards %>%
    dplyr::add_row(
      name = 'type_cat_count',
      title = 'Type Summary',
      desc = 'Package Dependencies by Type',
      value = type_cat_rows,
      score = "NULL",
      succ_icon = 'boxes-stacked',
      icon_class = "text-info",
      is_perc = 0,
      is_url = 0
    )
  
  x <- tibble("base" = levels(deps$base))
  y <- full_join(x, deps, by = "base")
  
  base_cat_rows <-
    y %>%
    mutate(cnt = ifelse(is.na(name), 0, 1)) %>%
    group_by(base) %>%
    summarize(base_cat_sum = sum(cnt)) %>%
    ungroup() %>%
    mutate(base_cat_pct = 100 * (base_cat_sum / nrow(deps))) %>% 
    mutate(base_cat_disp = if_else(is.nan(base_cat_pct),
                                   glue::glue('{base_cat_sum} ( 0%)       '),
                                   glue::glue('{base_cat_sum} ({format(base_cat_pct, digits = 1)}%)'))) %>% 
    filter(base == "Base") %>% 
    pull(base_cat_disp) %>%
    paste(., collapse = "\n")
  
  cards <- cards %>%
    dplyr::add_row(
      name = 'base_cat_count',
      title = 'Base R Summary',
      desc = 'Percent of Packages from Base R',
      value = base_cat_rows,
      score = "NULL",
      succ_icon = 'boxes-stacked',
      icon_class = "text-info",
      is_perc = 0,
      is_url = 0
    )
  
  cards
}


#' The 'Build Database Cards' function
#' 
#' @param data a data.frame
#' 
#' @import dplyr
#' @importFrom glue glue
#' @keywords internal
#' 
build_db_cards <- function(data){
  
  cards <- dplyr::tibble(
    name = character(),
    title = character(),
    desc = character(),
    value = character(),
    score = character(),
    succ_icon = character(),
    icon_class = character(),
    is_perc = numeric(),
    is_url = numeric()
  )
  
  if (nrow(data) == 0)
    return(cards)
  
  # Get the Number of packages in the db
  cards <- cards %>%
    dplyr::add_row(
      name = 'pkg_cnt',
      title = 'Package Count',
      desc = 'Number of Packages Uploaded to DB',
      value = paste(nrow(data)),
      score = "NULL",
      succ_icon = 'upload',
      icon_class = "text-info",
      is_perc = 0,
      is_url = 0
    )
  
  # Get the Count (and %) of pkgs with a decision made
  decision_cnt <-
    data %>%
    mutate(decision = as.character(decision)) %>%
    filter(decision != "-") %>%
    nrow
  # decision_cnt <- length(data$decision[data$decision != "-"])
  decision_pct <- format(100 * (decision_cnt / nrow(data)), digits = 1)
  
  cards <- cards %>%
    dplyr::add_row(
      name = 'decision_cnt',
      title = 'Decision Count',
      desc = 'Packages with Decisions Made',
      value = glue::glue('{decision_cnt} ({decision_pct}%)'),
      score = "NULL",
      succ_icon = 'gavel',
      icon_class = "text-info",
      is_perc = 0,
      is_url = 0
    )
  
  # Get the Count (and %) of pkgs by Decision
  # # Dummy test data set
  # data <- data.frame(
  #   package = c("tidyCDISC", "rhino", "MCPMod"),
  #   decision = c("-","-","-")
  # ) %>%
  # mutate(decision = factor(decision, levels = c("Low Risk", "Medium Risk", "High Risk")))
  
  decision_cat_rows <-
    data %>%
    filter(decision != "-") %>%
    group_by(decision) %>%
    summarize(decision_cat_sum = n()) %>%
    ungroup() %>%
    mutate(decision_cat_pct = 100 * (decision_cat_sum / nrow(data)),
           decision_cat_disp = glue::glue('{decision}: {decision_cat_sum} ({format(decision_cat_pct, digits = 1)}%)')) %>%
    arrange(decision) %>%
    pull(decision_cat_disp) %>%
    paste(., collapse = "\n")
  
  
  cards <- cards %>%
    dplyr::add_row(
      name = 'decision_cat_count',
      title = 'Decision Summary',
      desc = 'Package Counts by Decision Type',
      value = ifelse(decision_cat_rows == "", "No Decisions Made", decision_cat_rows),
      score = "NULL",
      succ_icon = 'boxes-stacked',
      icon_class = "text-info",
      is_perc = 0,
      is_url = 0
    )
  
  cards
}



#' Automatic font re-sizer
#'
#' A function that adjusts the number (to be used as font size) that is
#' proportional to the length of a text string. So the longer the text string,
#' the smaller the font. Used in MetricBox.R.
#'
#' @param txt a string
#' @param txt_max an integer to specify a length of text that is considered "to
#'   long" to continue to toggle the font size
#' @param size_min an integer specifying the smallest font size you'd like to
#'   see in the output
#' @param size_max integer specifying the largest font size you'd like to see in
#'   the output
#' @param num_bins when not NULL (the default), accepts an integer that bins a
#'   continuous font size into a categorical one.
#'  
#' @keywords internal
#' 
auto_font <- function(txt, txt_max = 45, size_min = .75, size_max = 1.5,
                      num_bins = NULL){
  txt_len <- nchar(txt)
  txt_pct <- 1- ifelse(txt_len >= txt_max, 1, txt_len / txt_max)
  cont_size <- round(size_min + (txt_pct * (size_max - size_min)), 3)
  if (is.null(num_bins)) {
    return(cont_size)
  } else {
    # when creating bins, we want equally sized categories and to choose the
    # left bound if cont_size falls in the lowest category; otherwise,
    # re-calculate the breaks to be more proportional and choose the upper bound
    num_bins0 <- ifelse(num_bins < 2, 2, num_bins)
    breaks <- seq(size_min, size_max, length.out = num_bins0 + 1)
    grp <- as.character(cut(cont_size, breaks, include.lowest = TRUE))
    
    breaks2 <- seq(size_min, size_max, length.out = num_bins0)
    return(ifelse(substr(grp, 1, 1) == "[",
                  size_min, 
                  breaks2[cut(cont_size, breaks, include.lowest = TRUE, labels = FALSE)])
    )
  }
}
