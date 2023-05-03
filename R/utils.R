
#' showHelperMessage
#' 
#' Displays a helper message. By default, it informs the user that he should
#' select a package.
#' 
#' @param message a string
#' @keywords internal
#' 
showHelperMessage <- function(message = "Please select a package"){
  h6(message,
     style = 
       "text-align: center;
        color: gray;
        padding-top: 50px;")
}

check_for_package <- function(package, version, lib.loc, quiet = FALSE) {
  if (missing(package) || missing(version))
    stop("You must provide a package and version to check for in R package library.")

  if (missing(lib.loc))
    lib.loc <- .libPaths()[1L]

  pkg_check <- find.package(package = package, lib.loc = lib.loc, quiet = quiet)
  if (length(pkg_check) == 0) {
    if (!quiet) stop(glue::glue("The package {package} is not present in '{lib.loc}'"))
    return(FALSE)
  }

  ver_check <- as.character(packageVersion(pkg = package, lib.loc = lib.loc))
  if (ver_check != version) {
    if (!quiet) stop(glue::glue("Provided version does not match library version '{ver_check}'"))
    return(FALSE)
  }

  TRUE
}

install_package <- function(package, version, lib.loc) {
  if (missing(lib.loc))
    lib.loc <- .libPaths()[1L]

  if (missing(version) || is.null(version)) {
    utils::install.packages(pkgs = package, lib = lib.loc, dependencies = FALSE)
  } else {
    devtools::install_version(package = package, version = version, dependencies = FALSE, lib = lib.loc)
  }
}

#' Get the package general information from CRAN/local
#' 
#' @param pkg_name string name of the package
#' 
#' @import dplyr
#' @importFrom glue glue 
#' @importFrom rvest read_html html_node html_table html_text
#' @importFrom stringr str_remove_all
#' @keywords internal
#' 
get_latest_pkg_info <- function(pkg_name) {
  url <- glue::glue('https://cran.r-project.org/web/packages/{pkg_name}')
  tryCatch(
    expr = { 
      url = url(url, "rb")
      on.exit(close(url)) 
    },
    warning = function(w) {
      stop("HTTP status was '404 Not Found'")
    })
  webpage <- rvest::read_html(url)

  # Regex that finds entry: '\n ', "'", and '"' (the `|` mean 'or' and the 
  # `\`` is to scape the double quotes).
  pattern <- '\n |\'|\"|\\"'
  
  # Save div with class container to get the title and description.
  div_container <- webpage %>% rvest::html_nodes("div.container")
  
  # Read package title and clean it.
  title <- div_container %>% 
    rvest::html_nodes("h2") %>% 
    rvest::html_text() %>%
    stringr::str_remove_all(pattern = pattern)
  
  # Read package description and clean it.
  description <- div_container %>% 
    rvest::html_nodes("h2 + p") %>% 
    rvest::html_text() %>%
    stringr::str_remove_all(pattern = pattern)
  
  # Get the table displaying version, authors, etc.
  table_infx <- (webpage %>% rvest::html_table())[[1]] %>%
    dplyr::mutate(X1 = stringr::str_remove_all(string = X1, pattern = ':')) %>%
    dplyr::mutate(X2 = stringr::str_remove_all(string = X2, pattern = pattern)) %>% 
    dplyr::filter(X1 %in% c("Version", "Maintainer", "Author", "License", "Published"))
  
  table_infy <- t(table_infx$X2) %>% dplyr::as_tibble(.name_repair = "minimal")
  colnames(table_infy) <- t(table_infx$X1) %>% as.vector()

  table_info <- table_infy %>% 
    dplyr::select(Version, Maintainer, Author, License, Published) %>%
    dplyr::mutate(Title = title, Description = description)
  
  return(table_info)
}

#' Generate Community Usage Data
#' 
#' @description 
#' Extracts community usage metrics for a given package.
#' @returns A tibble of community usage metrics
#' @param pkg_name A string containing the name of a package.
#' 
#' @import dplyr
#' @importFrom cranlogs cran_downloads
#' @importFrom lubridate year month
#' @importFrom glue glue 
#' @importFrom rvest read_html html_node html_table html_text
#' @importFrom loggit loggit
#' @importFrom stringr str_remove_all
#' @return a data.frame which includes downloads per month for the given pkg
#' @examples 
#' if( interactive()) {
#' ggplot_comm_df <- generate_comm_data("ggplot2")
#' head(ggplot_comm_df)
#' }
#' @keywords reproduce
#' @export
generate_comm_data <- function(pkg_name){
  
  # initialize empty tibble
  pkgs_cum_metrics <- dplyr::tibble()
  
  # turn off summarise() .groups message
  options(dplyr.summarise.inform = FALSE)
  
  tryCatch(
    expr = {
      
      # get current release version number and date
      curr_release <- get_latest_pkg_info(pkg_name) %>%
        dplyr::select(`Last modified` = Published, version = Version)
      # Get the packages past versions and dates.
      pkg_url <- url(glue::glue('https://cran.r-project.org/src/contrib/Archive/{pkg_name}'))
      pkg_page <- try(rvest::read_html(pkg_url), silent = TRUE)
      
      
      # if past releases exist... they usually do!
      if(all(class(pkg_page) != "try-error")){ #exists("pkg_page")
        versions_with_dates0 <- pkg_page %>% 
          rvest::html_node('table') %>%
          rvest::html_table() %>%
          dplyr::select("Name", "Last modified") %>%
          dplyr::filter(`Last modified` != "") %>%
          dplyr::mutate(version = stringr::str_remove_all(
            string = Name, pattern = glue::glue('{pkg_name}_|.tar.gz')),
            .keep = 'unused') %>%
          # get latest high-level package info
          union(curr_release) 
      } else {
        versions_with_dates0 <- curr_release
      }
      # close(pkg_url)
      
      versions_with_dates <- versions_with_dates0 %>%
        dplyr::mutate(date = as.Date(`Last modified`), .keep = 'unused') %>%
        dplyr::mutate(month = lubridate::month(date)) %>%
        dplyr::mutate(year = lubridate::year(date))
      
      # First release date.
      first_release_date <- versions_with_dates %>%
        dplyr::pull(date) %>%
        min()
      
      # Summarize versions as a range when there was
      # more than one release in a month
      one_v_per_month <- versions_with_dates %>%
        dplyr::arrange(year, month, date) %>%
        dplyr::select(month, year, ea_v = version) %>%
        dplyr::group_by(year, month) %>%
        dplyr::mutate(version = dplyr::case_when(
                 n() > 1 ~ paste(ea_v[1], ea_v[n()], sep = " - "),
                 TRUE ~ as.character(ea_v))
        ) %>%
        select(-ea_v) %>%
        distinct(year, month, .keep_all = TRUE)
      
      
      # Get the number of downloads by month, year.
      pkgs_cum_metrics <- 
        cranlogs::cran_downloads(
          pkg_name,
          from = first_release_date,
          to = Sys.Date()) %>%
        dplyr::mutate(month = lubridate::month(date),
                      year = lubridate::year(date)) %>%
        dplyr::filter(!(month == lubridate::month(Sys.Date()) &
                          year == lubridate::year(Sys.Date()))) %>%
        group_by(id = package, month, year) %>%
        summarise(downloads = sum(count)) %>%
        ungroup() %>%
        left_join(one_v_per_month, by = c('month', 'year')) %>%
        dplyr::arrange(year, month) 
      
    },
    error = function(e) {
      loggit::loggit("ERROR", paste("Error extracting cum metric info of the package:",
                                    pkg_name, "info", e),
                     app = "fileupload-webscraping", echo = FALSE)
    }
  )
  
  return(pkgs_cum_metrics)
}

#' showComments
#' 
#' Displays formatted comments
#' 
#' @param pkg_name string name of the package
#' @param comments data.frame comments table entry
#' @return a formatted string of comments
#' @keywords internal
#' @export
showComments <- function(pkg_name, comments){
  if (length(pkg_name) == 0)
    return("")
  
  ifelse(
    length(comments$user_name) == 0, 
    "No comments",
    paste0(
      "<div class='well'>",
      icon("user-tie"), " ", "user: ", comments$user_name, ", ", 
      icon("user-shield"), " ", "role: ", comments$user_role, ", ",
      icon("calendar-days"), " ", "date: ", comments$added_on,
      br(), br(), 
      comments$comment,
      "</div>",
      collapse = ""
    )
  )
}


#' getTimeStamp
#'
#' Retrieves Sys.time(), but transforms slightly
#'
#' @importFrom stringr str_replace
#' @keywords internal
getTimeStamp <- function(){
  initial <- stringr::str_replace(Sys.time(), " ", "; ")
  return(paste(initial, Sys.timezone()))
}


#' The 'Get Date Span' function
#' 
#' Function accepts a start date and optional end date and will 
#' 
#' @param start starting date
#' @param end ending date
#' 
#' @importFrom lubridate interval years
#' @importFrom stringr str_remove
#' @keywords internal
get_date_span <- function(start, end = Sys.Date()) {
  # Get approximate difference between today and latest release.
  # time_diff_latest_version <- lubridate::year(Sys.Date()) - last_ver$year
  time_diff <- lubridate::interval(start, end)
  time_diff_val <- time_diff %/% months(1)
  time_diff_label <- 'Months'
  
  if(time_diff_val >= 12) {
    # Get difference in months.
    time_diff_val <- time_diff %/% lubridate::years(1)
    time_diff_label <- 'Years'
  }
  # remove "s" off of "Years" or "Months" if 1
  if(time_diff_val == 1)
    time_diff_label <- stringr::str_remove(
      string = time_diff_label, pattern = 's$')
  return(list(value = time_diff_val, label = time_diff_label))
}

#' The 'Build Community Cards' function
#' 
#' @param data a data.frame
#' 
#' @import dplyr
#' @importFrom lubridate interval make_date year
#' @importFrom glue glue
#' @keywords internal
#' 
build_comm_cards <- function(data){
  
  cards <- dplyr::tibble(
    name = character(),
    title = character(),
    desc = character(),
    value = character(),
    succ_icon = character(),
    icon_class = character(),
    is_perc = numeric(),
    is_url = numeric()
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
            succ_icon = 'meteor',
            icon_class = "text-info",
            is_perc = 0,
            is_url = 0)
  
  downloads_last_year <- data %>%
    dplyr::arrange(year, month) %>% # insurance
    dplyr::filter(row_number() >= (n() - 11)) %>%
    dplyr::distinct(year, month, downloads)
  
  cards <- cards %>%
    dplyr::add_row(name = 'downloads_last_year',
            title = 'Package Downloads',
            desc = 'Number of downloads in last 12 months',
            value = format(sum(downloads_last_year$downloads), big.mark = ","),
            succ_icon = 'box-open',
            icon_class = "text-info",
            is_perc = 0,
            is_url = 0)
  
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



#' Build a plotly of community usage metrics
#' @description 
#' Responsible for building an interactive `{plotly}` graphic containing the trend line for number of CRAN pkg downloads by month.
#' 
#' @param data a data.frame containing monthly download data, built using `generate_comm_data()`. This argument is optional, but if `NULL`, a `pkg_name` must be provided.
#' @param pkg_name a string of a package name. This parameter is optional. If `pkg_name` is provided, the data argument should be `NULL`.
#' @returns a plotly object
#' @section Example:
#' 
#' \preformatted{
#'   build_comm_plotly(pkg_name = "ggplot2")
#' }
#' 
#' @section Example Output: 
#' \if{html}{\figure{build_comm_plotly_ex2.png}{options: width=95\%}}
#' 
#' @import dplyr
#' @importFrom lubridate NA_Date_ interval
#' @importFrom glue glue
#' @importFrom plotly plot_ly layout add_segments add_annotations config
#' @return an interactive plotly object
#' @keywords reproduce
#' @export
build_comm_plotly <- function(data = NULL, pkg_name = NULL) {
  
  # If there is a package listed, in pkg_name, a plotly graphic will be created
  if (!is.null(pkg_name)){
    data <- generate_comm_data(pkg_name)
  } else {
    if (is.null(data)) {
      stop("must include either data or pkg_name argument")
    }
  }
  
  if (nrow(data) == 0) return(NULL)
  
  pkg_name <- unique(data$id)
  
  downloads_data <- data %>%
    dplyr::mutate(day_month_year = glue::glue('1-{month}-{year}')) %>%
    dplyr::mutate(day_month_year = as.Date(day_month_year, "%d-%m-%Y")) %>%
    dplyr::mutate(month_year = glue::glue('{months(day_month_year)} {year}')) %>%
    dplyr::mutate(month = month.name[month]) %>%
    dplyr::arrange(day_month_year)
  
  # Last day that appears on the community metrics.
  latest_date <- downloads_data %>%
    dplyr::slice_max(day_month_year) %>%
    dplyr::pull(day_month_year)
  
  # Last day associated with a version release.
  last_version_date <- downloads_data %>%
    dplyr::filter(!(version %in% c('', 'NA'))) %>%
    dplyr::slice_max(day_month_year) %>%
    dplyr::pull(day_month_year)
  
  # First day associated with a version release.
  first_version_date <- downloads_data %>%
    dplyr::filter(!(version %in% c('', 'NA'))) %>%
    dplyr::slice_min(day_month_year) %>%
    dplyr::pull(day_month_year)
  
  # Get the difference in months.
  month_last <- lubridate::interval(last_version_date, latest_date) %/% months(1)
  month_first <- lubridate::interval(first_version_date, latest_date) %/% months(1)
  
  # Set plot range: [min - 15 days, max + 15 days].
  # Dates need to be transformed to milliseconds since epoch.
  dates_range <- c(
    (as.numeric(min(downloads_data$day_month_year)) - 15) * 86400000,
    (as.numeric(max(downloads_data$day_month_year)) + 15) * 86400000)
  
  # set default at 2 years
  default_range <- c(
    max(downloads_data$day_month_year) - 45 - (365 * 2),
    max(downloads_data$day_month_year) + 15)
  
  plotly::plot_ly(downloads_data,
          x = ~day_month_year,
          y = ~downloads,
          name = "# Downloads", type = 'scatter', 
          mode = 'lines+markers', line = list(color = '#1F9BCF'),
          marker = list(color = '#1F9BCF'),
          hoverinfo = "text",
          text = ~glue::glue('No. of Downloads: {format(downloads, big.mark = ",")}
                         {month} {year}')) %>%
    plotly::layout(title = glue::glue('NUMBER OF DOWNLOADS BY MONTH: {pkg_name}'),
           margin = list(t = 100),
           showlegend = FALSE,
           yaxis = list(title = "Downloads"),
           xaxis = list(title = "", type = 'date', tickformat = "%b %Y",
                        range = dates_range)) %>% 
    plotly::add_segments(
      x = ~dplyr::if_else(version %in% c("", "NA", NA), lubridate::NA_Date_, day_month_year),
      xend = ~dplyr::if_else(version %in% c("", "NA", NA), lubridate::NA_Date_, day_month_year),
      y = ~.98 * min(downloads),
      yend = ~1.02 * max(downloads),
      name = "Version Release",
      hoverinfo = "text",
      text = ~glue::glue('Version {version}'),
      line = list(color = '#4BBF73')) %>% 
    plotly::add_annotations( 
      yref = 'paper',
      xref = "x",
      y = .50,
      x = downloads_data$day_month_year,
      xanchor = 'left',
      showarrow = F,
      textangle = 270,
      font = list(size = 14, color = '#4BBF73'),
      text = ~ifelse(downloads_data$version %in% c("", "NA", NA), "", downloads_data$version)) %>%
    plotly::layout(
      xaxis = list(
        range = dates_range,
        rangeselector = list(
          buttons = list(
            list(count = month_first + 1,
                 label = "First Release",
                 step = "month",
                 stepmode = "todate"),
            list(count = month_last + 1,
                 label = "Last Release",
                 step = "month",
                 stepmode = "backward"),
            list(
              count = 24 + 1,
              label = "2 yr",
              step = "month",
              stepmode = "backward"),
            list(
              count = 12 + 1,
              label = "1 yr",
              step = "month",
              stepmode = "backward"),
            list(
              count = 6 + 1,
              label = "6 mo",
              step = "month",
              stepmode = "backward")
          )),
        rangeslider = list(visible = TRUE)
      )) %>%
    plotly::config(displayModeBar = F)
}














