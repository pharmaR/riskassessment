
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

#' @import dplyr
#' @importFrom desc desc_get_field
#' @importFrom glue glue
#' @importFrom purrr map set_names
#' @importFrom utils untar
#' 
#' @noRd
get_desc_pkg_info <- function(pkg_name, pkg_version, tar_dir = "tarballs") {
  tar_file <- file.path(tar_dir, glue::glue("{pkg_name}_{pkg_version}.tar.gz"))
  if (!file.exists(tar_file))
    return(get_latest_pkg_info(pkg_name))
  
  utils::untar(tar_file, exdir = "source")
  
  desc_file <- glue::glue("source/{pkg_name}/DESCRIPTION")
  
  keys <- c("Package", "Version", "Maintainer", "Author", "License", "Packaged", "Title", "Description")
  purrr::map(keys,
             desc::desc_get_field, file = desc_file) %>%
    purrr::set_names(keys) %>%
    dplyr::as_tibble() %>%
    dplyr::rename("Published"="Packaged")
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
          to = get_Date()) %>%
        dplyr::mutate(month = lubridate::month(date),
                      year = lubridate::year(date)) %>%
        dplyr::filter(!(month == lubridate::month(get_Date()) &
                          year == lubridate::year(get_Date()))) %>%
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
#' @param none_txt if there is no text to show in `comments` object, then
#'   display this text string. Default: "No comments"
#' @return a formatted string of comments
#' @keywords internal
#' @export
showComments <- function(pkg_name, comments, none_txt = "No comments"){
  if (length(pkg_name) == 0)
    return("")
  
  ifelse(
    length(comments$user_name) == 0, 
    none_txt,
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
  initial <- stringr::str_replace(get_time(), " ", "; ")
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
get_date_span <- function(start, end = get_Date()) {
  # Get approximate difference between today and latest release.
  # time_diff_latest_version <- lubridate::year(get_Date()) - last_ver$year
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
#' @importFrom stats lm predict
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
  
  # plot
  plot <- plotly::plot_ly(
    downloads_data,
    x = ~day_month_year,
    y = ~downloads,
    name = "# Downloads", type = 'scatter', 
    mode = 'lines+markers', 
    line = list(color = '#1F9BCF'),
    marker = list(color = '#1F9BCF'),
    hoverinfo = "text",
    text = ~glue::glue(
      'No. of Downloads: {format(downloads, big.mark = ",")}
      {month} {year}'
      )
    ) %>%
    plotly::layout(
      title = glue::glue('NUMBER OF DOWNLOADS BY MONTH: {pkg_name}'),
      margin = list(t = 100),
      showlegend = TRUE,
      yaxis = list(title = "Downloads"),
      xaxis = list(
        title = "", type = 'date', tickformat = "%b %Y",
        range = dates_range
      )
    ) %>% 
    plotly::add_segments(
      x = ~dplyr::if_else(version %in% c("", "NA", NA), lubridate::NA_Date_, day_month_year),
      xend = ~dplyr::if_else(version %in% c("", "NA", NA), lubridate::NA_Date_, day_month_year),
      y = ~.98 * min(downloads),
      yend = ~1.02 * max(downloads),
      name = "Version Release",
      hoverinfo = "text",
      text = ~glue::glue('Version {version}'),
      line = list(color = '#4BBF73')
    ) %>% 
    plotly::add_annotations( 
      yref = 'paper',
      xref = "x",
      y = .50,
      x = downloads_data$day_month_year,
      xanchor = 'left',
      showarrow = F,
      textangle = 270,
      font = list(size = 14, color = '#4BBF73'),
      text = ~ifelse(downloads_data$version %in% c("", "NA", NA), "", downloads_data$version)
    ) %>%
    plotly::layout(
      legend = list(
        title = list(text = 'Legend'),
        orientation = "h",
        y = -0.5
      ),
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
          )
        ),
        rangeslider = list(visible = TRUE)
      )
    ) %>%
    plotly::config(displayModeBar = FALSE)
  
  #Create linear model
  data_horizon <- downloads_data %>% 
    dplyr::arrange(day_month_year) %>%
    dplyr::filter(
      day_month_year >= max(day_month_year) - lubridate::years(2) + lubridate::month(1)
    ) %>% 
    dplyr::mutate(row_n = row_number())
  
  if (max(data_horizon$row_n) < 12) {
   return(plot) 
  } else {
    lm_model <- stats::lm(downloads ~ row_n, data = data_horizon)
    
    downloads_trend <- data_horizon %>% 
      dplyr::mutate(trend = stats::predict(lm_model, data_horizon))
    
    plot %>% 
      plotly::add_trace(
        data = downloads_trend,
        x = ~day_month_year,
        y = ~trend,
        inherit = FALSE,
        name = 'Linear trend',
        type = 'scatter',
        mode = "marker",
        opacity = 0.45,
        line = list(color = '#db5502')
      )
  }
  
}

#' @keywords internal
#' @noRd
get_Date <- function() {
  if (isTRUE(getOption("shiny.testmode")))
    as.Date("2023-07-20")
  else
    Sys.Date()
}

#' @keywords internal
#' @noRd
get_time <- function() {
  if (isTRUE(getOption("shiny.testmode")))
    as.POSIXct("2023-07-20 08:00:00 EDT")
  else
    Sys.time()
}

#' Remove shiny inputs
#'
#' Removes shiny inputs associated with a specific `id`. Useful when removing
#' inputs from a dynamic module.
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'   UI function
#' @param .input The shiny input object from the environment the module was
#'   called inside of
#' @param ns The namespace of the module
#'
#' @noRd
remove_shiny_inputs <- function(id, .input, ns = NS(NULL)) {
  invisible(
    lapply(grep(id, names(.input), value = TRUE), function(i) {
      .subset2(.input, "impl")$.values$remove(ns(i))
    })
  )
}

#' If NULL
#'
#' Function to substitute left-hand side with right-hand side if NULL
#' 
#' @noRd
`%||%` <- function(lhs, rhs) if (is.null(lhs)) rhs else lhs
