#' generate_comm_data()
#' 
#' @description 
#' generate_comm_data() is a function that extracts usage metrics for a given package.
#' @returns A tibble of package usage metrics
#' @param pkg_name A string name of the package.
#' 
#' @import dplyr
#' @importFrom cranlogs cran_downloads
#' @importFrom lubridate year month
#' @importFrom glue glue 
#' @importFrom rvest read_html html_node html_table html_text
#' @importFrom loggit loggit
#' @importFrom stringr str_remove_all
#' @importFrom tidyr tibble
#' @examples 
#' metricDF <- generate_comm_data("ggplot2")
#' metricDF <- generate_comm_data("dplyr")
#' @export
generate_comm_data <- function(pkg_name){
  pkgs_cum_metrics <- tidyr::tibble()
  
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
        group_by(month, year) %>%
        summarise(downloads = sum(count)) %>%
        ungroup() %>%
        left_join(versions_with_dates, by = c('month', 'year')) %>%
        dplyr::arrange(year, month) %>%
        dplyr::select(-date)
      
    },
    error = function(e) {
      loggit::loggit("ERROR", paste("Error extracting cum metric info of the package:",
                                    pkg_name, "info", e),
                     app = "fileupload-webscraping", echo = FALSE)
    }
  )
  
  return(pkgs_cum_metrics)
}