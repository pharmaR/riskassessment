
#' dbUpdate
#'
#' Deletes, updates or inserts queries.
#'
#' @param command a string
#' @param db_name a string
#'
#' @import dplyr
#' @importFrom DBI dbConnect dbSendStatement dbClearResult dbDisconnect
#'   dbGetRowsAffected
#' @importFrom RSQLite SQLite
#' @importFrom loggit loggit
#' @importFrom glue glue
#' 
#' @returns nothing
#' @keywords internal
#' @export
dbUpdate <- function(command, db_name = golem::get_golem_options('assessment_db_name')){
  con <- DBI::dbConnect(RSQLite::SQLite(), db_name)
  
  tryCatch({
    rs <- DBI::dbSendStatement(con, command)
  }, error = function(err) {
    message <- glue::glue("command: {command} resulted in {err}")
    message(message, .loggit = FALSE)
    loggit::loggit("ERROR", message, echo = FALSE)
    DBI::dbDisconnect(con)
  })
  
  nr <- DBI::dbGetRowsAffected(rs)
  DBI::dbClearResult(rs)
  
  if (nr == 0) {
    message <- glue::glue("zero rows were affected by the command: {command}")
    message(message, .loggit = FALSE)
  }
  DBI::dbDisconnect(con)
}

#' Call function to get and upload info from CRAN/local to db.
#' 
#' @param pkg_name string name of the package
#' 
#' @importFrom loggit loggit
#' 
#' @returns nothing
#' @noRd
insert_pkg_info_to_db <- function(pkg_name) {
  tryCatch(
    expr = {
      # get latest high-level package info
      # pkg_name <- "dplyr" # testing
      pkg_info <- get_latest_pkg_info(pkg_name)
      
      # store it in the database
      upload_package_to_db(pkg_name, pkg_info$Version, pkg_info$Title,
                           pkg_info$Description, pkg_info$Author,
                           pkg_info$Maintainer, pkg_info$License,
                           pkg_info$Published)
      
    },
    error = function(e) {
      if (pkg_name %in% rownames(installed.packages()) == TRUE) {
        for (i in .libPaths()) {
          if(file.exists(file.path(i, pkg_name)) == TRUE) {
            i <- file.path(i, pkg_name)
            d <- description$new(i)
            title <- d$get("Title")
            ver <- d$get("Version")
            desc <- d$get("Description")
            main <- d$get("Maintainer")
            auth <- d$get("Author")
            lis <- d$get("License")
            pub <- d$get("Packaged")
            
            upload_package_to_db(pkg_name, ver, title, desc, auth, main, lis, pub)
          }}
      } else{
        loggit::loggit("ERROR", paste("Error in extracting general info of the package",
                                      pkg_name, "info", e), app = "fileupload-webscraping")
      }
    }
  )
}


#' Upload the general info into DB.
#' @param name string the package name
#' @param version string package version
#' @param title string title of the package
#' @param description string description of the package
#' @param authors string author name(s)
#' @param maintainers string names of maintainers
#' @param license string type of package license
#' @param published_on string char date of publication
#' 
#' @importFrom glue glue
#' @importFrom loggit loggit
#' 
#' @returns nothing
#' @noRd
upload_package_to_db <- function(name, version, title, description,
                                 authors, maintainers, license, published_on) {
  tryCatch(
    expr = {
      dbUpdate(glue::glue(
        "INSERT or REPLACE INTO package
        (name, version, title, description, maintainer, author,
        license, published_on, decision, date_added)
        VALUES('{name}', '{version}', '{title}', '{description}',
        '{maintainers}', '{authors}', '{license}', '{published_on}',
        '', '{Sys.Date()}')"))
    },
    error = function(e) {
      loggit::loggit("ERROR", paste("Error in uploading the general info of the package", name, "info", e),
                     app = "fileupload-DB")
    }
  )
}


#' The 'Insert MM to DB' Function
#'
#' Get the maintenance and testing metrics info and upload into DB.
#' 
#' @param pkg_name string name of the package
#' 
#' @import dplyr
#' @importFrom riskmetric pkg_ref pkg_assess pkg_score
#' @importFrom glue glue 
#' 
#' @returns nothing
#' @noRd
insert_maintenance_metrics_to_db <- function(pkg_name){
  
  riskmetric_assess <-
    riskmetric::pkg_ref(pkg_name) %>%
    dplyr::as_tibble() %>%
    riskmetric::pkg_assess()
  
  # Get the metrics weights to be used during pkg_score.
  metric_weights_df <- dbSelect("SELECT id, name, weight FROM metric")
  metric_weights <- metric_weights_df$weight
  names(metric_weights) <- metric_weights_df$name
  
  riskmetric_score <-
    riskmetric_assess %>%
    riskmetric::pkg_score(weights = metric_weights)
  
  package_id <- dbSelect(glue::glue("SELECT id FROM package WHERE name = '{pkg_name}'"))
  
  # Leave method if package not found.
  if(nrow(package_id) == 0){
    print("PACKAGE NOT FOUND.")
    loggit::loggit("WARN", paste("Package", pkg_name, "not found."))
    return()
  }
  
  # Insert all the metrics (columns of class "pkg_score") into the db.
  # TODO: Are pkg_score and pkg_metric_error mutually exclusive?
  for(row in 1:nrow(metric_weights_df)){
    metric <- metric_weights_df %>% dplyr::slice(row)
    # If the metric is not part of the assessment, then skip iteration.
    if(!(metric$name %in% colnames(riskmetric_score))) next
    
    # If the metric errors out,
    #   then save "pkg_metric_error" as the value of the metric.
    # If the metric has NA or 0,
    #   then save such value as the metric value.
    # Otherwise, save all the possible values of the metric
    #   (note: has_website for instance may have multiple values).
    metric_value <- ifelse(
      "pkg_metric_error" %in% class(riskmetric_assess[[metric$name]][[1]]),
      "pkg_metric_error",
      # Since the actual value of these metrics appear on riskmetric_score
      #   and not on riskmetric_assess, they need to be treated differently.
      # TODO: this code is not clean, fix it. Changes to riskmetric?
      ifelse(metric$name %in% c('bugs_status', 'export_help'),
             round(riskmetric_score[[metric$name]]*100, 2),
             riskmetric_assess[[metric$name]][[1]][1:length(riskmetric_assess[[metric$name]])]))
    
    dbUpdate(glue::glue(
      "INSERT INTO package_metrics (package_id, metric_id, weight, value) 
      VALUES ({package_id}, {metric$id}, {metric$weight}, '{metric_value}')")
    )
  }
  
  dbUpdate(glue::glue(
    "UPDATE package
    SET score = '{format(round(riskmetric_score$pkg_score[1], 2))}'
    WHERE name = '{pkg_name}'"))
}


#' Generate community usage metrics and upload data into DB
#' 
#' @param pkg_name string name of the package
#' 
#' @import dplyr
#' @importFrom cranlogs cran_downloads
#' @importFrom lubridate year month
#' @importFrom glue glue 
#' @importFrom rvest read_html html_node html_table html_text
#' @importFrom loggit loggit
#' @importFrom stringr str_remove_all
#' @importFrom tidyr tibble
#' 
#' @returns nothing
#' @noRd
insert_community_metrics_to_db <- function(pkg_name) {
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
  
  if(nrow(pkgs_cum_metrics) != 0){
    for (i in 1:nrow(pkgs_cum_metrics)) {
      dbUpdate(glue::glue(
        "INSERT INTO community_usage_metrics 
        (id, month, year, downloads, version)
        VALUES ('{pkg_name}', {pkgs_cum_metrics$month[i]},
        {pkgs_cum_metrics$year[i]}, {pkgs_cum_metrics$downloads[i]},
        '{pkgs_cum_metrics$version[i]}')"))
    }
  }
}


#' update_metric_weight
#' 
#' @param metric_name a metric name, as a string
#' @param metric_weight a weight, as a string or double
#' 
#' @importFrom glue glue
#' 
#' @returns nothing
#' @noRd
update_metric_weight <- function(metric_name, metric_weight){
  dbUpdate(glue::glue(
    "UPDATE metric
    SET weight = {metric_weight}
    WHERE name = '{metric_name}'"
  ))
}