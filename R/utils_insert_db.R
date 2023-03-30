
#' dbUpdate
#'
#' Deletes, updates or inserts queries.
#'
#' @param command a string
#' @param db_name character name (and file path) of the database
#'
#' @import dplyr
#' @importFrom DBI dbConnect dbSendStatement dbClearResult dbDisconnect
#'   dbGetRowsAffected
#' @importFrom RSQLite SQLite
#' @importFrom loggit loggit
#' @importFrom glue glue
#' 
#' @returns nothing
#' @noRd
dbUpdate <- function(command, db_name = golem::get_golem_options('assessment_db_name')){
  errFlag <- FALSE
  con <- DBI::dbConnect(RSQLite::SQLite(), db_name)
  
  tryCatch({
    rs <- DBI::dbSendStatement(con, command)
  }, error = function(err) {
    message <- glue::glue("command: {command} resulted in {err}")
    message(message, .loggit = FALSE)
    loggit::loggit("ERROR", message, echo = FALSE)
    DBI::dbDisconnect(con)
    errFlag <<- TRUE
  },
  finally = {
    if (errFlag) return(invisible(NULL)) 
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
#' @param db_name character name (and file path) of the database
#' 
#' @importFrom loggit loggit
#' 
#' @returns nothing
#' @noRd
insert_pkg_info_to_db <- function(pkg_name, 
                                  db_name = golem::get_golem_options('assessment_db_name')) {
  tryCatch(
    expr = {
      # get latest high-level package info
      # pkg_name <- "dplyr" # testing
      if (!isTRUE(getOption("shiny.testmode")))
        pkg_info <- get_latest_pkg_info(pkg_name)
      else
        pkg_info <- test_pkg_info[[pkg_name]]
      
      # store it in the database
      upload_package_to_db(pkg_name, pkg_info$Version, pkg_info$Title,
                           pkg_info$Description, pkg_info$Author,
                           pkg_info$Maintainer, pkg_info$License,
                           pkg_info$Published, db_name)
      
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
            
            upload_package_to_db(pkg_name, ver, title, desc, auth, main, lis, pub, db_name)
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
#' @param db_name character name (and file path) of the database
#' 
#' @importFrom glue glue
#' @importFrom loggit loggit
#' 
#' @returns nothing
#' @noRd
upload_package_to_db <- function(name, version, title, description,
                                 authors, maintainers, license, published_on, db_name) {
  tryCatch(
    expr = {
      dbUpdate(glue::glue(
        "INSERT or REPLACE INTO package
        (name, version, title, description, maintainer, author,
        license, published_on, decision, decision_by, decision_date, date_added)
        VALUES('{name}', '{version}', '{title}', '{description}',
        '{maintainers}', '{authors}', '{license}', '{published_on}',
        '', '', '{as.Date(NA)}','{Sys.Date()}')"), db_name)
    },
    error = function(e) {
      loggit::loggit("ERROR", paste("Error in uploading the general info of the package", name, "info", e),
                     app = "fileupload-DB")
    }
  )
}


#' The 'Insert MM to DB ' Function
#'
#' Get the maintenance and testing metrics info and upload into DB.
#' 
#' @param pkg_name string name of the package
#' @param db_name character name (and file path) of the database
#' 
#' @import dplyr
#' @importFrom riskmetric pkg_ref pkg_assess pkg_score
#' @importFrom glue glue 
#' 
#' @returns nothing
#' @noRd
insert_riskmetric_to_db <- function(pkg_name, 
    db_name = golem::get_golem_options('assessment_db_name')){

  if (!isTRUE(getOption("shiny.testmode")))
    riskmetric_assess <-
      riskmetric::pkg_ref(pkg_name,
                          source = "pkg_cran_remote",
                          repos = c("https://cran.rstudio.com")) %>%
      dplyr::as_tibble() %>%
      riskmetric::pkg_assess()
  else
    riskmetric_assess <-
      test_pkg_assess[[pkg_name]]
  
  # Get the metrics weights to be used during pkg_score.
  metric_weights_df <- dbSelect("SELECT id, name, weight, is_perc FROM metric", db_name)
  metric_weights <- metric_weights_df$weight
  names(metric_weights) <- metric_weights_df$name
  
  riskmetric_score <-
    riskmetric_assess %>%
    riskmetric::pkg_score(weights = metric_weights)
  
  package_id <- dbSelect(glue::glue("SELECT id FROM package WHERE name = '{pkg_name}'"), db_name)
  
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
    exp_help <- function(pkg_name) {
      valu <- riskmetric::assess_export_help(riskmetric::pkg_ref(pkg_name)) %>% 
        riskmetric::metric_score()
      as.character(round(valu*100, 2))
    }

    metric_value <- ifelse(
      "pkg_metric_error" %in% class(riskmetric_assess[[metric$name]][[1]]), "pkg_metric_error",
      ifelse (metric$name == "export_help", exp_help(pkg_name),
      ifelse(metric$name == "dependencies", as.character(nrow(riskmetric_assess[[metric$name]][[1]])),
      ifelse(metric$name == "reverse_dependencies", as.character(length(as.vector(riskmetric_assess[[metric$name]][[1]]))),
      ifelse(metric$is_perc == 1L, as.character(round(riskmetric_score[[metric$name]]*100, 2)[[1]]),
      as.character(riskmetric_assess[[metric$name]][[1]][1:length(riskmetric_assess[[metric$name]])])
     )))))
   
    dbUpdate(glue::glue(
      "INSERT INTO package_metrics (package_id, metric_id, weight, value) 
      VALUES ({package_id}, {metric$id}, {metric$weight}, '{metric_value}')"), db_name
    )
  }
  
  dbUpdate(glue::glue(
    "UPDATE package
    SET score = '{format(round(riskmetric_score$pkg_score[1], 2))}'
    WHERE name = '{pkg_name}'"), db_name)
}


#' Generate community usage metrics and upload data into DB
#' 
#' @param pkg_name string name of the package
#' @param db_name character name (and file path) of the database
#' 
#' @import dplyr
#' @importFrom cranlogs cran_downloads
#' @importFrom lubridate year month
#' @importFrom glue glue 
#' @importFrom rvest read_html html_node html_table html_text
#' @importFrom loggit loggit
#' @importFrom stringr str_remove_all
#' 
#' @returns nothing
#' @noRd
insert_community_metrics_to_db <- function(pkg_name, 
                                           db_name = golem::get_golem_options('assessment_db_name')) {
  
  if (!isTRUE(getOption("shiny.testmode")))
    pkgs_cum_metrics <- generate_comm_data(pkg_name)
  else
    pkgs_cum_metrics <- test_pkg_cum[[pkg_name]]
  
  if(nrow(pkgs_cum_metrics) != 0){
    for (i in 1:nrow(pkgs_cum_metrics)) {
      dbUpdate(glue::glue(
        "INSERT INTO community_usage_metrics 
        (id, month, year, downloads, version)
        VALUES ('{pkg_name}', {pkgs_cum_metrics$month[i]},
        {pkgs_cum_metrics$year[i]}, {pkgs_cum_metrics$downloads[i]},
        '{pkgs_cum_metrics$version[i]}')"), db_name)
    }
  }
}


#' update_metric_weight
#' 
#' @param metric_name a metric name, as a string
#' @param metric_weight a weight, as a string or double
#' @param db_name character name (and file path) of the database
#' 
#' @importFrom glue glue
#' 
#' @returns nothing
#' @noRd
update_metric_weight <- function(metric_name, metric_weight, 
                                 db_name = golem::get_golem_options('assessment_db_name')){
  dbUpdate(glue::glue(
    "UPDATE metric
    SET weight = {metric_weight}
    WHERE name = '{metric_name}'"
  ), db_name)
}

#' db trash collection
#' 
#' clean up tables package_metrics, community_usage_metrics and comments
#' after one or more packages have been removed from the package table.
#' 
#' @param db_name character name (and file path) of the database 
#' 
#' @returns nothing
#' @noRd
db_trash_collection <- function(db_name = golem::get_golem_options('assessment_db_name')){
  
  dbUpdate("delete from package_metrics where package_id not in(select id from package)", db_name)
  dbUpdate("delete from community_usage_metrics where id not in(select name from package)", db_name)
  cmtbl <- dbSelect("select distinct id from comments", db_name)
  if (nrow(cmtbl) >0) {
    dbUpdate("delete from comments where id not in(select name from package)", db_name)
  }
}