
#' dbUpdate
#'
#' Deletes, updates or inserts queries.
#'
#' @param command a string
#' @param db_name character name (and file path) of the database
#' @param .envir Environment to evaluate each expression in
#' @param params A list of bindings, named or unnamed. Default is `NULL`, if present parameters will be passed to `DBI::dbBind()`
#'
#' @import dplyr
#' @importFrom DBI dbConnect dbSendStatement dbClearResult dbDisconnect
#'   dbGetRowsAffected
#' @importFrom RSQLite SQLite
#' @importFrom loggit loggit
#' @importFrom glue glue glue_sql
#' 
#' @returns nothing
#' @noRd
dbUpdate <- function(command, db_name = golem::get_golem_options('assessment_db_name'), .envir = parent.frame(), params = NULL){
  errFlag <- FALSE
  con <- DBI::dbConnect(RSQLite::SQLite(), db_name)
  
  tryCatch({
    rs <- DBI::dbSendStatement(con, glue::glue_sql(command, .envir = .envir, .con = con))
    if (!is.null(params))
      DBI::dbBind(rs, params)
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
insert_pkg_info_to_db <- function(pkg_name, pkg_version,
                                  db_name = golem::get_golem_options('assessment_db_name')) {
  tryCatch(
    expr = {
      # get latest high-level package info
      # pkg_name <- "dplyr" # testing
      if (isTRUE(getOption("shiny.testmode")))
        pkg_info <- test_pkg_info[[pkg_name]]
      else if (identical(Sys.getenv("TESTTHAT"), "true"))
        pkg_info <- get_latest_pkg_info(pkg_name)
      else
        pkg_info <- get_desc_pkg_info(pkg_name, pkg_version)
      
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
      dbUpdate(
        "INSERT or REPLACE INTO package
        (name, version, title, description, maintainer, author,
        license, published_on, decision_by, decision_date, date_added)
        VALUES({name}, {version}, {title}, {description},
        {maintainers}, {authors}, {license}, {published_on},
        '', {as.Date(NA)},{get_Date()})", db_name)
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
#' @importFrom desc description
#' @importFrom tools package_dependencies
#' 
#' @returns nothing
#' @noRd
insert_riskmetric_to_db <- function(pkg_name, pkg_version = "",
    db_name = golem::get_golem_options('assessment_db_name')){

  if (!isTRUE(getOption("shiny.testmode"))) {
    riskmetric_assess <-
      riskmetric::pkg_ref(pkg_name,
                          source = "pkg_cran_remote") %>%
      dplyr::as_tibble() %>%
      riskmetric::pkg_assess()
  } else {
    riskmetric_assess <-
      test_pkg_assess[[pkg_name]]
  }
  metric_weights_df <- dbSelect("SELECT id, name, weight, is_perc FROM metric", db_name)

  
  # Get the metrics weights to be used during pkg_score.
  metric_weights <- metric_weights_df$weight
  names(metric_weights) <- metric_weights_df$name
  
  riskmetric_score <-
    riskmetric_assess %>%
    riskmetric::pkg_score(weights = metric_weights)
  
  package_id <- dbSelect("SELECT id FROM package WHERE name = {pkg_name}", db_name)
  
  # Leave method if package not found.
  if(nrow(package_id) == 0){
    print("PACKAGE NOT FOUND.")
    loggit::loggit("WARN", paste("Package", pkg_name, "not found."))
    return()
  }

  assessment_serialized <- data.frame(pkg_assess = I(lapply(riskmetric_assess, serialize, connection = NULL)))
  
  # Insert all the metrics (columns of class "pkg_score") into the db.
  # TODO: Are pkg_score and pkg_metric_error mutually exclusive?
  for(row in 1:nrow(metric_weights_df)) {
    metric <- metric_weights_df %>% dplyr::slice(row)
    # If the metric is not part of the assessment, then skip iteration.
    if(!(metric$name %in% colnames(riskmetric_score))) next
    
    # If the metric errors out,
    #   then save "pkg_metric_error" as the value of the metric.
    # If the metric has NA or 0,
    #   then save such value as the metric value.
    # Otherwise, save all the possible values of the metric
    #   (note: has_website for instance may have multiple values).

    metric_value <- case_when(
      "pkg_metric_error" %in% class(riskmetric_assess[[metric$name]][[1]]) ~ "pkg_metric_error",
      metric$name == "dependencies" ~ as.character(length(unlist(as.vector(riskmetric_assess[[metric$name]][[1]][1])))),
      metric$name == "reverse_dependencies" ~ as.character(length(as.vector(riskmetric_assess[[metric$name]][[1]]))),
      metric$is_perc == 1L ~ as.character(round(riskmetric_score[[metric$name]]*100, 2)[[1]]),
      TRUE ~ as.character(riskmetric_assess[[metric$name]][[1]][1:length(riskmetric_assess[[metric$name]])])
    )
    
    metric_score <- case_when(
      is.na(riskmetric_score[[metric$name]][1]) ~ "NA",
      TRUE ~ as.character(riskmetric_score[[metric$name]][1]) # rounding
    )
    
    dbUpdate(
      "INSERT INTO package_metrics (package_id, metric_id, value, metric_score, encode) 
      VALUES ({package_id}, {metric$id}, {metric_value}, {metric_score}, $pkg_assess)", db_name,
      params = list(pkg_assess = assessment_serialized[metric$name,])
    )
  }

  # get suggests and add it to package_metrics table
  tar_file <- file.path("tarballs", glue::glue("{pkg_name}_{pkg_version}.tar.gz"))
  if (file.exists(tar_file)) {
    desc_file <- glue::glue("{pkg_name}/DESCRIPTION")
    
    tar_con <- archive::archive_read(tar_file, desc_file, format = "tar")
    on.exit(close(tar_con))
    
    desc_con <- desc::description$new(text = readLines(tar_con))
    if ('Suggests' %in% desc_con$fields()) {
      sug_vctr <- desc_con$get_list(key = 'Suggests') %>% sort()
    } else {
      msg <- paste("Suggests not found for package", pkg_name)
      rlang::warn(msg)
      sug_vctr <- character(0)
    }
  } else {
    sug_vctr <- unlist(tools::package_dependencies(pkg_name, available.packages(contrib.url(repos = "http://cran.us.r-project.org")),
                                                   which=c("Suggests"), recursive=FALSE)) %>% unname() %>% sort()
  }

  tbl_suggests <- tibble("package" = sug_vctr, type = "Suggests") 
  attr(tbl_suggests, "class") <- c('pkg_metric_dependencies', 'pkg_metric', 'data.frame')
  lst_suggests <- list(suggests = tbl_suggests)
  mostattributes(lst_suggests) <- attributes(riskmetric_assess$dependencies)
  attr(lst_suggests, "label") <- "Package Suggests"
  
  suggests_serialized   <- data.frame(pkg_assess = I(lapply(list("suggests" = lst_suggests), serialize, connection = NULL)))
  
  metric_id <- dbSelect("select id from metric where name = 'suggests'", db_name)
  metric_value <- as.character(length(sug_vctr))
  
  dbUpdate(
    "INSERT INTO package_metrics (package_id, metric_id, value, encode) 
      VALUES ({package_id}, {metric_id$id}, {metric_value}, $pkg_assess)", db_name,
    params = list(pkg_assess = suggests_serialized["suggests",])
  )
  
  dbUpdate(
    "UPDATE package
    SET score = {format(round(riskmetric_score$pkg_score[1], 2))}
    WHERE name = {pkg_name}", db_name)
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
  
  pkgs_cum_values <- glue::glue(
    "('{pkg_name}', {pkgs_cum_metrics$month}, {pkgs_cum_metrics$year}, 
  {pkgs_cum_metrics$downloads}, '{pkgs_cum_metrics$version}')") %>%
    glue::glue_collapse(sep = ", ")
  
  if(nrow(pkgs_cum_metrics) != 0){
    dbUpdate(glue::glue(
      "INSERT INTO community_usage_metrics 
        (id, month, year, downloads, version)
        VALUES {pkgs_cum_values}"), db_name)
  }
}

#' Upload a Package List
#' 
#' Uploads a list of packages to the database. Designed to be used to set up the
#' assessment database before deployment of the application.
#'
#' @param pkg_lst character vector of packages to upload
#' @param assess_db character name (and file path) of the database
#' @param repos character vector, the base URL(s) of the repositories to use
#' @param user list containing user name and role
#' @param repo_pkgs for internal use only, allows the function
#'   `available.packages()` to be run only once
#' @param updateProgress for internal use only, provides a function to update
#'   progress meter in the application
#' 
#' @return A data frame object containing a summary of the upload process
upload_pkg_lst <- function(pkg_lst, assess_db, repos, user, repo_pkgs, updateProgress = NULL) {
  
  if (missing(assess_db)) {
    warning("No value supplied for `assess_db`. Will try to use configuration file.")
    assess_db <- get_db_config("assessment_db")
  }
  if (!file.exists(assess_db))
    stop(glue::glue("The file `{assess_db}` does not exist."))
  
  if (missing(repos)) {
    warning("No value supplied for `repos`. Will try to use configuration file.")
    repos <- get_db_config("package_repo")
  }
  check_repos(repos)
  
  if (missing(user)) {
    user <- list(name = "system_bot", role = "admin")
  }
  
  if (!isTRUE(all.equal(getOption("repos"), repos))) {
    old_options <- options()
    on.exit(options(old_options))
    options(repos = repos)
  }
  
  if (missing(repo_pkgs))
    repo_pkgs <- as.data.frame(utils::available.packages()[,1:2])
  
  if (missing(assess_db)) assess_db <- get_db_config("assessment_db")
  
  rule_lst <- process_rule_tbl(assess_db)
  
  if (is.data.frame(pkg_lst)) {
    np <- nrow(pkg_lst)
    uploaded_packages <- pkg_lst
    if ('decision' %in% names(pkg_lst)) {
      decisions <- 
        pkg_lst$decision |>
        unique() |>
        {\(x) x[!is.na(x) & x != ""]}()
      decision_lst <-
        dbSelect("SELECT decision FROM decision_categories", assess_db)[[1]]
      if (length(decisions) > 0 && !all(decisions %in% decision_lst))
        stop("Provided decisions do not match allowable list from assessment database.")
    }
  } else {
    np <- length(pkg_lst)
    uploaded_packages <-
      dplyr::tibble(
        package = pkg_lst,
        version = rep('0.0.0', np),
        status = rep('', np),
        score = rep(NA_real_, np)
      )
    
    if (!rlang::is_empty(rule_lst)) {
      uploaded_packages$decision <- ""
      uploaded_packages$decision_rule <- ""
    }
  }
  
  for (i in 1:np) {
    if (is.function(updateProgress))
      updateProgress(1, glue::glue("{uploaded_packages$package[i]}"))
      
    if (grepl("^[[:alpha:]][[:alnum:].]*[[:alnum:]]$", uploaded_packages$package[i])) {
      if (!isTRUE(getOption("shiny.testmode")))
        ref <- riskmetric::pkg_ref(uploaded_packages$package[i],
                                   source = "pkg_cran_remote")
      else
        ref <- test_pkg_refs[[uploaded_packages$package[i]]]
    } else {
      ref <- list(name = uploaded_packages$package[i],
                  source = "name_bad")
    }
    
    if (ref$source %in% c("pkg_missing", "name_bad")) {
      if (is.function(updateProgress))
        updateProgress(4, glue::glue("Package {uploaded_packages$package[i]} not found"))
      
      # Suggest alternative spellings using utils::adist() function
      v <- utils::adist(uploaded_packages$package[i], repo_pkgs[[1]], ignore.case = FALSE)
      rlang::inform(paste("Package name",uploaded_packages$package[i],"was not found."))
      
      suggested_nms <- paste("Suggested package name(s):",paste(head(repo_pkgs[[1]][which(v == min(v))], 10),collapse = ", "))
      rlang::inform(suggested_nms)

      uploaded_packages$status[i] <- if (shiny::isRunning()) HTML(paste0('<a href="#" title="', suggested_nms, '">not found</a>')) else 'not found'
      
      wrn_msg <- {
        if (ref$source == "pkg_missing")
          glue::glue('Package {ref$name} was flagged by riskmetric as {ref$source}.')
        else
          glue::glue("Riskmetric can't interpret '{ref$name}' as a package reference.")
      }
      if (shiny::isRunning())
        loggit::loggit('WARN', wrn_msg)
      else
        warning(wrn_msg)
      
      next
    }
    
    uploaded_packages$version[i] <- as.character(ref$version)
    if (is.function(updateProgress))
      updateProgress(1, glue::glue("{uploaded_packages$package[i]} v{uploaded_packages$version[i]}"))
    
    found <- nrow(dbSelect(
      "SELECT name
              FROM package
              WHERE name = {uploaded_packages$package[i]}",
      assess_db))
    
    uploaded_packages$status[i] <- ifelse(found == 0, 'new', 'duplicate')
    
    # Add package and metrics to the db if package is not in the db.
    if(!found) {
      # Get and upload pkg general info to db.
      if (is.function(updateProgress))
        updateProgress(1)

      if (!isTRUE(getOption("shiny.testmode"))) {
        dwn_ld <- try(utils::download.file(ref$tarball_url, file.path("tarballs", basename(ref$tarball_url)), 
                                           quiet = TRUE, mode = "wb"),
                      silent = TRUE)
        if (inherits(dwn_ld, "try-error") | dwn_ld != 0) {
          wrn_msg <- glue::glue("Unable to download the source files for {uploaded_packages$package[i]} from '{ref$tarball_url}'.")
          if (shiny::isRunning()) loggit::loggit("INFO", wrn_msg) else warning(wrn_msg)
        }
      }
      
      insert_pkg_info_to_db(uploaded_packages$package[i], uploaded_packages$version[i], assess_db)
      
      # Get and upload maintenance metrics to db.
      if (is.function(updateProgress))
        updateProgress(1)
      insert_riskmetric_to_db(uploaded_packages$package[i], uploaded_packages$version[i], assess_db)
      
      # Get and upload community metrics to db.
      if (is.function(updateProgress))
        updateProgress(1)
      insert_community_metrics_to_db(uploaded_packages$package[i], assess_db)
      
      uploaded_packages$score[i] <- get_pkg_info(uploaded_packages$package[i], assess_db)$score
      if ("decision" %in% names(uploaded_packages) && uploaded_packages$decision[i] != "") {
        decision_id <- dbSelect("SELECT id FROM decision_categories WHERE decision = {uploaded_packages$decision[i]}", assess_db)
        log_message <- glue::glue("Decision for the package {uploaded_packages$package[i]} was assigned {uploaded_packages$decision[i]} by upload designation.")
        db_message <- glue::glue("Decision was assigned '{uploaded_packages$decision[i]}' by upload designation.")
        dbUpdate("UPDATE package SET decision_id = {decision_id},
                        decision_by = {user$name}, decision_date = {get_Date()}
                         WHERE name = {uploaded_packages$package[i]}",
                 assess_db)
        loggit::loggit("INFO", log_message)
        dbUpdate(
          "INSERT INTO comments
          VALUES ({uploaded_packages$package[i]}, {user$name}, {paste(user$role, collapse = ', ')},
          {db_message}, 'o', {getTimeStamp()})",
          assess_db)
      } else if (!rlang::is_empty(rule_lst)) {
        assigned_decision <- assign_decisions(rule_lst, uploaded_packages$package[i], assess_db)
        uploaded_packages$decision[i] <- assigned_decision$decision
        uploaded_packages$decision_rule[i] <- assigned_decision$decision_rule
      }
    } else if ("decision" %in% names(uploaded_packages) && uploaded_packages$decision[i] != "") {
      uploaded_packages$decision[i] <- ""
    }
    if (is.function(updateProgress))
      updateProgress(3)
  }
  if (is.function(updateProgress))
    updateProgress(1, "**Completed Package Uploads**")
  
  return(uploaded_packages)
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
  dbUpdate(
    "UPDATE metric
    SET weight = {metric_weight}
    WHERE name = {metric_name}"
  , db_name)
}

#' The Rescore Function
#'
#' Rescore package based on stored riskmetric assessments and weights
#' 
#' @param pkg_name string name of the package
#' @param db_name character name (and file path) of the database
#' 
#' @import dplyr
#' @importFrom riskmetric pkg_score
#' @importFrom glue glue 
#' @importFrom purrr pmap_dfc set_names
#' 
#' @returns nothing
#' @noRd
rescore_package <- function(pkg_name, 
                            db_name = golem::get_golem_options('assessment_db_name')) {
  
  riskmetric_assess <- get_assess_blob(pkg_name, db_name)
  
  # Get the metrics weights to be used during pkg_score.
  metric_weights_df <- get_metric_weights(db_name)
  metric_weights <- metric_weights_df$weight
  names(metric_weights) <- metric_weights_df$name
  
  riskmetric_score <-
    riskmetric_assess %>%
    riskmetric::pkg_score(weights = metric_weights)
  
  dbUpdate(
    "UPDATE package
    SET score = {format(round(riskmetric_score$pkg_score[1], 2))}
    WHERE name = {pkg_name}", db_name)
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

set_credentials_table <- function(credentials, db_name = golem::get_golem_options('credentials_db_name'), passphrase) {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_name)
  
  out <- shinymanager::write_db_encrypt(conn = con, value = credentials, name = "credentials", passphrase = passphrase)
  
  DBI::dbDisconnect(con)
  invisible(out)
}
