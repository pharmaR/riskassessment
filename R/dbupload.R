# Get the package general information from CRAN/local and upload it to db.
# TODO: get and upload should be distinct functions.
insert_pkg_info_to_db <- function(pkg_name) {
  tryCatch(
    expr = {
      webpage <- read_html(glue(
          'https://cran.r-project.org/web/packages/{pkg_name}'))
      
      #' Regex that finds entry: '\n ', "'", and '"' (the `|` mean 'or' and the 
      #' `\`` is to scape the double quotes).
      pattern <- '\n |\'|\"|\\"'
      
      # Save div with class container to get the title and description.
      div_container <- webpage %>% html_nodes("div.container")
      
      # Read package title and clean it.
      title <- div_container %>% 
        html_nodes("h2") %>% 
        html_text() %>%
        str_remove_all(pattern = pattern)
      
      # Read package description and clean it.
      description <- div_container %>% 
        html_nodes("h2 + p") %>% 
        html_text() %>%
        str_remove_all(pattern = pattern)
      
      # Get the table displaying version, authors, etc.
      #' TODO: this variable is redundant for now. Split the entire function into
      #' two: get the data in one function and upload in another.
      table_info <- (webpage %>% html_table())[[1]] %>%
        mutate(X1 = str_remove_all(string = X1, pattern = ':')) %>%
        mutate(X2 = str_remove_all(string = X2, pattern = pattern)) %>%
        pivot_wider(names_from = X1, values_from = X2) %>%
        select(Version, Maintainer, Author, License, Published) %>%
        mutate(Title = title, Description = description)
      
      upload_package_to_db(pkg_name, table_info$Version, table_info$Title,
                           table_info$Description, table_info$Author,
                           table_info$Maintainer, table_info$License,
                           table_info$Published)
      
    },
    error = function(e) {
      if (package_name %in% rownames(installed.packages()) == TRUE) {
        for (i in .libPaths()) {
          if(file.exists(file.path(i, package_name)) == TRUE) {
            i <- file.path(i, package_name)
            d <- description$new(i)
            title <- d$get("Title")
            ver <- d$get("Version")
            desc <- d$get("Description")
            main <- d$get("Maintainer")
            auth <- d$get("Author")
            lis <- d$get("License")
            pub <- d$get("Packaged")
            
            upload_package_to_db(package_name, ver, title, desc, auth, main, lis, pub)
          }}
      } else{
        loggit("ERROR", paste("Error in extracting general info of the package",
                              package_name, "info", e), app = "fileupload-webscraping")
      }
    }
  )
}

# Upload the general info into DB.
upload_package_to_db <- function(name, version, title, description,
                                 authors, maintainers, license, published_on) {
  tryCatch(
    expr = {
      dbUpdate(glue(
        "INSERT or REPLACE INTO package
        (name, version, title, description, maintainer, author,
        license, published_on, decision, date_added)
        VALUES('{name}', '{version}', '{title}', '{description}',
        '{maintainers}', '{authors}', '{license}', '{published_on}',
        '', '{Sys.Date()}')"))
    },
    error = function(e) {
      loggit("ERROR", paste("Error in uploading the general info of the package", name, "info", e),
             app = "fileupload-DB")
    }
  )
}


# Get the maintenance and testing metrics info and upload into DB.
insert_maintenance_metrics_to_db <- function(package_name){
  
  riskmetric_assess <-
    pkg_ref(package_name) %>%
    as_tibble() %>%
    pkg_assess()
  
  # Get the metrics weights to be used during pkg_score.
  metric_weights_df <- dbSelect("SELECT id, name, weight FROM metric")
  metric_weights <- metric_weights_df$weight
  names(metric_weights) <- metric_weights_df$name
  
  riskmetric_score <-
    riskmetric_assess %>%
    pkg_score(weights = metric_weights)
  
  package_id <- dbSelect(glue("SELECT id FROM package WHERE name = '{package_name}'"))
  
  # Leave method if package not found.
  if(nrow(package_id) == 0){
    print("PACKAGE NOT FOUND.")
    loggit("WARN", paste("Package", package_name, "not found."))
    return()
  }
  
  # Insert all the metrics (columns of class "pkg_score") into the db.
  # TODO: Are pkg_score and pkg_metric_error mutually exclusive?
  for(row in 1:nrow(metric_weights_df)){
    metric <- metric_weights_df %>% slice(row)
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
    
    dbUpdate(glue(
      "INSERT INTO package_metrics (package_id, metric_id, weight, value) 
      VALUES ({package_id}, {metric$id}, {metric$weight}, '{metric_value}')")
    )
  }
  
  dbUpdate(glue(
    "UPDATE package
    SET score = '{format(round(riskmetric_score$pkg_score[1], 2))}'
    WHERE name = '{package_name}'"))
}


# Get community usage metrics info and upload into DB.
insert_community_metrics_to_db <- function(pkg_name) {
  
  pkgs_cum_metrics <- tibble()
  
  tryCatch(
    expr = {
      
      # Get the package versions and dates.
      pkg_page <- read_html(
        glue('https://cran.r-project.org/src/contrib/Archive/{pkg_name}'))
      versions_with_dates <- pkg_page %>%
        html_node('table') %>%
        html_table() %>%
        select(-c("", "Description", 'Size')) %>%
        filter(`Last modified` != "") %>%
        mutate(date = as.Date(`Last modified`), .keep = 'unused') %>%
        mutate(version = str_remove_all(
          string = Name, pattern = glue('{pkg_name}_|.tar.gz')),
          .keep = 'unused') %>%
        mutate(month = month(date)) %>%
        mutate(year = year(date))

      # First release date.
      first_release_date <- versions_with_dates %>%
        pull(date) %>%
        min()
      
      # Get the number of downloads by month, year.
      pkgs_cum_metrics <- cranlogs::cran_downloads(pkg_name,
                                                   from = first_release_date,
                                                   to = Sys.Date()) %>%
        filter(month(date) != month(Sys.Date())) %>%
        mutate(month = month(date)) %>%
        mutate(year = year(date)) %>%
        group_by(month, year) %>%
        summarise(downloads = sum(count)) %>%
        ungroup() %>%
        left_join(versions_with_dates, by = c('month', 'year')) %>%
        select(-date)
      
    },
    error = function(e) {
      loggit("ERROR", paste("Error extracting cum metric info of the package:",
                            pkg_name, "info", e),
             app = "fileupload-webscraping", echo = FALSE)
    }
  )
  
  if(nrow(pkgs_cum_metrics) != 0)
    for (i in 1:nrow(pkgs_cum_metrics)) {
      dbUpdate(glue(
        "INSERT INTO community_usage_metrics 
        (id, month, year, downloads, version)
        VALUES ('{pkg_name}', {pkgs_cum_metrics$month[i]},
        {pkgs_cum_metrics$year[i]}, {pkgs_cum_metrics$downloads[i]},
        '{pkgs_cum_metrics$version[i]}')"))
    }
  }
