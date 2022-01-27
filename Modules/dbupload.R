# Get the package general information from CRAN/local.
get_packages_info_from_web <- function(package_name) {
  tryCatch(
    expr = {
      webpage <-
        read_html(paste0(
          "https://cran.r-project.org/web/packages/",
          package_name
        ))
      
      #' Regex that finds entry: '\n ', "'", and '"' (the `|` mean 'or' and the 
      #' `\`` is to scape the double quotes).
      pattern <- '\n |\'|\"|\\"'
      
      # Read package title and clean it.
      title <- webpage %>% 
        html_nodes("div.container h2") %>% 
        html_text() |>
        str_remove_all(pattern = pattern)
      
      # Read package description and clean it.
      description <- webpage %>% 
        html_nodes("div.container h2 + p") %>% 
        html_text() |>
        str_remove_all(pattern = pattern)
      
      # Part of the package's information is under the td tag. Saving it 
      # since we will use it multiple times.
      td <- html_nodes(webpage, 'td')
      
      # Read version and clean it.
      version <- td[2] %>%
        html_text() |>
        str_remove_all(pattern = pattern)
      
      # Read maintainers and clean it.
      maintainers <- td[14] %>%
        html_text() |>
        str_remove_all(pattern = pattern)
      
      # Read authors and clean it.
      authors <- td[12] %>%
        html_text() |>
        str_remove_all(pattern = pattern)
      
      # Read published date and clean it.
      published <- td[10] %>%
        html_text() |>
        str_remove_all(pattern = pattern)
      
      # Read published date and clean it.
      license <- td[18] %>%
        html_text() |>
        str_remove_all(pattern = pattern)
      
      upload_package_to_db(package_name, version, title, description, authors,
                           maintainers, license, published)
      
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
      db_ins(glue(
        "INSERT or REPLACE INTO package
        (name, version, title, description, maintainer, author,
        license, published_on, decision, date_added)
        VALUES('{name}', '{version}', '{title}', '{description}',
        '{maintainers}', '{authors}', '{license}', '{published_on}',
        '{Sys.Date()}', '')"))
    },
    error = function(e) {
      loggit("ERROR", paste("Error in uploading the general info of the package", name, "info", e),
             app = "fileupload-DB")
    }
  )
}


# Get the maintenance and testing metrics info and upload into DB.
metric_mm_tm_Info_upload_to_DB <- function(package_name){
  
  riskmetric_assess <-
    pkg_ref(package_name) %>%
    as_tibble() %>%
    pkg_assess()
  
  # Get the metrics weights to be used during pkg_score.
  metric_weights_df <- db_fun("SELECT id, name, weight FROM metric")
  metric_weights <- metric_weights_df$weight
  names(metric_weights) <- metric_weights_df$name
  
  riskmetric_score <-
    riskmetric_assess %>%
    pkg_score(weights = metric_weights)
  
  package_id <- db_fun(glue("SELECT id FROM package WHERE name = '{package_name}'"))
  
  # Leave method if package not found.
  # TODO: save this to the json file.
  if(nrow(package_id) == 0){
    print("PACKAGE NOT FOUND.")
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
    
    db_ins(glue(
      "INSERT INTO package_metrics (package_id, metric_id, weight, value) 
      VALUES ({package_id}, {metric$id}, {metric$weight}, '{metric_value}')")
    )
  }
  
  db_ins(glue(
    "UPDATE package
    SET score = '{format(round(riskmetric_score$pkg_score[1], 2))}'
    WHERE name = '{package_name}'"))
}


# Get community usage metrics info and upload into DB.
metric_cum_Info_upload_to_DB <- function(package_name) {
  pkg_vers_date_final <<- data.frame(matrix(ncol = 4, nrow = 0))
  time_since_first_release <<- NA
  time_since_version_release <<- NA
  downloads_1yr <<- NA
  
  tryCatch(
    expr = {
      downloads_yrs_br_i <- cranlogs::cran_downloads(package_name, from=Sys.Date()-730, to=Sys.Date())
      downloads_yrs_br_i <- filter(downloads_yrs_br_i, months(downloads_yrs_br_i$date) != months(Sys.Date()))
      downloads_yrs_br_i$date <- paste( months(downloads_yrs_br_i$date), year(downloads_yrs_br_i$date) )
      count<-c()
      for (i in 1:length(unique(downloads_yrs_br_i$date))) {
        count_df <- filter(downloads_yrs_br_i, downloads_yrs_br_i$date == unique(downloads_yrs_br_i$date)[i])
        count[i] <- sum(count_df$count)
      }
      downloads_yrs_br <-data.frame(Month = unique(downloads_yrs_br_i$date), Downloads = count)
      downloads_1yr <- sum(downloads_yrs_br$Downloads[(nrow(downloads_yrs_br)-11):nrow(downloads_yrs_br)])
      
      colnames(pkg_vers_date_final) <<- c("Month", "Downloads", "verRelease", "Position")
      pkg_vers_date_final <- downloads_yrs_br
      pkg_vers_date_final$Month <- as.character(pkg_vers_date_final$Month)
      pkg_vers_date_final$Position <- 12
      
      pkg_html <- read_html(paste0("https://github.com/cran/", package_name, "/tags"))
      pkg_nodes_v <- html_nodes(pkg_html, 'h4')
      pkg_text_v <- html_text(pkg_nodes_v)
      pkg_text_v <- str_split(pkg_text_v,"\n")
      pkg_vers <- c()
      for (i in 1:length(pkg_text_v)) { 
        pkg_vers[i]<-(trimws(pkg_text_v[[i]][3]))
      }
      pkg_vers <- pkg_vers[c(3:length(pkg_vers))]
      
      pkg_vers1 <- pkg_vers[length(pkg_vers)]
      loop<-"not_started"
      while (pkg_vers1 != "") {
        pkg_html1 <- read_html(paste0("https://github.com/cran/",package_name,"/tags?after=",pkg_vers1))
        pkg_nodes_v1 <- html_nodes(pkg_html1, 'h4')
        pkg_text_v1 <- html_text(pkg_nodes_v1)
        pkg_text_v1 <- str_split(pkg_text_v1,"\n")
        pkg_vers1 <- c()
        for (i in 1:length(pkg_text_v1)) { 
          pkg_vers1[i]<-(trimws(pkg_text_v1[[i]][3]))
        }
        pkg_vers1 <- pkg_vers1[length(pkg_vers1)]
        if (is.na(pkg_vers1)) {
          if(loop != "looped"){
            pkg_vers1 <- pkg_vers[length(pkg_vers)]
            pkg_nodes_d1 <- html_nodes(pkg_html, 'relative-time')
            pkg_text_d1 <- html_text(pkg_nodes_d1)
            pkg_date1 <- str_remove_all(pkg_text_d1[length(pkg_text_d1)], ",")
            pkg_date1 <- as.Date(pkg_date1, format = "%h %d %Y")
            time_since_first_release <- Sys.Date() - pkg_date1
            time_since_first_release <- floor(as.numeric(time_since_first_release / 30))
            break
          }else{
            break 
          }
        } else{
          pkg_nodes_d1 <- html_nodes(pkg_html1, 'relative-time')
          pkg_text_d1 <- html_text(pkg_nodes_d1)
          pkg_date1 <- str_remove_all(pkg_text_d1[length(pkg_text_d1)], ",")
          pkg_date1 <- as.Date(pkg_date1, format = "%h %d %Y")
          time_since_first_release <- Sys.Date() - pkg_date1
          time_since_first_release <- floor(as.numeric(time_since_first_release / 30))
          loop<-"looped"
        }
      }
      
      pkg_nodes_d <- html_nodes(pkg_html, 'relative-time')
      pkg_text_d <- html_text(pkg_nodes_d)
      pkg_date <- str_remove_all(pkg_text_d, ",")
      pkg_date <- as.Date(pkg_date, format = "%h %d %Y")
      time_since_version_release <- Sys.Date() - pkg_date[1]
      time_since_version_release <- floor(as.numeric(time_since_version_release / 30))
      
      pkg_vers_date <- data.frame(Version = c(pkg_vers), Date = c(paste(months(pkg_date), year(pkg_date))))
      pkg_vers_date <- pkg_vers_date %>% map_df(rev)
      pkg_vers_date$Date <- as.character(pkg_vers_date$Date)
      pkg_vers_date$Version <- as.character(pkg_vers_date$Version)
      pkg_vers_date <- filter(pkg_vers_date, pkg_vers_date$Date %in% pkg_vers_date_final$Month)
      
      for (i in 1:length(pkg_vers_date_final$Month)) {
        for (j in 1:length(pkg_vers_date$Date)) {
          if (pkg_vers_date_final$Month[i] == pkg_vers_date$Date[j]) {
            pkg_vers_date_final$verRelease[i] <- pkg_vers_date$Version[j]
            pkg_vers_date_final$Position[i] <- i - 1
            break
          } else{
            pkg_vers_date_final$verRelease[i] <- NA
            pkg_vers_date_final$Position[i] <- 12
          }
        }
      }
    },
    error = function(e) {
      loggit("ERROR", paste("Error in extracting cum metric info of the package:", package_name, "info", e),
             app = "fileupload-webscraping", echo = FALSE)
    }
  )# End of try catch
  
  for (i in 1:nrow(pkg_vers_date_final)) {
    db_ins(paste0("INSERT INTO community_usage_metrics values(",
                  "'", package_name,"',", "'", downloads_1yr, "',",
                  "'", pkg_vers_date_final$Month[i], "',", "'", pkg_vers_date_final$Downloads[i], "',", 
                  "'", pkg_vers_date_final$verRelease[i], "',", "'", pkg_vers_date_final$Position[i], "',",
                  "'", time_since_first_release, "',", "'", time_since_version_release, "'" , ")"))
    if(nrow(pkg_vers_date_final) == 0){
      break
    }
  }
  
}

# End of the functions