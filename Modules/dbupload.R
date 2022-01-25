#####################################################################################################################
# dbupload.R - Uploading the general and metric info of the package into DB
# Author: K Aravind Reddy
# Date: July 13th, 2020
# License: MIT License
#####################################################################################################################


# Get the package general information from CRAN/local.
get_packages_info_from_web <- function(package_name) {
  tryCatch(
    expr = {
      webpage <-
        read_html(paste0(
          "https://cran.r-project.org/web/packages/",
          package_name
        ))
      
      title_html <- html_nodes(webpage, 'h2')
      title <- html_text(title_html)
      title <- str_replace_all(title, "\n  ", "")
      title <- str_replace_all(title, "'", "")
      title <- str_replace_all(title, '"', "")
      
      desc_html <- html_nodes(webpage, 'p')
      desc <- html_text(desc_html)
      desc <- desc[1]
      desc <- str_replace_all(desc, "\n  ", "")
      desc <- str_replace_all(desc, "'", "")
      desc <- str_replace_all(desc, '"', "")
      
      ver_html <- html_nodes(webpage, 'td')
      ver <- html_text(ver_html)
      for(i in 1:length(ver)){
        if(!is.na(ver[i])){
          if(ver[i] == "Version:"){
            ver<-ver[i+1]
          }
        }
      }
      ver <- str_replace_all(ver, "\n  ", "")
      ver <- str_replace_all(ver, "'", "")
      ver <- str_replace_all(ver, '"', "")
      
      
      main_html <- html_nodes(webpage, 'td')
      main<-html_text(main_html)
      for(i in 1:length(main)){
        if(!is.na(main[i])){
          if(main[i] == "Maintainer:"){
            main<-main[i+1]
          }
        }
      }
      main <- str_replace_all(main, "\n  ", "")
      main <- str_replace_all(main, "'", "")
      main <- str_replace_all(main, '"', "")
      
      
      auth_html <- html_nodes(webpage, 'td')
      auth<-html_text(auth_html)
      for(i in 1:length(auth)){
        if(!is.na(auth[i])){
          if(auth[i] == "Author:"){
            auth<-auth[i+1]
          }
        }
      }
      auth <- str_replace_all(auth, "\n  ", "")
      auth <- str_replace_all(auth, "'", "")
      auth <- str_replace_all(auth, '"', "")
      
      pub_html <- html_nodes(webpage, 'td')
      pub <- html_text(pub_html)
      for(i in 1:length(pub)){
        if(!is.na(pub[i])){
          if(pub[i] == "Published:"){
            pub<-pub[i+1]
          }
        }
      }
      pub <- str_replace_all(pub, "\n  ", "")
      pub <- str_replace_all(pub, "'", "")
      pub <- str_replace_all(pub, '"', "")
      
      
      lis_html <- html_nodes(webpage, 'td')
      lis<-html_text(lis_html)
      for(i in 1:length(lis)){
        if(!is.na(lis[i])){
          if(lis[i] == "License:"){
            lis<-lis[i+1]
          }
        }
      }
      lis <- str_replace_all(lis, "\n  ", "")
      lis <- str_replace_all(lis, "'", "")
      lis <- str_replace_all(lis, '"', "")
      
      genInfo_upload_to_DB(package_name, ver, title, desc, auth, main, lis, pub)
      
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
            
            genInfo_upload_to_DB(package_name, ver, title, desc, auth, main, lis, pub)
          }}
      } else{
        loggit("ERROR", paste("Error in extracting general info of the package", package_name, "info", e), app = "fileupload-webscraping")
      }
    }
  )
}

# Upload the general info into DB.
genInfo_upload_to_DB <- function(package_name, ver, title, desc, auth, main, lis, pub) {
  tryCatch(
    expr = {
      db_ins(paste0("INSERT or REPLACE INTO package
                    (name, version, title, description, maintainer, author, license, published_on, decision)
                    values(", "'", package_name, "',", "'", ver, "',", "'", title ,"'," , "'", desc, "',",
                     "'", main, "',", "'", auth, "',", "'", lis, "',", "'", pub, "', '')"))
    },
    error = function(e) {
      loggit("ERROR", paste("Error in uploading the general info of the package", package_name, "info", e), app = "fileupload-DB")
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
  
  package_id <- db_fun(paste0("SELECT id FROM package WHERE name = ", "'", package_name, "';"))
  
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
    
    db_ins(
      paste0("INSERT INTO package_metrics
               (package_id, metric_id, weight, value) values(",
             package_id, ",",
             metric$id, ",",
             metric$weight , "," ,
             "'", metric_value, "'",
             ")"
      )
    )
  }

  db_ins(paste0("UPDATE package
                SET score = '", format(round(riskmetric_score$pkg_score[1], 2)), "'",
                " WHERE name = '" , package_name, "'"))
}


# Get community usage metrics info and upload into DB.
metric_cum_Info_upload_to_DB <- function(package_name) {
  pkg_vers_date_final<<-data.frame(matrix(ncol = 4, nrow = 0))
  time_since_first_release<<-NA
  time_since_version_release<<-NA
  total_downloads<<-NA
  
  tryCatch(
    expr = {
      
      vrsns_lst <- versions::available.versions(package_name) 
      vrsns_df  <- as.data.frame(vrsns_lst[[1]]) %>% 
        mutate(startdt = ymd(floor_date(as.Date(date), unit="month"))) %>% 
        #filter(available == TRUE) %>% 
        select(-available)
      vrsns_vec <- as.vector(vrsns_df[,"version"])
      
      to_date <- Sys.Date()
      # back3yrs <- to_date - 3*365
      nr <- nrow(vrsns_df)
      # go back as far as possible with available versions
      fr_date <- as.Date(vrsns_df[nr, "date"])
      
      downlds <- cranlogs::cran_downloads(package_name, from=fr_date, to=to_date)
      # drop current month, as was done previously ?
      downlds <- filter(downlds, !(months(downlds$date) == months(Sys.Date()) & year(downlds$date) == year(Sys.Date())))
  
      downlds_by_monyr <- downlds %>% 
        mutate(monthyear = ymd(floor_date(date, unit="month"))) %>% 
        mutate(nextmon   = ymd(ceiling_date(date, unit="month"))) %>%
        group_by(monthyear, nextmon) %>%
        summarise(Downloads = sum(count), .groups = 'drop') 
      
      total_downloads <- summarise(downlds_by_monyr, sum(Downloads))
      downloads_1yr <- sum(downlds_by_monyr$Downloads[(nrow(downlds_by_monyr)-11):nrow(downlds_by_monyr)])
      
      # slot the version and date into the correct month/year, create Month and Position
      downlds_data <- left_join(downlds_by_monyr, vrsns_df, by = c("monthyear" = "startdt") ) 
      
      # time_since_first_release   <- floor(as.numeric((downlds$date[nrow(downlds)] - as.Date(vrsns_df[nr,"date"]))) / (365.25/12) )
      # time_since_version_release <- floor(as.numeric((downlds$date[nrow(downlds)] - as.Date(vrsns_df[1,"date"]))) / (365.25/12) )
      time_since_first_release   <- floor(as.numeric(Sys.Date() - as.Date(vrsns_df[nr,"date"])) / (365.25/12) )
      time_since_version_release <- floor(as.numeric(Sys.Date() - as.Date(vrsns_df[1,"date"])) / (365.25/12) )
      
      pkg_vers_date_final <- downlds_data %>%
        mutate(Month = as.character(monthyear, format = "%B %Y")) %>%
        mutate(Position = ifelse(is.na(version), nrow(downlds_data) +1, row_number() -1 ))
      
    },
    error = function(e) {
      loggit("ERROR", paste("Error in extracting cum metric info of the package:", package_name, "info", e), app = "versions::available.versions")
    }
  )# End of try catch
  
  for (i in 1:nrow(pkg_vers_date_final)) {
    db_ins(paste0("INSERT INTO CommunityUsageMetrics values(",
                  "'", package_name,"',", "'", downloads_1yr, "',",
                  "'", pkg_vers_date_final$Month[i], "',", "'", pkg_vers_date_final$Downloads[i], "',", 
                  "'", pkg_vers_date_final$version[i], "',", "'", pkg_vers_date_final$Position[i], "',",
                  "'", time_since_first_release, "',", "'", time_since_version_release, "'" , ")"))
    if(nrow(pkg_vers_date_final) == 0){
      break
    }
  }
}
# End of the functions