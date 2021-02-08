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
          if (file.exists(paste(i, "/", package_name, sep = "")) == TRUE) {
            i <- paste0(i, "/", package_name)
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
  
  riskmetric_score <-
    riskmetric_assess %>%
    pkg_score()
  
  package_id <- db_fun(paste0("SELECT id FROM package WHERE name = ", "'", package_name, "';"))
  
  # Leave method if package not found.
  # TODO: save this to the json file.
  if(nrow(package_id) == 0){
    print("PACKAGE NOT FOUND.")
    return()
  }
  
  # Insert all the metrics (columns of class "pkg_score") into the db.
  # TODO: Are pkg_score and pkg_metric_error mutually exclusive?
  for(metric_name in colnames(riskmetric_score)){
    if("pkg_score" %in% class(riskmetric_score[[metric_name]])){
      
      metric_id <- db_fun(paste0("SELECT id, class
                                 FROM metric
                                 WHERE name = ", "'", metric_name, "'", ";"))
      
      # Skip if the metric is not on the metrics table.
      if(nrow(metric_id) == 0) next
      
      # If the metric errors out,
      #   then save "pkg_metric_error" as the value of the metric.
      # If the metric has NA or 0,
      #   then save such value as the metric value.
      # Otherwise, save all the possible values of the metric
      #   (note: has_website for instance may have multiple values).
      metric_value <- ifelse(
        "pkg_metric_error" %in% class(riskmetric_assess[[metric_name]][[1]]),
        "pkg_metric_error",
        # Since the actual value of these metrics appear on riskmetric_score
        #   and not on riskmetric_assess, they need to be treated differently.
        # TODO: this code is not clean, fix it. Changes to riskmetric?
        ifelse(metric_name %in% c('bugs_status', 'export_help'),
               round(riskmetric_score[[metric_name]]*100, 2),
               riskmetric_assess[[metric_name]][[1]][1:length(riskmetric_assess[[metric_name]])]))

      db_ins(
        paste0("INSERT INTO package_metrics
               (package_id, metric_id, weight, value) values(",
                package_id, ",",
                metric_id$id, ",",
                "1" , "," ,
                "'", metric_value, "'",
                ")"
        )
      )
    }
  }

  db_ins(paste0("UPDATE package
                SET score = '", format(round(riskmetric_score$pkg_score[1], 2)), "'",
                " WHERE name = '" , package_name, "'"))
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
    db_ins(paste0("INSERT INTO CommunityUsageMetrics values(",
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