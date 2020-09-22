#####################################################################################################################
# dbupload.R - Uploading the general and metric info of the package into DB
# Author: K Aravind Reddy
# Date: July 13th, 2020
# License: MIT License
#####################################################################################################################


# Start of the functions

# 1. Function to get the package general information from CRAN/local.

get_packages_info_from_web <- function(package_name, package_version) {
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
      ver<-html_text(ver_html)
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
      
      if (ver != package_version) signalCondition(e)
      
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
      pub<-html_text(pub_html)
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
        print(paste("in error function for",package_name,"version",package_version))
        for (i in .libPaths()) {
          if (file.exists(paste(i, "/", package_name, sep = "")) == TRUE) {
            i <- paste0(i, "/", package_name)
            d <- description$new(i)
            title <- d$get("Title")
            ver <- d$get("Version")
            desc <- d$get("Description")
            desc <- gsub("'", "", desc) # remove single quotes
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
  )# End of try catch
}

# End of the function

# 2. Function to upload the general info into DB.

genInfo_upload_to_DB <- function(package_name, ver, title, desc, auth, main, lis, pub) {
  tryCatch(
    expr = {
      db_ins(paste0( "INSERT or REPLACE INTO Packageinfo values(", "'", package_name, "',", "'", ver, "',", "'", title ,"'," , "'", desc, "',",
                     "'", main, "',", "'", auth, "',", "'", lis, "',", "'", pub, "',", "'',", "''", ")"))
    },
    error = function(e) {
      loggit("ERROR", paste("Error in uploading the general info of the package", package_name, "info", e), app = "fileupload-DB")
    }
  )# End of try catch 

}
# End of the function


# 3. Function to get the maintenance and testing metrics info and upload into DB.

metric_mm_tm_Info_upload_to_DB <- function(package_name, package_version){
  
  package_riskmetric1 <<-
    pkg_ref(package_name) %>%
    as_tibble() %>%
    pkg_assess() %>%
    pkg_score() %>%
    mutate(risk = summarize_scores(.))
  
  package_riskmetric2 <<-
    pkg_ref(package_name) %>%
    as_tibble() %>%
    pkg_assess()
  
  package_riskmetric1$bugs_status <- package_riskmetric1$bugs_status*100
  package_riskmetric1$export_help <- package_riskmetric1$export_help*100
  

  db_ins(paste0("INSERT INTO MaintenanceMetrics values(", 
                "'", package_name,    "',", 
                "'", package_version, "',",
                "'", package_riskmetric1$has_vignettes[1], ",", ifelse(class(package_riskmetric2$has_vignettes[[1]])[1] == "pkg_metric_error", -1, package_riskmetric2$has_vignettes[[1]][1]), "',",
                "'", package_riskmetric1$has_news[1], ",",  ifelse(class(package_riskmetric2$has_news[[1]])[1] == "pkg_metric_error", -1, package_riskmetric2$has_news[[1]][1]), "',", 
                "'", package_riskmetric1$news_current[1], ",",  ifelse(class(package_riskmetric2$news_current[[1]])[1] == "pkg_metric_error", -1, package_riskmetric2$news_current[[1]][1]), "',",  
                "'", package_riskmetric1$has_website[1], ",",  ifelse(class(package_riskmetric2$has_website[[1]])[1] == "pkg_metric_error", -1, package_riskmetric2$has_website[[1]][1]), "',", 
                "'", package_riskmetric1$has_bug_reports_url[1], ",",  ifelse(class(package_riskmetric2$has_bug_reports_url[[1]])[1] == "pkg_metric_error", -1, package_riskmetric2$has_bug_reports_url[[1]][1]), "',",
                "'", package_riskmetric1$has_maintainer[1], ",",  ifelse(class(package_riskmetric2$has_maintainer[[1]])[1] == "pkg_metric_error", -1, package_riskmetric2$has_maintainer[[1]][1]), "',", 
                "'", package_riskmetric1$has_source_control[1], ",",  ifelse(class(package_riskmetric2$has_source_control[[1]])[1] == "pkg_metric_error", -1, package_riskmetric2$has_source_control[[1]][1]), "',",
                "'", format(round(package_riskmetric1$export_help[1],2)), ",",  ifelse(class(package_riskmetric2$export_help[[1]])[1] == "pkg_metric_error", -1, package_riskmetric2$export_help[[1]][1]), "',",
                "'", format(round(package_riskmetric1$bugs_status[1],2)), ",",  ifelse(class(package_riskmetric2$bugs_status[[1]])[1] == "pkg_metric_error", -1, package_riskmetric2$bugs_status[[1]][1]), "'", ")"))
  
  db_ins(
    paste0( "INSERT INTO TestMetrics values(",
            "'", package_name, "',",  
            "'", package_version, "',",
            "'", format(round(package_riskmetric1$covr_coverage[1], 2)),",", ifelse(class(package_riskmetric2$covr_coverage[[1]])[1] == "pkg_metric_error", -1, package_riskmetric2$covr_coverage[[1]][1]), "'", ")" )
  )
  
  db_ins(paste0( "UPDATE Packageinfo SET score = '", format(round(package_riskmetric1$pkg_score[1], 2)), "'", " WHERE package = '" ,
                 package_name, "'"))
 
}  

# End of the function

# 4. Function to get community usage metrics info and upload into DB.

metric_cum_Info_upload_to_DB <- function(package_name, package_version) {
  pkg_vers_date_final<<-data.frame(matrix(ncol = 4, nrow = 0))
  time_since_first_release<<-NA
  time_since_version_release<<-NA
  downloads_1yr<<-NA
  
  tryCatch(
    expr = {
      downloads_1yr_br_i <- pkg_ref(package_name)$downloads
      downloads_1yr_br_i <- filter(downloads_1yr_br_i, months(downloads_1yr_br_i$date) != months(Sys.Date()))
      downloads_1yr_br_i$date <- paste( months(downloads_1yr_br_i$date), year(downloads_1yr_br_i$date) )
      count<-c()
      for (i in 1:length(unique(downloads_1yr_br_i$date))) {
        count_df <- filter(downloads_1yr_br_i, downloads_1yr_br_i$date == unique(downloads_1yr_br_i$date)[i])
        count[i] <- sum(count_df$count)
      }
      downloads_1yr_br <-data.frame(Month = unique(downloads_1yr_br_i$date), Downloads = count)
      downloads_1yr <- sum(downloads_1yr_br$Downloads)
      colnames(pkg_vers_date_final) <<- c("Month", "Downloads", "verRelease", "Position")
      pkg_vers_date_final <- downloads_1yr_br
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
      loggit("ERROR", paste("Error in extracting cum metric info of the package:", package_name, "info", e), app = "fileupload-webscraping")
    }
  )# End of try catch
  
  for (i in 1:nrow(pkg_vers_date_final)) {
    db_ins(paste0("INSERT INTO CommunityUsageMetrics values(",
                  "'", package_name,    "',",
                  "'", package_version, "',",
                  "'", downloads_1yr,   "',",
                  "'", pkg_vers_date_final$Month[i], "',", "'", pkg_vers_date_final$Downloads[i], "',", 
                  "'", pkg_vers_date_final$verRelease[i], "',", "'", pkg_vers_date_final$Position[i], "',",
                  "'", time_since_first_release, "',", "'", time_since_version_release, "'" , ")"))
    if(nrow(pkg_vers_date_final) == 0){
      break
    }
  }
  
}

# End of the functions