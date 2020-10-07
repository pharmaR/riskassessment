#####################################################################################################################
# dbupload.R - Uploading the general and metric info of the package into DB
# Author: K Aravind Reddy
# Date: July 13th, 2020
# License: MIT License
#####################################################################################################################


# Start of the functions

# 1. Function to get the package general information from CRAN/local.
get_packages_info_from_web <- function(package_name, package_ver) {
  tryCatch(
    expr = {
      info <- packinfo(package_name, package_ver)
      genInfo_upload_to_DB(package_name, info$ver, info$title, info$desc, info$auth, info$main, info$lis, info$pub)   
      return("")
    },
    error = function(e) {
      loggit("ERROR", paste("Error in extracting general info of the package", package_name, "info", e), app = "using packinfo function")
      return("ERROR")
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
  total_downloads<<-NA
  
  tryCatch(
    expr = {
      
      vrsns_lst <- versions::available.versions(package_name) 
      vrsns_df  <- as.data.frame(vrsns_lst[[1]]) %>% 
        mutate(startdt = ymd(floor_date(as.Date(date), unit="month"))) %>% select(-available)
      vrsns_vec <- as.vector(vrsns_df[,"version"])
      
      ith <- which(vrsns_vec %in% package_version)
      nr <- nrow(vrsns_df)
      to_date <- as.Date(vrsns_df[ith,"date"])
      
      # go back 1-year, or the earliest release date, whichever comes first
      back1yr <- max(to_date - 365, as.Date(vrsns_df[nr,"date"]))
      
      downlds <- cranlogs::cran_downloads(package_name, from=back1yr, to=to_date)
      
      downlds_by_monyr <- downlds %>% 
        mutate(monthyear = ymd(floor_date(date, unit="month"))) %>% 
        mutate(nextmon   = ymd(ceiling_date(date, unit="month"))) %>%
        group_by(monthyear, nextmon) %>%
        summarise(Downloads = sum(count), .groups = 'drop') 
      
      onerow <- downlds_by_monyr %>% filter(monthyear >= back1yr) %>% slice_min(order_by = monthyear)
      fr_date <- onerow[, "monthyear"]
      
      total_downloads <- summarise(downlds, sum(count))
      
      # slot the version and date into the correct month/year, create Month and Position
      downlds_data <- left_join(downlds_by_monyr, vrsns_df, by = c("monthyear" = "startdt") ) 
      
      # add +1 to include endpoints here
      time_since_first_release   <- floor(as.numeric((as.Date(vrsns_df[ith,"date"]) - as.Date(vrsns_df[nr,"date"]))) / (365.25/12) + 1 ) 
      time_since_version_release <- floor(as.numeric((Sys.Date() - as.Date(vrsns_df[ith,"date"]))) / (365.25/12) )
      
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
                  "'", package_name,"',", 
                  "'", package_version, "',", 
                  "'", all_of(total_downloads), "',",
                  "'", pkg_vers_date_final$Month[i], "',", 
                  "'", pkg_vers_date_final$Downloads[i], "',", 
                  "'", pkg_vers_date_final$version[i], "',", 
                  "'", pkg_vers_date_final$Position[i], "',",
                  "'", time_since_first_release, "',", 
                  "'", time_since_version_release, "'" , ")"))
    if(nrow(pkg_vers_date_final) == 0){
      break
    }
  }
}

# End of the functions