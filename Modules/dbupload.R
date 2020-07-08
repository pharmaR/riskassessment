#####################################################################################################################
# dbupload.R - Uploading the CSV file into DB
# 
# Author:
# Created:
#####################################################################################################################

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
        #loggit("ERROR", paste("Error in scraping the package", package_name, "info", e), app = "fileupload-webscraping")
      }
    },
    warning = function(w) {
      # (Optional)
      # Do this if an warning is caught...
    },
    finally = {
      # (Optional)
      # Do this at the end before quitting the tryCatch structure...
    }
  )# End of try catch
}


genInfo_upload_to_DB <- function(package_name, ver, title, desc, auth, main, lis, pub) {
  tryCatch(
    expr = {
      db_fun(paste0( "INSERT INTO Packageinfo values(", "'", package_name, "',", "'", ver, "',", "'", title ,"'," , "'", desc, "',",
                     "'", main, "',", "'", auth, "',", "'", lis, "',", "'", pub, "',", "'',", "''", ")"))
      pkg_uploaded <<- "uploaded"
    },
    error = function(e) {
      #loggit("ERROR", paste("Error in uploading the package", pkg_name, "info", e), app = "fileupload-DB")
    },
    warning = function(w){
      # (Optional)
      # Do this if an warning is caught...
    },
    finally = {
      # (Optional)
      # Do this at the end before quitting the tryCatch structure...
    }
  )# End of try catch 
}


metric_mm_tm_Info_upload_to_DB <- function(package_name){
  
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
  
  package_riskmetric1$bugs_status<<-package_riskmetric1$bugs_status*100
  package_riskmetric1$export_help<<-package_riskmetric1$export_help*100
  
  # tryCatch(
  #   expr = {
  db_fun(paste0("INSERT INTO MaintenanceMetrics values(", 
                "'", package_name, "',", 
                "'", package_riskmetric1$has_vignettes[1], ",", ifelse(class(package_riskmetric2$has_vignettes[[1]])[1] == "pkg_metric_error", -1, package_riskmetric2$has_vignettes[[1]][1]), "',",
                "'", package_riskmetric1$has_news[1], ",",  ifelse(class(package_riskmetric2$has_news[[1]])[1] == "pkg_metric_error", -1, package_riskmetric2$has_news[[1]][1]), "',", 
                "'", package_riskmetric1$news_current[1], ",",  ifelse(class(package_riskmetric2$news_current[[1]])[1] == "pkg_metric_error", -1, package_riskmetric2$news_current[[1]][1]), "',",  
                "'", package_riskmetric1$has_website[1], ",",  ifelse(class(package_riskmetric2$has_website[[1]])[1] == "pkg_metric_error", -1, package_riskmetric2$has_website[[1]][1]), "',", 
                "'", package_riskmetric1$has_bug_reports_url[1], ",",  ifelse(class(package_riskmetric2$has_bug_reports_url[[1]])[1] == "pkg_metric_error", -1, package_riskmetric2$has_bug_reports_url[[1]][1]), "',",
                "'", package_riskmetric1$has_maintainer[1], ",",  ifelse(class(package_riskmetric2$has_maintainer[[1]])[1] == "pkg_metric_error", -1, package_riskmetric2$has_maintainer[[1]][1]), "',", 
                "'", package_riskmetric1$has_source_control[1], ",",  ifelse(class(package_riskmetric2$has_source_control[[1]])[1] == "pkg_metric_error", -1, package_riskmetric2$has_source_control[[1]][1]), "',",
                "'", format(round(package_riskmetric1$export_help[1],2)), ",",  ifelse(class(package_riskmetric2$export_help[[1]])[1] == "pkg_metric_error", -1, package_riskmetric2$export_help[[1]][1]), "',",
                "'", format(round(package_riskmetric1$bugs_status[1],2)), ",",  ifelse(class(package_riskmetric2$bugs_status[[1]])[1] == "pkg_metric_error", -1, package_riskmetric2$bugs_status[[1]][1]), "'", ")"))
  
  db_fun(
    paste0( "INSERT INTO TestMetrics values(",
            "'", package_name, "',",  
            "'", format(round(package_riskmetric1$covr_coverage[1], 2)),",", ifelse(class(package_riskmetric2$covr_coverage[[1]])[1] == "pkg_metric_error", -1, package_riskmetric2$covr_coverage[[1]][1]), "'", ")" )
  )
  
  db_fun(paste0( "UPDATE Packageinfo SET score = '", format(round(package_riskmetric1$pkg_score[1], 2)), "'", " WHERE package = '" ,
                 package_name, "'"))
  #   },
  # error = function(e) {
  #   #loggit("ERROR", paste("Error in uploading the package", pkg_name, "info", e), app = "fileupload-DB")
  # },
  # warning = function(w){
  #   # (Optional)
  #   # Do this if an warning is caught...
  # },
  # finally = {
  #   # (Optional)
  #   # Do this at the end before quitting the tryCatch structure...
  # }
  # )# End of try catch
}  


metric_cum_Info_upload_to_DB <- function(package_name) {
  final<<-data.frame(matrix(ncol = 4, nrow = 0))
  diff1<<-NA
  diff2<<-NA
  downloads_1yr<<-NA
  
  tryCatch(
    expr = {
      downloads_1yr_br_i <- tail(cran_stats(package_name), n = 13)
      downloads_1yr_br_i <- downloads_1yr_br_i[c(1:nrow(downloads_1yr_br_i)-1),]
      downloads_1yr_br <-data.frame(Month = c(paste(
        months(downloads_1yr_br_i$end),
        year(downloads_1yr_br_i$end)
      )), Downloads = downloads_1yr_br_i$downloads)
      downloads_1yr <- sum(downloads_1yr_br$Downloads)
      colnames(final) <<- c("Month", "Downloads", "verRelease", "Position")
      final <- downloads_1yr_br
      final$Month <- as.character(final$Month)
      final$Position <- 13
      
      a <- read_html(paste0("https://github.com/cran/", package_name, "/tags"))
      b <- html_nodes(a, 'h4')
      c <- html_text(b)
      c <- str_split(c,"\n")
      d <- c()
      for (i in 1:length(c)) { 
        d[i]<-(trimws(c[[i]][3]))
      }
      d <- d[c(3:length(d))]
      
      d1 <- d[length(d)]
      loop<-"not_started"
      while (d1 != "") {
        a1 <- read_html(paste0("https://github.com/cran/",package_name,"/tags?after=",d1))
        b1 <- html_nodes(a1, 'h4')
        c1 <- html_text(b1)
        c1 <- str_split(c1,"\n")
        d1 <- c()
        for (i in 1:length(c1)) { 
          d1[i]<-(trimws(c1[[i]][3]))
        }
        d1 <- d1[length(d1)]
        if (is.na(d1)) {
          if(loop != "looped"){
            d1 <- d[length(d)]
            e1 <- html_nodes(a, 'relative-time')
            e1 <- html_text(e1)
            f1 <- str_remove_all(e1[length(e1)], ",")
            f1 <- as.Date(f1, format = "%h %d %Y")
            diff1 <- Sys.Date() - f1
            diff1 <- floor(as.numeric(diff1 / 30))
            break
          }else{
            break 
          }
        } else{
          e1 <- html_nodes(a1, 'relative-time')
          e1 <- html_text(e1)
          f1 <- str_remove_all(e1[length(e1)], ",")
          f1 <- as.Date(f1, format = "%h %d %Y")
          diff1 <- Sys.Date() - f1
          diff1 <- floor(as.numeric(diff1 / 30))
          loop<-"looped"
        }
      }
      
      e <- html_nodes(a, 'relative-time')
      e <- html_text(e)
      f <- str_remove_all(e, ",")
      f <- as.Date(f, format = "%h %d %Y")
      diff2 <- Sys.Date() - f[1]
      diff2 <- floor(as.numeric(diff2 / 30))
      g <- months(f)
      h <- year(f)
      
      # a1<-read_html(paste0( "https://github.com/cran/dplyr/tags?after=",q[length(q)]))
      
      res <- data.frame(Version = c(d), Date = c(paste(g, h)))
      res <- res %>% map_df(rev)
      res$Date <- as.character(res$Date)
      res$Version <- as.character(res$Version)
      
      r <- filter(res, res$Date %in% final$Month)
      
      for (i in 1:length(final$Month)) {
        for (j in 1:length(r$Date)) {
          if (final$Month[i] == r$Date[j]) {
            final$verRelease[i] <- r$Version[j]
            final$Position[i] <- i - 1
            break
          } else{
            final$verRelease[i] <- NA
            final$Position[i] <- 13
          }
        }
      }
    },
    error = function(e) {
      #loggit("ERROR", paste("Error in uploading the package", package_name, "info", e), app = "fileupload-DB")
      #downloads_1yr<<-NA
      #return()
    },
    warning = function(w) {
      # (Optional)
      # Do this if an warning is caught...
    },
    finally = {
      # (Optional)
      # Do this at the end before quitting the tryCatch structure...
    }
  )# End of try catch
  
  for (i in 1:nrow(final)) {
    db_fun(paste0("INSERT INTO CommunityUsageMetrics values(",
                  "'", package_name,"',", "'", downloads_1yr, "',",
                  "'", final$Month[i], "',", "'", final$Downloads[i], "',", "'", final$verRelease[i], "',", "'", final$Position[i], "',",
                  "'", diff1, "',", "'", diff2, "'" , ")"))
    if(nrow(final)==0){
      break
    }
  }
  
}
