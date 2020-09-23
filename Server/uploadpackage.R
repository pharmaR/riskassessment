#####################################################################################################################
# uploadpackage.R - upload pacakge Source file for server Module.
# Author: K Aravind Reddy
# Date: July 13th, 2020
# License: MIT License
#####################################################################################################################


# Reactive variable to load the sample csv file into data().

data <- reactive({
  data1<-read_csv("./Data/upload_format.csv")
  data1<-data.table(data1)
  data1
})  # End of the reactive.

# Start of the observe's'

# 1. Observe to load the columns from DB into below reactive values.

observeEvent(input$total_new_undis_dup, {
  # req(input$total_new_undis_dup)
  if (input$total_new_undis_dup == "Total") {
    values$Total_New_Undis_Dup <- values$Total
  } else if (input$total_new_undis_dup == "New") {
    values$Total_New_Undis_Dup <- values$New
  } else if (input$total_new_undis_dup == "Undiscovered") {
    values$Total_New_Undis_Dup <- values$Undis
  } else if (input$total_new_undis_dup == "Duplicates") {
    values$Total_New_Undis_Dup <- values$Dup
  } 
}, ignoreInit = TRUE)  # End of the observe.

# 2. Observe to disable the input widgets while the packages uploading into DB.

observeEvent(input$uploaded_file, {
  # req(input$uploaded_file)
  values$uploaded_file_status <- file_upload_error_handling(input$uploaded_file)
  if (values$uploaded_file_status != "no_error") {
    shinyjs::hide("upload_summary_text")
    shinyjs::hide("upload_summary_select")
    shinyjs::hide("total_new_undis_dup_table")
    shinyjs::hide("dwnld_all_reports_btn")
    shinyjs::hide("all_reports_format")
    reset("uploaded_file") 
    return()
  } else{
    shinyjs::show("upload_summary_text")
    shinyjs::show("upload_summary_select")
    shinyjs::show("total_new_undis_dup_table")
    shinyjs::show("dwnld_all_reports_btn")
    shinyjs::show("all_reports_format")
  }
  file_to_read <- input$uploaded_file
  pkgs_file <-
    read.csv(file_to_read$datapath,
             sep = ",",
             stringsAsFactors = FALSE)
  names(pkgs_file) <- tolower(names(pkgs_file))
  pkgs_file$package <- trimws(pkgs_file$package)
  pkgs_file$version <- trimws(pkgs_file$version)
  
  j <- rep(FALSE, nrow(pkgs_file))
  for (i in 1:nrow(pkgs_file)) {
    if (!pkgs_file$package[i] %in% pkgs_vec) {
      message(paste("Package",pkgs_file$package[i],"not found on CRAN. Names are case-sensitive. Check your spelling."))
      j[i] <- TRUE
    }
    if (j[i] == FALSE)  {
      # vrsn_lst <- versions::available.versions(pkgs_file$package[i])
      # vrsn_vec <- unlist(vrsn_lst[[1]]$version)
      
      pkg_html <- read_html(paste0("https://github.com/cran/", pkgs_file$package[i], "/tags"))
      pkg_nodes_v <- html_nodes(pkg_html, 'h4')
      pkg_text_v <- html_text(pkg_nodes_v)
      pkg_text_v <- str_split(pkg_text_v,"\n")
      pkg_vers <- rep("", length(pkg_text_v))
      for (k in 1:length(pkg_text_v)) {
        pkg_vers[k]<-(trimws(pkg_text_v[[k]][3]))
      }
      vrsn_vec <- pkg_vers[which(!is.na(pkg_vers))] #pkg_vers[c(3:length(pkg_vers))]
      
      if (!pkgs_file$version[i] %in% vrsn_vec) {
        message(paste("Version",pkgs_file$version[i],"of ",pkgs_file$package[i],"not found on CRAN. Check your spelling."))
        j[i] <- TRUE
      }
    }
  }
  
  if (any(j)) {
    # drop the non-existent packages
    pkgs_file <- pkgs_file[-which(j),]
    # reset the row numbers
    row.names(pkgs_file) <- NULL
  }
  
  values$Total <- pkgs_file
  print(values$Total)

  # pkgs_db1 <- db_fun("SELECT package FROM Packageinfo")
  # values$Dup <- filter(values$Total, values$Total$package %in% pkgs_db1$package)
  # values$New <- filter(values$Total, !(values$Total$package %in% pkgs_db1$package))
  pkgs_db1 <- db_fun("SELECT package, version FROM Packageinfo")
  values$Dup <- filter(values$Total,   values$Total$package %in% pkgs_db1$package & values$Total$version %in% pkgs_db1$version)
  values$New <- filter(values$Total, !(values$Total$package %in% pkgs_db1$package & values$Total$version %in% pkgs_db1$version))
  
  withProgress(message = "Uploading Packages to DB:", value = 0, {
    if (nrow(values$New) != 0) {
      for (i in 1:nrow(values$New)) {
          new_package<-values$New$package[i]
          new_version<-values$New$version[i]
          get_packages_info_from_web(new_package,new_version)
          metric_mm_tm_Info_upload_to_DB(new_package,new_version)
          metric_cum_Info_upload_to_DB(new_package,new_version)
          incProgress(1 / nrow(values$New), detail = values$New[i, 1])
          Sys.sleep(0.1)
      }
    }
  })
  
  pkgs_db2 <- db_fun("SELECT package FROM Packageinfo")
  values$Undis <-
    filter(values$New,!(values$New$package %in% pkgs_db2$package))
  values$packsDB <- db_fun("SELECT package FROM Packageinfo")
  updateSelectizeInput(
    session,
    "select_pack",
    choices = c("Select", values$packsDB$package),
    selected = "Select"
  )
  
  showNotification(id = "show_notification_id", "Upload completed to DB", type = "message")
  values$upload_complete <- "upload_complete"
  loggit("INFO", paste("Summary of the uploaded file:",input$uploaded_file$name, 
                       "Total Packages:", nrow(values$Total),
                       "New Packages:", nrow(values$New),
                       "Undiscovered Packages:", nrow(values$Undis),
                       "Duplicate Packages:", nrow(values$Dup)), echo = FALSE)
}, ignoreInit = TRUE)  # End of the Observe.

# End of the observe's'.

# Start of the render Output's'.

# 1. Render Output to download the sample format dataset.

output$upload_format_download <- downloadHandler(
  filename = function() {
    paste("Upload_file_structure", ".csv", sep = "")
  },
  content = function(file) {
    write.csv(read_csv("./Data/upload_format.csv"), file, row.names = F)
  }
)  # End of the render Output.

# 2. Render Output to show the summary of the uploaded csv into application.

output$upload_summary_text <- renderText({
  if (values$upload_complete == "upload_complete") {
    paste(
      "<h3><b>Summary of:</b>",
      input$uploaded_file$name,
      "</h3>",
      "<h4>Total Packages: ",
      nrow(values$Total),
      "</h4>",
      "<h4>New Packages:",
      nrow(values$New),
      "</h4>",
      "<h4>Undiscovered Packages:",
      nrow(values$Undis),
      "</h4>",
      "<h4>Duplicate Packages:",
      nrow(values$Dup),
      "</h4>",
      "<h4><b>Note: The information extracted of the package will be always from latest version irrespective of uploaded version."
    )
  }
})  # End of the render Output.

# 3. Render Output to show the select input to select the choices to display the table.

output$upload_summary_select <- renderUI({
  if (values$upload_complete == "upload_complete") {
    removeUI(selector = "#Upload")
    selectInput(
      "total_new_undis_dup",
      "",
      choices = c("Total", "New", "Undiscovered", "Duplicates")
    )
  } 
})  # End of the render Output.

# 4. Render Output to show the data table of uploaded csv.

output$total_new_undis_dup_table <- DT::renderDataTable(
  if (values$upload_complete == "upload_complete") {
    datatable(
      values$Total_New_Undis_Dup,
      escape = FALSE,
      class = "cell-border",
      selection = 'none',
      extensions = 'Buttons',
      options = list(
        sScrollX = "100%",
        aLengthMenu = list(c(5, 10, 20, 100,-1), list('5', '10', '20', '100', 'All')),
        iDisplayLength = 5
      )
    )
  }
)  # End of the render Output 
# End of the Render Output's'.


# 5. Render Output for download handler to export the report for each .
# Data displayed: values$Total_New_Undis_Dup
# file name uplaoded: input$uploaded_file$name
# selected type: input$upload_summary_select
values$cwd<-getwd()
output$dwnld_all_reports_btn <- downloadHandler(
  filename = function() {
    # name will include the type of packages selected to display in DT
    paste0(input$total_new_undis_dup, "_",
           stringr::str_remove(input$uploaded_file$name, ".csv"),
           ".zip")
  },
  content = function(file) {
    n_pkgs <- nrow(values$Total_New_Undis_Dup)
    req(n_pkgs > 0)
    shiny::withProgress(
      message = paste0("Downloading ",n_pkgs," Report",ifelse(n_pkgs > 1,"s","")),
      value = 0,
      max = n_pkgs + 2, # tell the progress bar the total number of events
      {
        shiny::incProgress(1)
        
        my_dir <- tempdir()
        if (input$all_reports_format == "html") {
          Report <- file.path(my_dir, "Report_html.Rmd")
          file.copy("Reports/Report_html.Rmd", Report, overwrite = TRUE)
        } else {
          Report <- file.path(my_dir, "Report_doc.Rmd")
          file.copy("Reports/Report_doc.Rmd", Report, overwrite = TRUE)
        }
        fs <- c()
        for (i in 1:n_pkgs) {
          # grab package name and version, then create filename and path
          this_pkg <- values$Total_New_Undis_Dup$package[i]
          this_ver <- values$Total_New_Undis_Dup$version[i]
          file_named <- paste0(this_pkg,"_",this_ver,"_Risk_Assessment.",input$all_reports_format)
          path <- file.path(my_dir, file_named)
          # render the report, passing parameters to the rmd file
          rmarkdown::render(
            input = Report,
            output_file = path,
            params = list(package = this_pkg,
                          version = this_ver,
                          cwd = values$cwd)
          )
          fs <- c(fs, path)  # save all the 
          shiny::incProgress(1) # increment progress bar
        }
        # zip all the files up, -j retains just the files in zip file
        zip(zipfile = file, files = fs ,extras = "-j")
        shiny::incProgress(1) # increment progress bar
      })
  },
  contentType = "application/zip"
)  # End of the render Output for download report.



# Observe Event for view sample dataset button.

observeEvent(input$upload_format, {
  dataTableOutput("sampletable")
  showModal(modalDialog(
    output$sampletable <- DT::renderDataTable(
      datatable(
        data(),
        escape = FALSE,
        class = "cell-border",
        editable = FALSE,
        filter = "none",
        selection = 'none',
        extensions = 'Buttons',
        options = list(
          sScrollX = "100%",
          aLengthMenu = list(c(5, 10, 20, 100, -1), list('5', '10', '20', '100', 'All')),
          iDisplayLength = 5,
          dom = 't'
        )
      )
    ),
    downloadButton("upload_format_download", "Download", class = "btn-secondary")
  ))
})  # End of the observe event for sample button.

# End of the upload package Source file for server Module.
