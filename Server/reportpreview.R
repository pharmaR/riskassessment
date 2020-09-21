#####################################################################################################################
# reportpreview.R - Report Preview Source file for Server Module.
# Author: K Aravind Reddy
# Date: July 13th, 2020
# License: MIT License
#####################################################################################################################

#Start of the Render Output's'

# 0. All available packages that have been uploaded to the db in the past

# observeEvent({}) # need to observe if any new comments are pushed in realtime to db...
output$db_pkgs <- DT::renderDataTable({
  values$db_pkg_overview <- db_fun(paste0("
      SELECT case when package = '", input$select_pack,"' then 1 else 0 end as Selected
      , pi.package
      , pi.version
      , pi.score
      , pi.decision
      , c.last_comment
      FROM Packageinfo as pi
      LEFT JOIN (
        SELECT comm_id
             , max(added_on) as last_comment
        FROM Comments
        GROUP BY comm_id
      ) as c
      on c.comm_id = pi.package
      ORDER BY 1 DESC
    "
  ))
  DT::datatable(values$db_pkg_overview,
    selection = list(mode = 'multiple'), #, selected = c(1)
    extensions = "Buttons",
    colnames = c("Selected", "Package", "Version", "Score", "Decision", "Last Comment"),
    options = list(
      dom = 'Blftpr',
      pageLength = 10,
      lengthMenu = list(c(10, 50, 100, -1),c('15', '50', '100', "All")),
      columnDefs = list(list(targets = 1, visible = FALSE)),
      buttons = list(list(
        extend = "excel", 
        filename = paste("RiskAsses_PkgDB_Dwnld",str_replace_all(str_replace(Sys.time(), " ", "_"),":", "-"), sep = "_")
      ))
    )
  ) %>%
  formatStyle('Selected', target = 'row',
    backgroundColor = styleEqual(c(1), c('aquamarine'))
  )
})

# 0 (cont'd). Render Output for download handler to export the report for each .
values$cwd<-getwd()
output$dwnld_sel_db_pkgs_btn <- downloadHandler(
  filename = function() {
    paste("RiskAsses_PkgDB_Dwnld",str_replace_all(str_replace(Sys.time(), " ", "_"),":", "-"),".zip", sep = "_")
  },
  content = function(file) {
    these_pkgs <- values$db_pkg_overview %>% slice(input$db_pkgs_rows_selected)
    n_pkgs <- nrow(these_pkgs)
    req(n_pkgs > 0)
    shiny::withProgress(
      message = paste0("Downloading ",n_pkgs," Report",ifelse(n_pkgs > 1,"s","")),
      value = 0,
      max = n_pkgs + 2, # tell the progress bar the total number of events
      {
        shiny::incProgress(1)
        
        my_dir <- tempdir()
        if (input$report_formats == "html") {
          Report <- file.path(my_dir, "Report_html.Rmd")
          file.copy("Reports/Report_html.Rmd", Report, overwrite = TRUE)
        } else {
          Report <- file.path(my_dir, "Report_doc.Rmd")
          file.copy("Reports/Report_doc.Rmd", Report, overwrite = TRUE)
        }
        fs <- c()
        for (i in 1:n_pkgs) {
          # grab package name and version, then create filename and path
          this_pkg <- these_pkgs$package[i]
          this_ver <- these_pkgs$version[i]
          file_named <- paste0(this_pkg,"_",this_ver,"_Risk_Assessment.",input$report_formats)
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

# 1. Render Output to display the general information of the selected package.
output$gen_info <- renderText({
  pkg_GenInfo <-
    db_fun(
      paste0(
        "SELECT * FROM Packageinfo WHERE package ='",
        input$select_pack,
        "'"
      )
    )
  
  paste(
    "<h2><b>Package:</b> ",
    pkg_GenInfo$package,
    "</h2>",
    "<h4><b>Version: </b>",
    pkg_GenInfo$version,
    "</h4>",
    "<h4><b>Title: </b>",
    pkg_GenInfo$title,
    "</h4>",
    "<h4><b>Description:</b>",
    pkg_GenInfo$description,
    "</h4>",
    "<h4><b>Author:</b>",
    pkg_GenInfo$author,
    "</h4>",
    "<h4><b>Maintainer: </b>",
    pkg_GenInfo$maintainer,
    "<h4><b>License: </b>",
    pkg_GenInfo$license,
    "</h4>",
    "<h4><b>Published:</b>",
    pkg_GenInfo$published,
    "</h4>"
  )
})  # End of the render output for genral information.

# 2. Render Output to display the decision status of the selected pacakge.

output$decision_display <- renderText({
  if (!identical(values$selected_pkg$decision, character(0)) && values$selected_pkg$decision != "") {
    paste("<br>", "<h3>Overall risk: ", "<b>", values$selected_pkg$decision, "</b></h3>") 
  } else{
    paste("<br>", "<h3>Overall risk: Pending</h3>")
  }
})    # End of the render Text Output.

# 3. Render Output to display the overall comment of the selected package. 
output$overall_comments <- renderText({
  req(values$selected_pkg$package)
  if (values$o_comment_submitted == "yes" ||
      values$o_comment_submitted == "no") {
    values$comment_o1 <-
      db_fun(
        paste0(
          "SELECT * FROM Comments WHERE comm_id = '",
          values$selected_pkg$package,
          "' AND comment_type = 'o'"
        )
      )
    values$comment_o2 <- values$comment_o1 %>% arrange(desc(values$comment_o1$added_on))
    req(values$comment_o2$comment)
    values$o_comment_submitted <- "no"
     paste(
      "<div class='col-sm-12 comment-border-bottom single-comment-div'><i class='fa fa-user-tie fa-4x'></i><h3 class='ml-3'><b class='user-name-color'>",
      values$comment_o2$user_name,
      "(",
      values$comment_o2$user_role,
      ")",
      "</b><sub>",
      values$comment_o2$added_on,
      "</sub></h3><h4 class='ml-3 lh-4'>",
      values$comment_o2$comment,
      "</h4></div>"
    )
  }
})  # End of the render Text Output.

# 4. Render Output for download handler to export the report.
values$cwd<-getwd()
output$download_report_btn <- downloadHandler(
  filename = function() {
    paste0(input$select_pack,"_",input$select_ver,"_Risk_Assessment.", switch(input$report_format, "docx" = "docx", "html" = "html"))
  },
  content = function(file) {
    shiny::withProgress(message = paste0("Downloading ", input$dataset, " Report"),
                        value = 0,
                        {
                          shiny::incProgress(1 / 10)
                          shiny::incProgress(5 / 10)
                          if (input$report_format == "html") {
                            Report <- file.path(tempdir(), "Report_html.Rmd")
                            file.copy("Reports/Report_html.Rmd", Report, overwrite = TRUE)
                          } else {
                            Report <- file.path(tempdir(), "Report_doc.Rmd")
                            file.copy("Reports/Report_doc.Rmd", Report, overwrite = TRUE)
                          }
                          
                          rmarkdown::render(
                            Report,
                            output_file = file,
                            params = list(package = values$selected_pkg$package,
                                          version = values$selected_pkg$version,
                                          cwd = values$cwd)
                          )
                        })
  }
)  # End of the render Output for download report.

source(file.path("Server", "mm_report.R"), local = TRUE)$value
source(file.path("Server", "cum_report.R"), local = TRUE)$value
source(file.path("Server", "tm_report.R"), local = TRUE)$value

# End of the report preview Source file for Server Module.
