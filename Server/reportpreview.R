#####################################################################################################################
# reportpreview.R - Report Preview Source file for Server Module.
# Author: K Aravind Reddy
# Date: July 13th, 2020
# License: MIT License
#####################################################################################################################

#Start of the Render Output's'

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
  
  riskinfo <- package_info("riskmetric", dependencies = FALSE)
  
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
    "</h4>",
    "<br><h3><b>riskmetric version:</b>",
    riskinfo$loadedversion,
    "</h3>"
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
    values$comment_o2 <- sel_cmts(input$select_pack, "o")
    req(values$comment_o2$comment)
    values$o_comment_submitted <- "no"
    dsp_cmts(values$comment_o2)
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
