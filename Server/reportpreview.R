#####################################################################################################################
# reportpreview.R - Report Preview Source file for Server Module.
# 
# Author: Aravind
# Created: 02/06/2020.
#####################################################################################################################

#Start of the Render Output's'

# 1. Render Output to dispaly the general information of the selected package.
output$gen_info <- renderText({
  res4 <-
    db_fun(
      paste0(
        "SELECT * FROM Packageinfo WHERE package ='",
        input$select_pack,
        "'"
      )
    )
  
  paste(
    "<h2><b>Package:</b> ",
    res4$package,
    "</h2>",
    "<h4><b>Version: </b>",
    res4$version,
    "</h4>",
    "<h4><b>Title: </b>",
    res4$title,
    "</h4>",
    "<h4><b>Description:</b>",
    res4$description,
    "</h4>",
    "<h4><b>Author:</b>",
    res4$author,
    "</h4>",
    "<h4><b>Maintainer: </b>",
    res4$maintainer,
    "<h4><b>License: </b>",
    res4$license,
    "</h4>",
    "<h4><b>Published:</b>",
    res4$published,
    "</h4>"
  )
})  # End of the render output for genral information.

# 2. Render Output to display the decision status of the selected pacakge.

output$decision_display <- renderText({
  if (!identical(values$selected_pkg$decision, character(0)) && values$selected_pkg$decision != "") {
    paste("<br>", "<h3>Overall Risk: ", "<b>", values$selected_pkg$decision, "</b></h3>") 
  } else{
    paste("<br>", "<h3>Overall Risk: Pending</h3>")
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
    paste0("Report.", switch(input$report_format, "docx" = "docx", "html" = "html"))
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
                            params = list(package = values$selected_pkg$package, cwd = values$cwd)
                          )
                        })
  }
)  # End of the render Output for download report.

source(file.path("Server", "mm_report.R"), local = TRUE)$value
source(file.path("Server", "cum_report.R"), local = TRUE)$value
source(file.path("Server", "tm_report.R"), local = TRUE)$value

# End of the report preview Source file for Server Module.
