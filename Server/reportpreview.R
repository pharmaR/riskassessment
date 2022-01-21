# Implement the intro logic. Sidebar steps are listed in global.r
# this dataset is also static... perhaps it should be sourced from global.r?
rp_steps <- reactive(
  data.frame(
    # Note that we access chooseCSVtext with '.' instead of '#', because we track its class and not its id.
    element = c( "#dwnld_rp", "#rep_prev"),
    intro = c(
      "Select file output type for report seen below and download for later use",
      "The current assessment of this package including your comments and overall decision have been collected from the other tabs to prepare the following report for convenience."
    ),
    position = c("left", "top")
  )
)

# Start introjs when help button is pressed.
observeEvent(input$help_rp,
             introjs(session,
                     options = list(
                       steps = 
                         rp_steps() %>%
                         union(sidebar_steps),
                       "nextLabel" = "Next",
                       "prevLabel" = "Previous",
                       "skipLabel" = "Close")))


# Display general information of the selected package.
output$gen_info <- renderText({
  pkg_GenInfo <-
    db_fun(
      paste0(
        "SELECT * FROM package WHERE name ='",
        input$select_pack,
        "'"
      )
    )
  
  paste(
    "<h2><b>Package:</b> ",
    pkg_GenInfo$name,
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
})


# Display the decision status of the selected pacakge.
output$decision_display <- renderText({
  if (!identical(values$selected_pkg$decision, character(0)) && values$selected_pkg$decision != "") {
    paste("<br>", "<h3>Overall risk: ", "<b>", values$selected_pkg$decision, "</b></h3>") 
  } else{
    paste("<br>", "<h3>Overall risk: Pending</h3>")
  }
})    # End of the render Text Output.

# Display the overall comment of the selected package. 
output$overall_comments <- renderText({
  req(values$selected_pkg$name)
  if (values$o_comment_submitted == "yes" ||
      values$o_comment_submitted == "no") {
    values$comment_o1 <-
      db_fun(
        paste0(
          "SELECT * FROM Comments WHERE comm_id = '",
          values$selected_pkg$name,
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
})

# Create report.
values$cwd <- getwd()
output$download_report_btn <- downloadHandler(
  filename = function() {
    paste0(input$select_pack, "_", input$select_ver, "_Risk_Assessment.",
           switch(input$report_format, "docx" = "docx", "html" = "html"))
  },
  content = function(file) {
    shiny::withProgress(
      message = paste0("Downloading ", input$dataset, " Report"),
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
          params = list(package = values$selected_pkg$name,
                        riskmetric_version = packageVersion("riskmetric"),
                        cwd = values$cwd,
                        username = values$name,
                        user_role = values$role)
        )
      })
  }
)

source(file.path("Server", "mm_report.R"), local = TRUE)$value
source(file.path("Server", "cum_report.R"), local = TRUE)$value
# source(file.path("Server", "tm_report.R"), local = TRUE)$value
