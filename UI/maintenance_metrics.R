#####################################################################################################################
# maintenance_metrics.R - Maintenance Metrics to show the info box's to show the information and multiple comments 
#                         for users and display the comments by users.
# Author: K Aravind Reddy
# Date: July 13th, 2020
# License: MIT License
#####################################################################################################################

# Start of the Maintenance_Metrics Source file for UI Module.

# Render Output UI for Maintenance Metrics.

output$maintenance_metrics <- renderUI({
  Sys.sleep(0.1)
  if (!is.null(values$packsDB$name) &&
      !identical(values$packsDB$name, character(0))) {
    if (input$select_pack != "Select") {
      shiny::tagList(
        br(),
        div(class = "row col-sm-12 u_p_heading_row",
                 shinyWidgets::actionBttn("help_mm", "Need help?", color = "primary",
                   icon = icon("far fa-star"),
                   block = FALSE, style = "simple", size = "sm")),
        br(), br(),
        fluidRow(
          div(style = "height:25px;"),
          class = "mm-main-row",
          div(id = "mm_infoboxes", 
              fluidRow(
              class = "mm-row-1",
              shinydashboard::infoBoxOutput("has_vignettes"),  # Info box for 'has_vignettes' metric.
              shinydashboard::infoBoxOutput("has_website"),  # Info box for 'has_website' metric.
              shinydashboard::infoBoxOutput("has_news"),  # Info box for 'has_news' metric.
            ),
            fluidRow(
              class = "mm-row-2",
              shinydashboard::infoBoxOutput("news_current"),  # Info box for 'news_current' metric.
              shinydashboard::infoBoxOutput("has_bug_reports_url"),  # Info box for 'has_bug_reports_url' metric.
              shinydashboard::infoBoxOutput("bugs_status"),  # Info box for 'bugs_status' metric.
            ),
            fluidRow(
              class = "mm-row-3",
              shinydashboard::infoBoxOutput("export_help"),  # Info box for 'export_help' metric.
              shinydashboard::infoBoxOutput("has_source_control"),  # Info box for 'has_source_control' metric.
              shinydashboard::infoBoxOutput("has_maintainer"),  # Info box for 'has_maintainer' metric.
            )
          ),
          fluidRow(
            id = "mm_add_comment",
            class = "mm-row-comments-box",
            column(
              width = 8,
              class = "mb-4 label-float-left",
              # Text input box to leave the Maintenance Metrics Comments.
              textAreaInput(
                "mm_comment",
                h3(tags$b("Leave Your Comment for Maintenance Metrics:")),
                width = "100%",
                rows = 4,
                placeholder = paste("Commenting as", values$name, "(", values$role, ")")
              ) %>%
                shiny::tagAppendAttributes(style = 'width: 100%;'),
              # Action button to submit the comment.
              actionButton("submit_mm_comment", class = "submit_mm_comment_class btn-secondary", "Submit")
            )
          ),
          fluidRow(
            id = "mm_prev_comments",
            class = "mm-row-comments",
            column(
              width = 12,
              align = "left",
              h3(tags$b(paste0('Comments(',nrow(values$comment_mm2),'):'))),
              htmlOutput("mm_commented")  # html output to show the comments on application.
            )
          )
        )
      )
    } 
    # Show the select the package message if user not selected any package from dropdown in the application. 
    
    else{
      fluidRow(
        div(style = "height:150px;"),
        class = "",
        id = "Upload_mm",
        column(
          width = 12,
          align = "center",
          class = "",
          h1("Please select a package")
        )
      )
    }
  }
  # Show the upload a list of R packages message if application not loaded the packages from DB.
  
  else{
    fluidRow(
      div(style = "height:150px;"),
      class = "",
      id = "Upload",
      column(
        width = 12,
        align = "center",
        class = "",
        h1("Please upload a list of R packages to proceed")
      )
    )
  }
})

# End of the Maintenance_Metrics Source file for UI Module.
