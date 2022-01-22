# Render Output UI for Community Usage Metrics.
output$community_usage_metrics <- renderUI({
  Sys.sleep(0.1)
  if (!is.null(values$packsDB$name) &&
      !identical(values$packsDB$name, character(0))) {
    if (input$select_pack != "Select") {
      shiny::tagList(
        br(),
        div(class = "row col-sm-12 u_p_heading_row",
            actionBttn("help_cum", "Need help?", color = "primary",
                       icon = icon("far fa-star"),
                       block = FALSE, style = "simple", size = "sm")),
        br(), br(),
        fluidRow(
          div(style = "height:25px;"),
          class = "c_u_m_row_main",
          fluidRow(
            id = "cum_infoboxes",
            class = "c_u_m_row_1",
            infoBoxOutput("time_since_first_release", width = 4),  # Info box to show the time since First release.
            infoBoxOutput("time_since_version_release", width = 4),  # Info box to show the time since version release.
            infoBoxOutput("dwnlds_last_yr", width = 4)  # Info box to show the total # of Downloads in the last year.
          ),
          fluidRow(
            id = "cum_plot",
            class = "c_u_m_row_graph",
            column(width = 1, ),
            column(width = 10,
                   class = "w-90",
                   plotly::plotlyOutput("no_of_downloads")
                   ),
            column(width = 1, )
          ),
          
          fluidRow(
            id = "cum_add_comment",
            class = "c_u_m_row_comments_box",
            column(
              
              width = 8,
              class = "mb-4 label-float-left",
              # Text box to leave community usage metrics comments.
              textAreaInput(
                "cum_comment",
                h3(tags$b("Leave Your Comment for Community Usage Metrics:")),
                width = "100%",
                rows = 4,
                placeholder = paste("Commenting as", values$name, "(", values$role, ")")
              ) %>%
                shiny::tagAppendAttributes(style = 'width: 100%;'),
              # Action button to submit the comments.
              actionButton("submit_cum_comment", class = "submit_cum_comment_class btn-secondary", "Submit")
            )
          ),
          fluidRow(
            id = "cum_prev_comments",
            class = "c_u_m_row_comments",
            column(
              width = 12,
              align = "left",
              h3(tags$b(paste0('Comments(',nrow(values$comment_cum2),'):'))),
              htmlOutput("cum_commented")  # html output to show the comments on applicaiton.
            ))
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
  # Show the upload a list of R packages message if application not loaded the pacakges from DB.
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


# End of the Community Usage Metrics UI source file.
