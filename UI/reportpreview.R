# Render Output UI for Report Preview.
output$report_preview <- renderUI({
  req(selected_pkg$name())
  
  fluidPage(
    
    if(selected_pkg$name() == "-")
      showSelectPackageMessage()
    
    else {
      tagList(
        br(),
        introJSUI("report_introJS"),
        h4("Report Preview", style = "text-align: center;"),
        br(), br(),
        
        fluidRow(column(width = 12, selectInput("report_format", "Select Format", c("html", "docx")))),
        fluidRow(column(width = 12, downloadButton("download_report_btn", "Download Report"))),
        
        fluidRow(
          column(
            width = 12,
            uiOutput("pkg_overview"),
            uiOutput("decision_display"))
        ),
        
        fluidRow(
          column(width = 12,
                 h5(glue('Overall Comments ({nrow(values$comment_o2)}):')),
                 viewCommentsUI("view_overall_comments_for_report"))
        ),
        fluidRow(
          h5("Maintenance Metrics"),
          metricGridUI("report_mm_metricGrid"),
          viewCommentsUI("view_mm_comments_for_report")
        ),
        
        fluidRow(
          h5("Community Usage Metrics"),
          metricGridUI("report_mm_metricGrid"),
          viewCommentsUI("view_cum_comments_for_report")
        )
      )
    }
  )
})