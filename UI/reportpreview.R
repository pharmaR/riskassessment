# Render Output UI for Report Preview.
output$report_preview <- renderUI({
  
  # Lets the user know that a package needs to be selected.
  if(identical(selected_pkg$name(), character(0)))
    showHelperMessage()
  
  else {
    
    fluidPage(
      
      tagList(
        br(),
        introJSUI("report_introJS"),
        h4("Report Preview", style = "text-align: center;"),
        br(), br(),
        
        div(id = "dwnld_rp",
          selectInput("report_format", "Select Format", c("html", "docx")),
          downloadButton("download_report_btn", "Download Report")
        ),
        
        br(), br(),
        
        div(id = "rep_prev",
          fluidRow(
            column(
              width = 12,
              uiOutput("pkg_overview"),
              uiOutput("decision_display"))
          ),
          
          fluidRow(
            column(width = 12,
                   viewCommentsUI("view_overall_comments_for_report"))
          ),
          
          br(), br(),
          hr(),
          fluidRow(
            column(width = 12,
                   h5("Maintenance Metrics"),
                   metricGridUI("report_mm_metricGrid"),
                   viewCommentsUI("view_mm_comments_for_report"))
          ),
          
          br(), br(),
          hr(),
          fluidRow(
            h5("Community Usage Metrics"),
            #metricGridUI("report_mm_metricGrid"),
            viewCommentsUI("view_cum_comments_for_report")
          )
        )
      )
    )
  }
})