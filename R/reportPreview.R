reportPreviewUI <- function(id) {
  uiOutput(NS(id, "reportPreview_ui"))
}

reportPreviewServer <- function(id, selected_pkg, maint_metrics, com_metrics,
                                mm_comments, cm_comments,
                                downloads_plot_data, user) {
  moduleServer(id, function(input, output, session) {
    
    # IntroJS.
    introJSServer(id = "introJS", text = rp_steps)
    
    # Render Output UI for Report Preview.
    output$reportPreview_ui <- renderUI({
      
      # Lets the user know that a package needs to be selected.
      if(identical(selected_pkg$name(), character(0)))
        showHelperMessage()
      
      else {
        
        fluidPage(
          tagList(
            br(),
            introJSUI(NS(id, "introJS")),
            h4("Report Preview", style = "text-align: center;"),
            br(), br(),
            
            div(id = "dwnld_rp",
                selectInput(NS(id, "report_format"), "Select Format", c("html", "docx")),
                downloadButton(NS(id, 'download_report'), "Download Report")
            ),
            
            br(), br(),
            
            div(id = "rep_prev",
                fluidRow(
                  column(
                    width = 12,
                    uiOutput(NS(id, "pkg_overview")),
                    uiOutput(NS(id, "decision_display")))
                ),
                
                fluidRow(
                  column(width = 12, viewCommentsUI(NS(id, 'overall_comments')))
                ),
                
                br(), br(),
                hr(),
                fluidRow(
                  column(width = 12,
                         h5("Maintenance Metrics",
                            style = "text-align: center; padding-bottom: 50px;"),
                         metricGridUI(session$ns('mm_metricGrid')),
                         viewCommentsUI(NS(id, 'mm_comments')))
                ),
                
                br(), br(),
                hr(),
                fluidRow(
                  column(width = 12,
                         h5("Community Usage Metrics",
                            style = "text-align: center; padding-bottom: 50px;"),
                         metricGridUI(NS(id, 'cm_metricGrid')),
                         div(id = "cum_plot", fluidRow(
                           column(width = 12, style = 'padding-left: 20px; padding-right: 20px;',
                                  plotlyOutput(NS(id, "downloads_plot"), height = "500px")))),
                         viewCommentsUI(NS(id, 'cm_comments')))
                )
            )
          )
        )
      }
    })
    
    output$downloads_plot <- plotly::renderPlotly({
      downloads_plot_data()
    })
    
    overall_comments <- reactive({
      selected_pkg$overall_comment_added()
      
      dbSelect(glue(
        "SELECT * FROM comments
        WHERE comment_type = 'o' AND id = '{selected_pkg$name()}'"))
    })
    
    # View comments.
    viewCommentsServer(id = 'overall_comments',
                       comments = overall_comments,
                       pkg_name = selected_pkg$name,
                       label = 'Overall Comments')
    
    # View comments.
    viewCommentsServer(id = "mm_comments",
                       comments = mm_comments,
                       pkg_name = selected_pkg$name,
                       label = 'Maintainance Metrics Comments')
    
    # View comments.
    viewCommentsServer(id = 'cm_comments',
                       comments = cm_comments,
                       pkg_name = selected_pkg$name,
                       label = 'Community Usage Metrics Comments')
    
    # Maintenance metrics cards.
    metricGridServer("mm_metricGrid", metrics = maint_metrics)
    
    # Community usage metrics cards.
    metricGridServer("cm_metricGrid", metrics = com_metrics)
    
    
    # Display general information of the selected package.
    output$pkg_overview <- renderUI({
      req(selected_pkg$name())

      tagList(
        h5('Package:'), selected_pkg$name(),
        h5('Version:'), selected_pkg$version(),
        h5('Title:'), selected_pkg$title(),
        h5('Description:'), selected_pkg$description(),
        h5('Author:'), selected_pkg$author(),
        h5('Maintainer:'), selected_pkg$maintainer(),
        h5('License:'), selected_pkg$license(),
        h5('Published:'), selected_pkg$published()
      )
    })
    
    # Display the decision status of the selected package.
    output$decision_display <- renderUI({
      tagList(
        h5('Overall risk:'),
        ifelse(selected_pkg$decision() == '', 
               'Pending',
               selected_pkg$decision()))
    })
    
    # Create report.
    output$download_report <- downloadHandler(
      filename = function() {
        glue("{selected_pkg$name()_{selected_pkg$version()}_Risk_Assessment.
               {switch(input$report_format, 'docx = 'docx, html = 'html)}")
      },
      content = function(file) {
        shiny::withProgress(
          message = glue('Downloading {input$dataset} Report'),
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
              params = list(package = selected_pkg$name(),
                            riskmetric_version = packageVersion("riskmetric"),
                            cwd = getwd(),
                            username = user$name,
                            user_role = user$role)
            )
          })
      }
    )
  })
}