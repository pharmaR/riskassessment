#' UI for 'Report Preview' module
#' 
#' @param id a module id name
#' 
#' 
reportPreviewUI <- function(id) {
  uiOutput(NS(id, "reportPreview_ui"))
}

#' Server logic for 'Report Preview' module
#'
#' @param id a module id name
#' @param selected_pkg placeholder
#' @param maint_metrics placeholder
#' @param com_metrics placeholder
#' @param com_metrics_raw placeholder
#' @param mm_comments placeholder
#' @param cm_comments placeholder
#' @param downloads_plot_data placeholder
#' @param user placeholder
#' @param app_version placeholder
#' @param metric_weights placeholder
#' 
#' 
#' @import dplyr
#' @importFrom rmarkdown render
#' @importFrom plotly plotlyOutput renderPlotly
#' @importFrom DT dataTableOutput renderDataTable
#' @importFrom glue glue
#' 
reportPreviewServer <- function(id, selected_pkg, maint_metrics, com_metrics,
                                com_metrics_raw, mm_comments, cm_comments,
                                downloads_plot_data, user, app_version,
                                metric_weights) {
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
                selectInput(NS(id, "report_formats"), "Select Format", c("html", "docx", "pdf")),
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
                  column(width = 12, uiOutput(NS(id, 'communityMetrics_ui')))
                ),
                br(), br(),
                hr(),
                fluidRow(
                  column(width = 12,
                         h5("About Report",
                            style = "text-align: center; padding-bottom: 50px;"),
                         fluidRow(column(width = 12,
                                         uiOutput(NS(id, 'about_report')),
                                         h5('Weights Table:'),
                                         DT::dataTableOutput(NS(id, 'weights_table'))
                         )))
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
      
      get_overall_comments(selected_pkg$name())
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
    
    output$communityMetrics_ui <- renderUI({
      
      vect <- dbSelect("select distinct id from community_usage_metrics") %>% dplyr::pull()
      
      if(!selected_pkg$name() %in% vect) {
        tagList(
          h5("Community Usage Metrics",
             style = "text-align: center;"),
        showHelperMessage(message = glue::glue("Community Usage Metrics not avaiable for ", {selected_pkg$name()} ))
        )
      } else {
        tagList(
          h5("Community Usage Metrics",
             style = "text-align: center; padding-bottom: 50px;"),
          metricGridUI(NS(id, 'cm_metricGrid')),
          div(id = "cum_plot", fluidRow(
            column(width = 12, style = 'padding-left: 20px; padding-right: 20px;',
                   plotly::plotlyOutput(NS(id, "downloads_plot"), height = "500px")))),
          viewCommentsUI(NS(id, 'cm_comments'))
        )
      }
      
    })
    
    
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
      req(selected_pkg$name())
      
      tagList(
        h5('Risk Score:'),
        selected_pkg$score(),
        h5('Overall risk:'),
        ifelse(selected_pkg$decision() == '', 
               'Pending',
               selected_pkg$decision()))
    })
    
    # Display general information about report.
    output$about_report <- renderUI({
      req(selected_pkg$name())
      
      tagList(
        h5('Risk Assessment App Version:'), app_version,
        h5('riskmetric Version:'), paste0(packageVersion("riskmetric")),
        h5('Generated on:'), format(Sys.time(), usetz = TRUE)
      )
    })

    # Display the metric weights.
    output$weights_table <- DT::renderDataTable({
      req(selected_pkg$name())
      
      metric_weights()
      
    }, options = list(searching = FALSE, pageLength = 15, lengthChange = FALSE,
                      info = FALSE))
    
    # Create report.
    output$download_report <- downloadHandler(
      filename = function() {
        glue::glue('{selected_pkg$name()}_{selected_pkg$version()}_Risk_Assessment.',
             "{switch(input$report_formats, docx = 'docx', html = 'html', pdf = 'pdf')}")
      },
      content = function(file) {
        shiny::withProgress(
          message = glue::glue('Downloading Report: {selected_pkg$name()}'),
          value = 0,
          {
            shiny::incProgress(1 / 10)
            shiny::incProgress(5 / 10)
            
            report <- ''
            my_tempdir <- tempdir()
            
            if (input$report_formats == "html") {
              # Temporary warning: make sure user has a functional version of fontawesome while
              # this bug still exists in v0.4.0: https://github.com/rstudio/fontawesome/issues/99
              fa_v <- packageVersion("fontawesome")
              if(fa_v != '0.3.0') warning(glue::glue("HTML reports may require fontawesome 0.3.0 to render. You currently have v{fa_v} installed. If the report download failed, please install correct version using code: remotes::install_version('fontawesome', version = '0.3.0', repos = 'http://cran.us.r-project.org')"))
            
              report <- file.path('inst/app/www', 'reportHtml.Rmd')
            }
            else if (input$report_formats == "docx") {
              report <- file.path(my_tempdir, "reportDocx.Rmd")
              if (!dir.exists(file.path(my_tempdir, "images")))
                dir.create(file.path(my_tempdir, "images"))
              file.copy(file.path('inst/app/www', 'reportDocx.Rmd'),
                        report, overwrite = TRUE)
              file.copy(file.path('inst/app/www', 'read_html.lua'),
                        file.path(my_tempdir, "read_html.lua"),
                        overwrite = TRUE)
              file.copy(file.path('inst/app/www', 'images', 'user-tie.png'),
                        file.path(my_tempdir, "images", "user-tie.png"),
                        overwrite = TRUE)
              file.copy(file.path('inst/app/www', 'images', 'user-shield.png'),
                        file.path(my_tempdir, "images", "user-shield.png"),
                        overwrite = TRUE)
              file.copy(file.path('inst/app/www', 'images', 'calendar-alt.png'),
                        file.path(my_tempdir, "images", "calendar-alt.png"),
                        overwrite = TRUE)
            } else {
              report <- file.path(my_tempdir, "reportPdf.Rmd")
              if (!dir.exists(file.path(my_tempdir, "images")))
                dir.create(file.path(my_tempdir, "images"))
              file.copy(file.path('inst/app/www', 'reportPdf.Rmd'),
                        report, overwrite = TRUE)
              file.copy(file.path('inst/app/www', 'read_html.lua'),
                        file.path(my_tempdir, "read_html.lua"),
                        overwrite = TRUE)
              file.copy(file.path('inst/app/www', 'images', 'user-tie.png'),
                        file.path(my_tempdir, "images", "user-tie.png"),
                        overwrite = TRUE)
              file.copy(file.path('inst/app/www', 'images', 'user-shield.png'),
                        file.path(my_tempdir, "images", "user-shield.png"),
                        overwrite = TRUE)
              file.copy(file.path('inst/app/www', 'images', 'calendar-alt.png'),
                        file.path(my_tempdir, "images", "calendar-alt.png"),
                        overwrite = TRUE)
            }
            
            # Collect info about package.
            pkg_list <- list(
              id = selected_pkg$id(),
              name = selected_pkg$name(),
              version = selected_pkg$version(),
              title = selected_pkg$title(),
              decision = selected_pkg$decision(),
              description = selected_pkg$description(),
              author = selected_pkg$author(),
              maintainer = selected_pkg$maintainer(),
              license = selected_pkg$license(),
              published = selected_pkg$published(),
              score = selected_pkg$score()
            )
            
            rmarkdown::render(
              report,
              output_file = file,
              params = list(pkg = pkg_list,
                            riskmetric_version = paste0(packageVersion("riskmetric")),
                            app_version = app_version,
                            metric_weights = metric_weights(),
                            user_name = user$name,
                            user_role = user$role,
                            overall_comments = overall_comments(),
                            mm_comments = mm_comments(),
                            cm_comments = cm_comments(),
                            maint_metrics = maint_metrics(),
                            com_metrics = com_metrics(),
                            com_metrics_raw = com_metrics_raw(), # used for word doc
                            downloads_plot_data = downloads_plot_data()
              ),
              envir = new.env(parent = globalenv())
            )
          })
      }
    )
  })
}
