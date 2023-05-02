#' UI for 'Report Preview' module
#' 
#' @param id a module id name
#' @keywords internal
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
#' @importFrom rlang is_empty
#' @importFrom shinyjs enable disable show hide disabled
#' @keywords internal
#' 
reportPreviewServer <- function(id, selected_pkg, maint_metrics, com_metrics,
                                com_metrics_raw, mm_comments, cm_comments,
                                downloads_plot_data, user, app_version,
                                metric_weights) {
  moduleServer(id, function(input, output, session) {
    
    # IntroJS.
    introJSServer(id = "introJS", text = reactive(rp_steps), user)

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
            h4("Build Report", style = "text-align: center;"),
            br(), br(),
            
            div(id = "dwnld_rp",
              h5("Report Configurations"),
              br(),
              fluidRow(
                column(5, div(align = 'center',
                  mod_downloadHandler_filetype_ui(NS(id, "downloadHandler")),
                  mod_downloadHandler_button_ui(NS(id, "downloadHandler"), multiple = FALSE)
                )),
                column(7, 
                   mod_downloadHandler_include_ui(NS(id, "downloadHandler"))
                   # div(
                   #   strong(p("Elements to include:")),
                   #   div(align = 'left', class = 'twocol', style = 'margin-top: 0px;',
                   #       # checkboxGroupInput(
                   #       shinyWidgets::prettyCheckboxGroup(
                   #         NS(id, "report_includes"), label = NULL, inline = FALSE,
                   #         choices = report_include_choices,
                   #         selected = if(isolate(input$report_includes) != report_include_choices)
                   #                       isolate(input$report_includes) else report_include_choices)
                   #   )
                   # )
                 )
              )
            ),
            
            br(), br(),
            
            div(id = NS(id, "pkg-summary-grp"),
              # Compose pkg summary - either disabled, enabled, or pre-populated
              uiOutput(NS(id, "pkg_summary_ui")),
              
              # Submit or Edit Summary for selected Package.
              uiOutput(NS(id, "submit_edit_pkg_summary_ui")),
            ),
            
            br(), br(), 
            
            div(id = "rep_prev", style = "border: 3px solid; padding: 30px; box-shadow: 5px 10px 8px #888888;",
                br(),
                
                HTML("<span class='h2 txtasis'>R Package Risk Assessment  </span><br>"),
                HTML(glue::glue("<span class='h4 txtasis'>Report for Package: {selected_pkg$name()}</span><br>")),
                # if("Report Author" %in% report_includes()) 
                  HTML(glue::glue("<span class='h4 txtasis'>Author (Role): {user$name} ({user$role})</span><br>")),
                # if("Report Date" %in% report_includes()) 
                  HTML(glue::glue("<span class='h4 txtasis'>Report Date: {format(Sys.time(), '%B %d, %Y')}</span><br>")),
                
                br(),
                
                fluidRow(
                  column(
                    width = 12,
                    h5('General Information'),
                    uiOutput(NS(id, "pkg_overview")),
                    uiOutput(NS(id, "decision_display")))
                ),
                
                fluidRow(
                  column(width = 12, viewCommentsUI(NS(id, 'overall_comments')))
                ),
                fluidRow(
                  column(width = 12, viewCommentsUI(NS(id, 'pkg_summary')))
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
    # call this somewhere
    # report_includes <- mod_downloadHandler_include_server("downloadHandler")
    # 
    # observe({
    #   print("#####################################")
    #   print("report_includes():")
    #   print(report_includes())
    # })
    
    observeEvent(input$edit_pkg_summary, {
      shinyjs::enable("pkg_summary")
      output$submit_edit_pkg_summary_ui <- renderUI(actionButton(NS(id, "submit_pkg_summary"),"Submit Summary"))
    })
    

    # Display/update overall comments for selected package/version.
    # observeEvent(selected_pkg$version(), {
    observe({
      # req(selected_pkg$name())
      req(selected_pkg$version())
      
      # If no package/version is selected, then clear comments.
      if(selected_pkg$name() == "-" || selected_pkg$version() == "-"){
        shinyjs::disabled(updateTextAreaInput(session, "pkg_summary",
                            placeholder = 'Please select a package and a version.'))
      }
      else {
        # Display package comments if a package and version are selected.
        summary <- get_pkg_summary(selected_pkg$name())$comment 

        # If no summary, enable text box and submit button
        if(rlang::is_empty(summary)) {
          output$pkg_summary_ui <- renderUI({
            textAreaInput(NS(id, "pkg_summary"),h5("Write Package Summary"),
              rows = 8, width = "100%", value = "", placeholder = "Write review here."
            )
          })
          output$submit_edit_pkg_summary_ui <- renderUI(actionButton(NS(id, "submit_pkg_summary"),"Submit Summary"))
        } else { # summary exists, so disable text box and show edit button
          output$pkg_summary_ui <- renderUI({
            shinyjs::disabled(textAreaInput(NS(id, "pkg_summary"), h5("Write Package Summary"),
              rows = 8, width = "100%", value = summary
            ))
          })
          output$submit_edit_pkg_summary_ui <- renderUI(actionButton(NS(id, "edit_pkg_summary"),"Edit Summary"))
        }
      }
    })
    
    # Update db if comment is submitted.
    observeEvent(input$submit_pkg_summary, {
      current_summary <- trimws(input$pkg_summary)
      if(current_summary == "")
        validate("Please write a package summary.")
      
      req(selected_pkg$name())

      previous_summary <- get_pkg_summary(selected_pkg$name())
      if (nrow(previous_summary) > 0) { # not first summary!
        showModal(modalDialog(
          title = h2("Update Summary"),
          h3("Do you want to update your previous summary?"),
          br(),
          HTML("Yes - Overwrites the previous summary.<br>
               Keep Editing - Go back to editing the summary."
          ),
          footer = tagList(
            actionButton(NS(id, "submit_pkg_summary_yes"), "Yes"),
            actionButton(NS(id, "submit_pkg_summary_edit"), "Keep Editing")
          )
        ))
      } else { # first summary!
        comment <- stringr::str_replace_all(current_summary, "'", "''")
        dbUpdate(glue::glue(
          "INSERT INTO comments
          VALUES ('{selected_pkg$name()}', '{user$name}', '{user$role}',
          '{comment}', 's', '{getTimeStamp()}')"))
        # updateTextAreaInput(session, "pkg_summary", value = "",
        #                     placeholder = glue::glue('Current Summary: \n{current_summary}'))
        showModal(modalDialog(
          title = h2("Summary Submitted"),
          br(),
          h5(strong("Current Summary:")),
          p(current_summary),
          easyClose = TRUE
        ))
        
        shinyjs::disable("pkg_summary")
        output$submit_edit_pkg_summary_ui <- renderUI(actionButton(NS(id, "edit_pkg_summary"),"Edit Summary"))

      }
    })
    
    # if yes, insert summary into comments table
    observeEvent(input$submit_pkg_summary_yes, {
      
      req(selected_pkg$name())
      
      comment <- stringr::str_replace_all(input$pkg_summary, "'", "''")
      
      dbUpdate(
        glue::glue(
          "UPDATE comments
          SET comment = '{comment}', added_on = '{getTimeStamp()}'
          WHERE id = '{selected_pkg$name()}' AND
          user_name = '{user$name}' AND
          user_role = '{user$role}' AND
          comment_type = 's'"
        )
      )
      
      shinyjs::disable("pkg_summary")
      # shinyjs::hide("submit_pkg_summary")
      # shinyjs::show("edit_pkg_summary")
      output$submit_edit_pkg_summary_ui <- renderUI(actionButton(NS(id, "edit_pkg_summary"),"Edit Summary"))
      
      # current_summary <- trimws(input$pkg_summary)
      # updateTextAreaInput(session, "pkg_summary", value = "",
      #                     placeholder = glue::glue('Current Summary: \n{current_summary}'))
      removeModal()
    })
    
    # if edit, do nothing
    observeEvent(input$submit_pkg_summary_edit, {
      removeModal()
    })
    
    
    output$downloads_plot <- plotly::renderPlotly({
      downloads_plot_data()
    })
    
    overall_comments <- reactive({
      selected_pkg$overall_comment_added()
      
      get_overall_comments(selected_pkg$name())
    })
    
    pkg_summary_added <- reactive(c(input$submit_pkg_summary, input$submit_pkg_summary_yes))
    pkg_summary <- reactive({
      pkg_summary_added()
      get_pkg_summary(selected_pkg$name())
    })
    
    # View Overall comments.
    viewCommentsServer(id = 'overall_comments',
                       comments = overall_comments,
                       pkg_name = selected_pkg$name,
                       label = 'Overall Comments')
    
    # View Pkg Summary
    viewCommentsServer(id = 'pkg_summary',
                       comments = pkg_summary,
                       pkg_name = selected_pkg$name,
                       label = 'Package Summary',
                       none_txt = "No summary."
                       )
    
    # View MM comments.
    viewCommentsServer(id = "mm_comments",
                       comments = mm_comments,
                       pkg_name = selected_pkg$name,
                       label = 'Maintainance Metrics Comments')
    
    # View Comm Usage comments.
    viewCommentsServer(id = 'cm_comments',
                       comments = cm_comments,
                       pkg_name = selected_pkg$name,
                       label = 'Community Usage Metrics Comments')
    
    # Maintenance metrics cards.
    metricGridServer("mm_metricGrid", metrics = maint_metrics)
    
    # Community usage metrics cards.
    metricGridServer("cm_metricGrid", metrics = com_metrics)
    
    output$communityMetrics_ui <- renderUI({
      req(selected_pkg$name())
      
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
        h5('Package Decision:'),
        ifelse(selected_pkg$decision() == '', 
               'Pending',
               selected_pkg$decision()))
    })
    
    # Display general information about report.
    output$about_report <- renderUI({
      req(selected_pkg$name())
      
      tagList(
        h5('{riskassessment} App Version:'), app_version,
        h5('riskmetric Version:'), paste0(packageVersion("riskmetric")),
        h5('Generated on:'), format(Sys.time(), usetz = TRUE)
      )
    })

    # Display the metric weights.
    output$weights_table <- DT::renderDataTable({
      req(selected_pkg$name())
      
      metric_weights()
      
    }, options = list(dom = "t", searching = FALSE, pageLength = -1, lengthChange = FALSE,
                      info = FALSE,
                      columnDefs = list(list(className = 'dt-center', targets = 2))
                      ))
    
    mod_downloadHandler_server("downloadHandler", selected_pkg$name, user, metric_weights)
  })
}
