
# #' assessment_criteria UI Function
# #'
# #' @description A shiny Module.
# #'
# #' @param id,input,output,session Internal parameters for {shiny}.
# #'
# #' @noRd 
# #'
# #' @import shiny
# mod_assessment_criteria_ui <- function(){ # no id
  # ns <- NS(id)
    
# }
    


#' assessment_criteria Server Functions
#' 
#' @import shiny
#' @noRd 
mod_assessment_criteria_server <-
  function(input, output, session
    ){ # id removed & added: input, output, session!
    
  # moduleServer( id, function(input, output, session){
  #   ns <- session$ns
  # })
    
    showModal(tags$div(id = "assessment_criteria_id", modalDialog(
      
      actionButton("assessment_criteria_close", "X"),  # Action button to close the window.
      
      tabsetPanel(
        id = "assessment_criteria_tabs_id",
        tabPanel(
          id = "tab0",
          value = "tab_0",
          tags$b("Risk Calculation", class = "txt-color"),
          h3("Description"),
          uiOutput("riskcalc_desc"),  # Maintenance metrics description.
          br(),
          div(style = "display: block;margin-left: auto; margin-right: auto; width:50%;",
              dataTableOutput("riskcalc_weights_table"))  # data table for maintenance metrics.
        ),
        tabPanel(
          id = "tab1",
          value = "tab_1",
          tags$b("Maintenance Metrics", class = "txt-color"),
          h3("Description"),
          uiOutput("maintenance_desc"),  # Maintenance metrics description.
          br(),
          dataTableOutput("maintenance_table")  # data table for maintenance metrics. 
        ),
        tabPanel(
          id = "tab2",
          value = "tab_2",
          tags$b("Community Usage Metrics", class = "txt-color"),
          h3("Description"),
          htmlOutput("community_usage_desc"),  # html output for community usage metrics content.
          br(),
          dataTableOutput("community_usage_table")  # data table for community usage metrics.
        ),
        tabPanel(
          id = "tab3",
          value = "tab_3",
          tags$b("Testing Metrics", class = "txt-color"),
          h3("Description"),
          htmlOutput("testing_desc"),  # html output for testing metrics content.
          br(),
          dataTableOutput("testing_table")  # data table for testing metrics.
        )
      ),
      footer = NULL, easyClose = TRUE
    )))  
    
    riskcalc_text <- "<h4>Per the <b>riskmetric</b> package, there 
are a series of metrics underlying the risk calculation for any 
given package. The short-hand names for each metric are 
listed below with more detail provided on consecutive tabs.
To calculate a packages overall risk, 
each metric is assigned a quantitative value: the yes/no metrics (like 
<b>has_bug_reports_url</b>) recieve a 0 if 'no'
or a 1 if 'yes', while the quantitative metrics (like <b>bugs_status</b>'s percentage) 
remain as-is. Since a metric's
importance is subjective,  weights are applied to put more/less emphasis
on how certain metrics contribute to the over risk score.
The weights below were set by this app's admin(s) and are standardized so 
that each is between 0 and 1, and when summed, 
equal 1. The risk of a package will be determined by 1 - sum(metric's
numeric value <b>x</b> standardized weight)</h4>"
    
    # Display the Maintenance Metrics description.
    output$riskcalc_desc <- renderUI({
      HTML(riskcalc_text)
    })
    
    # Render table for Maintenance Metrics.
    output$riskcalc_weights_table <- DT::renderDataTable({
      d <- get_metric_weights() %>%
        mutate(weight = ifelse(name == "covr_coverage", 0, weight)) %>%
        formattable() %>%
        mutate(standardized_weight = round(weight / sum(weight, na.rm = TRUE), 4)) %>%
        select(-new_weight)
      
      as.datatable(d,
                   selection = list(mode = 'single'),
                   colnames = c("Metric Name", "Admin Weight", "Standardized Weight"),
                   rownames = FALSE,
                   options = list(
                     searching = FALSE,
                     lengthChange = FALSE,
                     pageLength = 15,
                     columnDefs = list(list(className = 'dt-center', targets = 1:2))
                   )
      ) %>%
        DT::formatStyle(names(d),lineHeight='80%')
    })
    
    maintenance_metrics_text <- "<h4>Best practices in software development and
maintenance can significantly reduce the potential for bugs / errors.
Package maintainers are not obliged to share their practices (and rarely do),
however the open source community provides several ways of measuring software
development best practices. The R Validation Hub proposes the following
metrics based on the white paper
<a target='_blank' href='https://www.pharmar.org/presentations/r_packages-white_paper.pdf'>
A Risk-based Approach for Assessing R package Accuracy within a Validated
Infrastructure</a>.</h4>"
    
    
    # Display the Maintenance Metrics description.
    output$maintenance_desc <- renderUI({
      HTML(maintenance_metrics_text)
    })
    
    
    # Render table for Maintenance Metrics.
    output$maintenance_table <- DT::renderDataTable(
      datatable(
        maintenance, # internal data
        escape = FALSE,
        class = "cell-border",
        selection = 'none',
        options = list(
          sScrollX = "100%",
          aLengthMenu = list(c(5, 10, 20, 100,-1), list('5', '10', '20', '100', 'All')),
          iDisplayLength = 15
        )
      )
    )
    
    
    # Display the Community Usage Metrics text content.
    output$community_usage_desc <- renderText({
      desc_community_usage <- #read_file(file.path("Data", "community.txt"))
        "The user community plays an important role in open source software development.  The more exposure a package has had to the user community, the more ad-hoc testing it has been exposed to.  Over time the better packages tend to rise to the top of the pack, leading to more downloads and increased exposure.

The aim of the community usage metrics is to assess the level of exposure to the wider community and thus the level of risk that a package presents.  The following table highlights some community usage metrics."
      paste("<h4>", desc_community_usage, "</h4>")
    })
    
    
    # Render table for Community Usage Metrics.
    output$community_usage_table <- DT::renderDataTable(
      datatable(
        community, # internal data
        escape = FALSE,
        class = "cell-border",
        selection = 'none',
        options = list(
          sScrollX = "100%",
          aLengthMenu = list(c(5, 10, 20, 100,-1), list('5', '10', '20', '100', 'All')),
          iDisplayLength = 15
        )
      )
    )
    
    
    # Display the Testing Metrics text content.
    output$testing_desc <- renderText({
      desc_testing <- #read_file(file.path("Data", "testing.txt"))
        "Testing is a vital component in a well-established Software Development Life Cycle (SDLC).  The more tests the more confident we can be in the stability of the package over time.  The following table highlights an important testing metric. Note: the 'covr_coverage' metric is currently disabled (weight = 0) until the 'riskmetric' package returns a non-NA value for this metric."
      paste("<h4>", desc_testing, "</h4>")
    })
    
    
    # Render table for Testing Metrics.
    output$testing_table <- DT::renderDataTable(
      datatable(
        testing, # internal data
        escape = FALSE,
        class = "cell-border",
        selection = 'none',
        options = list(
          sScrollX = "100%",
          aLengthMenu = list(c(5, 10, 20, 100,-1), list('5', '10', '20', '100', 'All')),
          iDisplayLength = 5
        )
      )
    )
    
    
    # Close the assessment criteria window.
    observeEvent(input$assessment_criteria_close, {
      removeModal()
    })
    
    
}
    
## To be copied in the UI
# mod_assessment_criteria_ui("assessment_criteria_ui_1")
    
## To be copied in the server
# mod_assessment_criteria_server("assessment_criteria_ui_1")
