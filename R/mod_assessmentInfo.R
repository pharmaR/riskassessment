#' UI for 'Assessment Info' Module
#' 
#' @param id a module id name
#' 
#' @importFrom DT dataTableOutput
#' 
#' @keywords internal
#' 
assessmentInfoUI <- function(id) {
  fluidPage(
    # fluidRow(
      # column(
      #   width = 8, offset = 2,
        h2("Assessment Criteria Overview", align = "center", `padding-bottom`="20px"),
        br(),
        tabsetPanel(
          tabPanel(
            title = "Risk Calculation",
            h6("About Risk Calculation"),
            uiOutput(NS(id, "riskcalc_desc")),  # Maintenance metrics description.
            br(),
            column(width = 8, offset = 2,
              DT::dataTableOutput(NS(id, "riskcalc_weights_table"))
            )
          ),
          tabPanel(
            title = "Maintenance Metrics",
            h6("About Maintenance Metrics"),
            uiOutput(NS(id, "maintenance_desc")),  # Maintenance metrics description.
            br(),
            DT::dataTableOutput(NS(id, "maintenance_table"))  # data table for maintenance metrics. 
          ),
          tabPanel(
            title = "Community Usage Metrics",
            h6("About Community Usage Metrics"),
            htmlOutput(NS(id, "community_usage_desc")),  # html output for community usage metrics content.
            br(),
            DT::dataTableOutput(NS(id, "community_usage_table"))  # data table for community usage metrics.
          ),
          tabPanel(
            title = "Testing Metrics",
            h6("About Testing Metrics"),
            htmlOutput(NS(id, "testing_desc")),  # html output for testing metrics content.
            br(),
            DT::dataTableOutput(NS(id, "testing_table"))  # data table for testing metrics.
          )
        # ))
    ))
}

#' Server Logic for 'Assessment Info' Module
#' 
#' @param id a module id name
#' @param metric_weights placeholder
#' 
#' @import dplyr
#' @importFrom DT renderDataTable formatStyle datatable
#' @importFrom formattable as.datatable
#' 
#' @keywords internal
#' 
assessmentInfoServer <- function(id, metric_weights) {
  moduleServer(id, function(input, output, session) {
    
    # Display the Maintenance Metrics description.
    output$riskcalc_desc <- renderUI(riskcalc_text)
    
    # Render table for Maintenance Metrics.
    output$riskcalc_weights_table <- DT::renderDataTable({
      d <- metric_weights() %>%
        formattable::formattable() %>%
        dplyr::mutate(standardized_weight = round(weight / sum(weight, na.rm = TRUE), 4))
      
      formattable::as.datatable(d,
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

    # Display the Maintenance Metrics description.
    output$maintenance_desc <- renderUI(maintenance_metrics_text)
    
    
    # Render table for Maintenance Metrics.
    output$maintenance_table <- DT::renderDataTable(
      DT::datatable(
        maintenance_metrics_tbl,
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
    output$community_usage_desc <- renderText(community_usage_txt)
    
    
    # Render table for Community Usage Metrics.
    output$community_usage_table <- DT::renderDataTable(
        DT::datatable(
          community_usage_tbl,
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
    output$testing_desc <- renderText(testing_text)
    
    
    # Render table for Testing Metrics.
    output$testing_table <- DT::renderDataTable(
      DT::datatable(
        testing_tbl,
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
  })
}
