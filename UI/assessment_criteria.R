###############################################################################
# assessment_criteria.R - Display the modal pop up window with Maintenance,
# Community Usage, and Testing Metrics info.
# Author: K Aravind Reddy
# Date: July 13th, 2020
# License: MIT License
###############################################################################


# Show modal pop window for assessment criteria action link.
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
    )
    # tabPanel( 
    #   id = "tab3",
    #   value = "tab_3",
    #   tags$b("Testing Metrics", class = "txt-color"),
    #   h3("Description"),
    #   htmlOutput("testing_desc"),  # html output for testing metrics content.
    #   br(),
    #   dataTableOutput("testing_table")  # data table for testing metrics.
    # )
  ),
  footer = NULL, easyClose = TRUE
)))  
