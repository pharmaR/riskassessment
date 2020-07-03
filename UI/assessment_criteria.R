#####################################################################################################################
# assessment_criteria.R - Display the modal pop up window with Maintenance Metrics,Community Usage Metrics and Testing Metrics  
# infomation with tables.
# 
# Author: Aravind
# Created: 02/06/2020.
#####################################################################################################################

# Start of the Assessment Criteria Source file for UI Module.

# Show modal pop window for assessment criteria action link

showModal(tags$div(id = "assessment_criteria_id", modalDialog(
 
  actionButton("assessment_criteria_close", "X"),  # Action button to close the window.
  
  tabsetPanel(
    id = "assessment_criteria_tabs_id",
    tabPanel(
      id = "tab1",
      value = "tab_1",
      tags$b("Maintenance Metrics", class = "txt-color"),
      h3("Description"),
      htmlOutput("maintenance_desc"),  # html output for maintenacne metrics content.  
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

# End of the Assessment Criteria Source file for UI Module.