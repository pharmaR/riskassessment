###############################################################################
# assessment_criteria.R - Assessment Criteria server source file.  
# Author: K Aravind Reddy
# Date: July 13th, 2020
# License: MIT License
###############################################################################


# Display the Maintenance Metrics text content.
output$maintenance_desc <- renderText({
  desc_maintenance <- read_file(file.path("Data", "maintenance.txt"))
  paste("<h4>", desc_maintenance, "</h4>")
})


# Render table for Maintenance Metrics.
output$maintenance_table <- DT::renderDataTable(
  datatable(
    read_csv(file.path("Data", "maintenance.csv")),
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


# Display the Community Usage Metrics text content.
output$community_usage_desc <- renderText({
  desc_community_usage <- read_file(file.path("Data", "community.txt"))
  paste("<h4>", desc_community_usage, "</h4>")
})


# Render table for Community Usage Metrics.
output$community_usage_table <- DT::renderDataTable(
  datatable(
    read_csv(file.path("Data", "community.csv")),
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


# Display the Testing Metrics text content.
output$testing_desc <- renderText({
  desc_testing <- read_file(file.path("Data", "testing.txt"))
  paste("<h4>", desc_testing, "</h4>")
})


# Render table for Testing Metrics.
output$testing_table <- DT::renderDataTable(
  datatable(
    read_csv(file.path("Data", "testing.csv")),
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
