#####################################################################################################################
# assessment_criteria.R - Assessment Criteria server source file.  
# Author: K Aravind Reddy
# Date: July 13th, 2020
# License: MIT License
#####################################################################################################################


# Start of the render outputs

# 1. Output of the render text to Display the Maintenance Metrics content.

output$maintenance_desc <- renderText({
  desc_maintenance <- read_file("./Data/maintenance.txt")
  paste("<h4>", desc_maintenance, "</h4>")
})  # End of the maintenance text render Output.

# 2. Output of the render table for Maintenance Metrics.

output$maintenance_table <- DT::renderDataTable(
  datatable(
    read_csv("./Data/maintenance.csv"),
    escape = FALSE,
    class = "cell-border",
    selection = 'none',
    options = list(
      sScrollX = "100%",
      aLengthMenu = list(c(5, 10, 20, 100,-1), list('5', '10', '20', '100', 'All')),
      iDisplayLength = 5
    )
  )
)  # End of the maintenance table render output.

# 3. Output of the render text to Dispaly the Community Usage Metrics content.

output$community_usage_desc <- renderText({
  desc_community_usage <- read_file("./Data/community.txt")
  paste("<h4>", desc_community_usage, "</h4>")
})  # End of the Community usage text render output.

# 4. Output of the render table for Community Usage Metrics.

output$community_usage_table <- DT::renderDataTable(
  datatable(
    read_csv("./Data/community.csv"),
    escape = FALSE,
    class = "cell-border",
    selection = 'none',
    options = list(
      sScrollX = "100%",
      aLengthMenu = list(c(5, 10, 20, 100,-1), list('5', '10', '20', '100', 'All')),
      iDisplayLength = 5
    )
  )
)  # End of the community usage table render output.

# 5. Output of the render text to Dispaly the Testing Metrics content.

output$testing_desc <- renderText({
  desc_testing <- read_file("./Data/testing.txt")
  paste("<h4>", desc_testing, "</h4>")
})  # End of the testing text render Output

# 6. Output of the render table for Testing Metrics.

output$testing_table <- DT::renderDataTable(
  datatable(
    read_csv("./Data/testing.csv"),
    escape = FALSE,
    class = "cell-border",
    selection = 'none',
    options = list(
      sScrollX = "100%",
      aLengthMenu = list(c(5, 10, 20, 100,-1), list('5', '10', '20', '100', 'All')),
      iDisplayLength = 5
    )
  )
)  # End of the testing table render Output.

# End of the render Output's'

# Start of the observeEvent's'

# Observe event to close the assessment criteria window.  

observeEvent(input$assessment_criteria_close, {
  removeModal()
})  # End of the observe event.

# End of the Assessment Criteria server source file.
