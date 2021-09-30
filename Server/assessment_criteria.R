###############################################################################
# assessment_criteria.R - Assessment Criteria server source file.
# Author: K Aravind Reddy
# Date: July 13th, 2020
# License: MIT License
###############################################################################


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
    formattable() %>%
    mutate(standardized_weight = weight / sum(weight, na.rm = TRUE))

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
  )
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
    suppressMessages(read_csv(file.path("Data", "maintenance.csv"))),
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
  desc_community_usage <- read_file(file.path("Data", "community.txt"))
  paste("<h4>", desc_community_usage, "</h4>")
})


# Render table for Community Usage Metrics.
output$community_usage_table <- DT::renderDataTable(
  datatable(
    suppressMessages(read_csv(file.path("Data", "community.csv"))),
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
