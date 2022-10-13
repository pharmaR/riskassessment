# Global variables used within the application
app_version <- 'beta'
passphrase <- 'somepassphrase'
database_name <- "database.sqlite"
credentials_name <- "credentials.sqlite"


# Overall descriptive text for community usage. Please edit text file to make changes.
community_usage_txt <- readr::read_file(file.path("data-raw", "community.txt"))

# Table of community usage descriptions. Please edit the csv file to make changes.
community_usage_tbl <-
  DT::datatable(
    suppressMessages(readr::read_csv(file.path("data-raw", "community.csv"))),
    escape = FALSE,
    class = "cell-border",
    selection = 'none',
    options = list(
      sScrollX = "100%",
      aLengthMenu = list(c(5, 10, 20, 100,-1), list('5', '10', '20', '100', 'All')),
      iDisplayLength = 15
    )
  )

# Overall descriptive text for maintenance metrics.
maintenance_metrics_text <- shiny::HTML("Best practices in software development and
maintenance can significantly reduce the potential for bugs / errors.
Package maintainers are not obliged to share their practices (and rarely do),
however the open source community provides several ways of measuring software
development best practices. The R Validation Hub proposes the following
metrics based on the white paper
<a target='_blank' href='https://www.pharmar.org/presentations/r_packages-white_paper.pdf'>
A Risk-based Approach for Assessing R package Accuracy within a Validated
Infrastructure</a>.")

# Table of maintenance metrics descriptions. Please edit the csv file to make changes.
maintenance_metrics_tbl <-
  DT::datatable(
  suppressMessages(readr::read_csv(file.path("data-raw", "maintenance.csv"))),
  escape = FALSE,
  class = "cell-border",
  selection = 'none',
  options = list(
    sScrollX = "100%",
    aLengthMenu = list(c(5, 10, 20, 100,-1), list('5', '10', '20', '100', 'All')),
    iDisplayLength = 15
  )
)

# Overall descriptive text for testing. Please edit text file to make changes.
testing_text <- readr::read_file(file.path("data-raw", "testing.txt"))

# Table of testing descriptions. Please edit the csv file to make changes.
testing_tbl <-
  DT::datatable(
  readr::read_csv(file.path("data-raw", "testing.csv")),
  escape = FALSE,
  class = "cell-border",
  selection = 'none',
  options = list(
    sScrollX = "100%",
    aLengthMenu = list(c(5, 10, 20, 100,-1), list('5', '10', '20', '100', 'All')),
    iDisplayLength = 5
  )
)

# Overall risk calculation text.
riskcalc_text <- shiny::HTML("Per the <b>riskmetric</b> package, there 
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
numeric value <b>x</b> standardized weight)")


# Upload format template.
template <- readr::read_csv(file.path('data-raw', 'upload_format.csv'), show_col_types = FALSE)

usethis::use_data(
  app_version,
  passphrase,
  database_name, credentials_name,
  community_usage_txt, community_usage_tbl,
  maintenance_metrics_text, maintenance_metrics_tbl,
  testing_text, testing_tbl,
  riskcalc_text, template,
  internal = TRUE, overwrite = TRUE)
