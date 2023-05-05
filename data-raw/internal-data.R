# Global variables used within the application
# app_version <- 'beta'
passphrase <- 'somepassphrase'
# database_name <- "database.sqlite"
# credentials_name <- "credentials.sqlite"


# Overall descriptive text for community usage. Please edit text file to make changes.
community_usage_txt <- readLines(file.path("data-raw", "community.txt"))

# Table of community usage descriptions. Please edit the csv file to make changes.
community_usage_tbl <- read.csv(file.path("data-raw", "community.csv"), stringsAsFactors = FALSE)

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
maintenance_metrics_tbl <- read.csv(file.path("data-raw", "maintenance.csv"), stringsAsFactors = FALSE)

# Overall descriptive text for testing. Please edit text file to make changes.
testing_text <- readLines(file.path("data-raw", "testing.txt"))

# Table of testing descriptions. Please edit the csv file to make changes.
testing_tbl <- read.csv(file.path("data-raw", "testing.csv"), stringsAsFactors = FALSE)

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
template <- read.csv(file.path('data-raw', 'upload_format.csv'),  stringsAsFactors = FALSE)

test_pkg_lst <- c("dplyr", "tidyr", "readr", "purrr", "tibble", "stringr", "forcats")

test_pkg_refs_compl <-
  test_pkg_lst %>%
  purrr::map(riskmetric::pkg_ref, source = "pkg_cran_remote", repos = c("https://cran.rstudio.com")) %>%
  purrr::set_names(test_pkg_lst)

test_pkg_refs <-
  test_pkg_refs_compl %>%
  purrr::map(~ .x[c("name", "version", "source")] %>% purrr::set_names(c("name", "version", "source")))

test_pkg_df <-
  test_pkg_refs %>%
  map(~ .x["version"]) %>%
  stack() %>%
  dplyr::rename("Version"="values","Package"="ind")

test_pkg_info <-
  test_pkg_lst %>%
  purrr::map(get_latest_pkg_info) %>%
  purrr::set_names(test_pkg_lst)

test_pkg_assess <-
  test_pkg_refs_compl %>%
  purrr::map( ~ .x %>%
                dplyr::as_tibble() %>%
                riskmetric::pkg_assess())

test_pkg_cum <-
  test_pkg_lst %>%
  purrr::map(generate_comm_data) %>%
  purrr::set_names(test_pkg_lst)

color_palette <- c("#06B756","#2FBC06","#67BA04","#81B50A","#96AB0A","#A99D04","#B78D07","#BE7900","#BE6200","#B24F22","#A63E24")

usethis::use_data(
  # app_version, 
  # database_name, #credentials_name,
  passphrase,
  community_usage_txt, community_usage_tbl,
  maintenance_metrics_text, maintenance_metrics_tbl,
  testing_text, testing_tbl,
  riskcalc_text, template,
  test_pkg_df, test_pkg_refs, test_pkg_info, test_pkg_assess, test_pkg_cum,
  color_palette,
  internal = TRUE, overwrite = TRUE)
