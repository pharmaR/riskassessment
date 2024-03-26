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

privileges_tbl <- readr::read_csv(file.path("data-raw", "privileges.csv"), col_types = "cccc")


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

# New light palette, verified as color-blind friendly here:
# https://davidmathlogic.com/colorblind/#%239CFF94-%23B3FF87-%23BCFF43-%23D8F244-%23F2E24B-%23FFD070-%23FFBE82-%23FFA87C-%23FF8F6C-%23FF765B
# code:
# paste0("'",viridisLite::turbo(11, begin = 0.4, end = .8225)[2:11] %>% colorspace::lighten(.25) %>% paste(collapse = "', '"),"'")
color_palette <- c('#9CFF94FF', '#B3FF87FF', '#BCFF43FF', '#D8F244FF', '#F2E24BFF', '#FFD070FF', '#FFBE82FF', '#FFA87CFF', '#FF8F6CFF', '#FF765BFF')

used_privileges <- c("admin", "weight_adjust", "auto_decision_adjust", "final_decision", "revert_decision", "add_package", "delete_package", "overall_comment", "general_comment")

metric_lst <- c("1"='has_vignettes', "2"='has_news', "3"='news_current', "4"='has_bug_reports_url', "5"='has_website', "6"='has_maintainer', "7"='has_source_control', "8"='export_help', "9"='bugs_status', "10"='license', "11"='dependencies', "12"='reverse_dependencies', "13"='covr_coverage', "14"='downloads_1yr')

rpt_choices <- c("Report Author", "Report Date", "Risk Score", "Overall Comment", "Package Summary",
                 "Maintenance Metrics", "Maintenance Comments", "Community Usage Metrics", "Community Usage Comments",
                 "Package Dependencies", "Source Explorer Comments", "Function Explorer Comments")

usethis::use_data(
  # app_version, 
  # database_name, #credentials_name,
  passphrase,
  community_usage_txt, community_usage_tbl,
  maintenance_metrics_text, maintenance_metrics_tbl,
  testing_text, testing_tbl,
  riskcalc_text, template,
  privileges_tbl,
  test_pkg_lst, test_pkg_refs, test_pkg_info, test_pkg_assess, test_pkg_cum,
  color_palette, used_privileges, metric_lst, rpt_choices,
  internal = TRUE, overwrite = TRUE)

