
#' The `riskassessment` package
#'
#' The `riskassessment` App is an interactive web application serving as a front
#' end application for the `riskmetric` R package. `riskmetric` is a framework
#' to quantify risk by assessing a number of metrics meant to evaluate
#' development best practices, code documentation, community engagement, and
#' development sustainability. The app and `riskmetric` aim to provide some
#' context for validation within regulated industries.
#'
#' @keywords internal
#'
#' 
#' @import dplyr
#' @import riskmetric
#' @importFrom stats lm predict
#' @importFrom utils download.file tail untar
#' 
"_PACKAGE"

# avoid "no visible binding for global variable" messages from check()
utils::globalVariables(
  c(
    '.',
    'Actions',
    'Author',
    'base', 
    'base_cat_disp',
    'base_cat_pct',
    'base_cat_sum',
    'cnt',
    'contrib.url',
    'date_added',
    'day_month_year',
    'decision',
    'decision_by',
    'decision_cat_disp',
    'decision_cat_sum',
    'decision_date',
    'description',
    'description',
    'downloads',
    'ea_v',
    'estimate',
    'explore_metrics',
    'func',
    'have_changed',
    'Last modified',
    'last_comment',
    'License',
    'line',
    'line1',
    'll',
    'long_name',
    'lower_limit',
    'Maintainer',
    'metric_score',
    'must_change',
    'name',
    'Name',
    'new_role',
    'new_weight',
    'non_base',
    'non_base_sum',
    'old_role',
    'package',
    'Package',
    'path',
    'Published',
    'Role',
    'role',
    'row_n',
    'rpt_choices',
    'score',
    'setNames',
    'size',
    'status',
    'succ_icon',
    'text',
    'token',
    'type',
    'type_cat_disp',
    'type_cat_pct',
    'type_cat_sum',
    'upld',
    'upld_cat_disp',
    'upld_cat_pct',
    'upld_cat_sum',
    'upld_non_base',
    'upld_non_base_sum',
    'upld_non_base_pct',
    'upper_limit',
    'user_role',
    'Version',
    'was_decision_made',
    'weight',
    'X1',
    'X2'
  )
)


# report_include_choices <- c("Report Author", "Report Date", "Risk Score", 
#   "Overall Comment", "Package Summary", "Maintenance Metrics",
#   "Maintenance Comments", "Community Usage Metrics", "Community Usage Comments")