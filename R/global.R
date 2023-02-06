
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
#' 
"_PACKAGE"

# avoid "no visible binding for global variable" messages from check()
utils::globalVariables(c('.',
 'Author', 'day_month_year', 'decision', 'description', 'description', 'downloads',   
 'have_changed', 'Last modified', 'last_comment', 'License', 'long_name', 'Maintainer',
 'must_change', 'name', 'Name', 'new_weight', 'package', 'Published',
 'score', 'Version', 'was_decision_made', 'weight', 'X1', 'X2')
)
