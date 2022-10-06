
#' The `riskassessment` package
#'
#' The Risk Assessment App is an interactive web application serving as a front
#' end application for the `riskmetric` R package. `riskmetric` is a framework
#' to quantify risk by assessing a number of metrics meant to evaluate
#' development best practices, code documentation, community engagement, and
#' development sustainability. The app and `riskmetric` aim to provide some
#' context for validation within regulated industries.
#'
#' @keywords internal
#'
#' @import shiny
#' @import dplyr
#' 
"_PACKAGE"



library_all <- FALSE
if(library_all) {
  # Packages needed for the app.
  packages = c(
     # "shiny"
     # ,"shinyhelper"
     # ,"shinyjs"
     # ,
     "shinydashboard"
     # ,"shinyWidgets"
     # ,"data.table"
     # ,"DT"
     # ,"readr"
     # ,"lubridate"
     # ,"RSQLite"
     # ,"DBI"
     # ,"rvest"
     # ,"xml2"
     # ,"httr"
     # ,"desc"
     # ,"dplyr"
     # ,"tools"
     # ,"stringr"
     # ,"tidyverse"
     # ,"loggit"
     # ,"plotly"
     # ,"cranlogs"
     # ,"formattable"
     # ,"rintrojs"
     # ,"shinymanager"
     # ,"keyring"
     # ,"rstudioapi"
     # ,"glue"
     # ,"bslib"
    ,
    'bslib',
    'config',
    'cranlogs',
    # 'crayon',
    # 'data.table',
    'DBI',
    'desc',
    'dplyr',
    'DT',
    'forcats',
    'formattable',
    'glue',
    'golem',
    'keyring',
    'loggit',
    'lubridate',
    'plotly',
    'purrr',
    'readr',
    'rintrojs',
    'riskmetric',
    'rlang',
    'rmarkdown',
    'RSQLite',
    'rvest',
    'shiny',
    'shinyhelper',
    'shinyjs',
    'shinymanager',
    'shinyWidgets',
    'stringr'
  )
  
  # Load required packages.
  package.check <- lapply(
    packages, FUN = function(x) { library(x, character.only = TRUE) }
  )
  
  # Note: If deploying the app to shinyapps.io, then the code to directly install
  # missing packages will need to be removed as the app will fail to deploy.
  
  # Install and load required packages.
  package.check <- lapply(
    packages,
    FUN = function(x) {
      if (!require(x, character.only = TRUE)) {
        install.packages(x, dependencies = TRUE)
        library(x, character.only = TRUE)
      }
    }
  )
}


