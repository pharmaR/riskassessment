# Packages needed for the app.
packages = c("shiny"
             ,"shinyjs"
             ,"shinydashboard"
             ,"shinyWidgets"
             ,"data.table"
             ,"DT"
             ,"readr"
             ,"lubridate"
             ,"RSQLite"
             ,"DBI"
             ,"rvest"
             ,"xml2"
             ,"httr"
             ,"desc"
             ,"dplyr"
             ,"tools"
             ,"stringr"
             ,"tidyverse"
             ,"loggit"
             ,"highcharter"
             ,"shinycssloaders"
             ,"rAmCharts"
             ,"devtools"
)

# Load or install and load required packages.
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

# Install and load riskmetric.
if(!require(riskmetric)){
  # Remove hardcoded commit number
  devtools::install_github("pharmaR/riskmetric")
  library(riskmetric)
}
