

## Packages to install to run the app.
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
             ,"dplyr"
             ,"desc"
             ,"tools"
             ,"stringr"
             ,"loggit"
             ,"highcharter"
             ,"shinycssloaders"
             ,"rAmCharts"
             ,"devtools"
             ,"versions"
             ,'remotes'
)

## load or install&load all required Packages.
# ,"tidyverse"


package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

# riskmetric is not yet on CRAN. Until it is, install using devtools.

if(!require(riskmetric)){
  # Remove hardcoded commit number
  devtools::install_github("pharmaR/riskmetric")
  library(riskmetric)
}

# load vector of available packages (on CRAN)
if (!exists("pkgs_vec")) {
  pkgs_vec <- as.data.frame(utils::available.packages())[[1]]
} 