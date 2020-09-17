

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
             ,"versions"
)

## load or install&load all required Packages.

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
  devtools::install_github("pharmaR/riskmetric@bfda3247563322d051c3aa50875cef9d48729b3a")
  library(riskmetric)
  
}

# load vector of available packages (on CRAN)
if (!exists("pkgs_vec")) {
pkgs_vec <- as.data.frame(utils::available.packages())[[1]]
}