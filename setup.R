

## Packages to install to run the app.
packages = c("shiny"
             ,"shinyalert"
             ,"shinyWidgets"
             ,"shinymaterial"
             ,"shinyjs"
             ,"shinybusy"
             ,"shinydashboard"
             ,"data.table"
             ,"DT"
             ,"readr"
             ,"lubridate"
             ,"htmlTable"
             ,"rlang"
             ,"RSQLite"
             ,"DBI"
             ,"selectr"
             ,"rvest"
             ,"xml2"
             ,"httr"
             ,"dplyr"
             ,"desc"
             ,"tools"
             ,"loggit"
             ,"compare"
             ,"config"
             ,"renv"
             ,"flexdashboard"
             ,"ECharts2Shiny"
             ,"ggplot2"
             ,"highcharter"
             ,"dlstats"
             ,"shinycssloaders"
             ,"tidyverse"
             ,"rAmCharts"
             ,"shinyBS"
             ,"tippy"
             ,"devtools"
             ,"future"
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

