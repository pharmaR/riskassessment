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
             ,"shinycssloaders"
             ,"rAmCharts"
             ,"devtools"
             ,"glue"
             ,"plotly"
)

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

# Install riskmetric from github and load it
if(!require(riskmetric)){
  devtools::install_github("pharmaR/riskmetric")
  library(riskmetric)
}


# Note: If deploying the app to shinyapps.io, then the code to directly install
# missing packages will need to be removed as the app will fail to deploy.
# Instead comment the code that install packages and attach them directly by
# uncommenting the lines below.

# library(shiny)
# library(shinyjs)
# library(shinydashboard)
# library(shinyWidgets)
# library(data.table)
# library(DT)
# library(readr)
# library(lubridate)
# library(RSQLite)
# library(DBI)
# library(rvest)
# library(xml2)
# library(httr)
# library(desc)
# library(dplyr)
# library(tools)
# library(stringr)
# library(tidyverse)
# library(loggit)
# library(shinycssloaders)
# library(rAmCharts)
# library(devtools)
# library(plotly)
# library(riskmetric) # devtools::install_github("pharmaR/riskmetric")
