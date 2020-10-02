# This is only temporary to make it work on shinyapps.io
library(shiny)
library(shinyjs)
library(shinydashboard)
library(shinyWidgets)
library(data.table)
library(DT)
library(readr)
library(lubridate)
library(RSQLite)
library(DBI)
library(rvest)
library(xml2)
library(httr)
library(desc)
library(dplyr)
library(tools)
library(stringr)
library(tidyverse)
library(loggit)
library(highcharter)
library(shinycssloaders)
library(rAmCharts)
library(devtools)

#devtools::install_github("pharmaR/riskmetric")
library(riskmetric)


# Commenting this part only for shinyapps.io to work
# # Packages needed for the app.
# packages = c("shiny"
#              ,"shinyjs"
#              ,"shinydashboard"
#              ,"shinyWidgets"
#              ,"data.table"
#              ,"DT"
#              ,"readr"
#              ,"lubridate"
#              ,"RSQLite"
#              ,"DBI"
#              ,"rvest"
#              ,"xml2"
#              ,"httr"
#              ,"desc"
#              ,"dplyr"
#              ,"tools"
#              ,"stringr"
#              ,"tidyverse"
#              ,"loggit"
#              ,"highcharter"
#              ,"shinycssloaders"
#              ,"rAmCharts"
#              ,"devtools"
# )
# 
# # Load or install and load required packages.
# package.check <- lapply(
#   packages,
#   FUN = function(x) {
#     if (!require(x, character.only = TRUE)) {
#       install.packages(x, dependencies = TRUE)
#       library(x, character.only = TRUE)
#     }
#   }
# )
# 
# # Install and load riskmetric.
# if(!require(riskmetric)){
#   devtools::install_github("pharmaR/riskmetric")
#   library(riskmetric)
# }
