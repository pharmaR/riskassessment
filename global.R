# Packages needed for the app.
packages = c("shiny"
             ,"shinyhelper"
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
             ,"plotly"
             ,"cranlogs"
             ,"formattable"
             ,"rintrojs"
             ,"shinymanager"
             ,"keyring"
             ,"rstudioapi"
             ,"glue"
             ,"bslib"
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

# Load the functions to create infoboxes.
source("Utils/infoboxes.R")

# does this belong in utils or global.r? It is static, and non-functional
sidebar_steps <-
  data.frame(
    element = c("#assessment_criteria_bttn", "#db_dash_bttn", "#sel_pack", "#sel_ver",
                "#status", "#score", "#overall_comment", "#decision"),
    intro = c(
      "Click here to understand the package assessment process & criteria",
      "See an overview of the R packages that already exist in the database",
      "Click this dropdown to select assess a specific package",
      "The latest package version will autopopulate here.",
      "The status can be either 'Under Review' or 'Reviewed'.",
      "The score can take any value between 0 (no risk) and 1 (highest risk).",
      "After reviewing your package, you can leave an overall comment.",
      "Provide your input on the overall risk of the selected package."
    ),
    position = c(rep("left", 2), rep("bottom", 6))
  )

# Note: If deploying the app to shinyapps.io, then the code to directly install
# missing packages will need to be removed as the app will fail to deploy.
# Instead comment the code that install packages and attach them directly by
# uncommenting the lines below.

# library(shiny)
# library(shinyhelper)
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
# library(formattable)
# library(rintrojs)

options(keyring_user = "NeildeGrasseTyson")
