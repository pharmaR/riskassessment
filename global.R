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
             ,'waiter'
             ,'riskmetric'
)

# Load required packages.
package.check <- lapply(
  packages, FUN = function(x) { library(x, character.only = TRUE) }
)

# Install and load required packages.
# package.check <- lapply(
#   packages,
#   FUN = function(x) {
#     if (!require(x, character.only = TRUE)) {
#       install.packages(x, dependencies = TRUE)
#       library(x, character.only = TRUE)
#     }
#   }
# )

# Install a specific version of riskmetric from GitHub and load it.
# if(!require(riskmetric)){
#   devtools::install_github("pharmaR/riskmetric", ref = "release-v0.1.1")
#   library(riskmetric)
# }


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

#' Displays a helper message. By default, it informs the user that he should
#' select a package.
showHelperMessage <- function(message = "Please select a package"){
  h6(message,
     style = 
       "text-align: center;
        color: gray;
        padding-top: 50px;")
}

# Displays formatted comments.
showComments <- function(pkg_name, comments){
  if (length(pkg_name) == 0)
    return("")

  ifelse(
    length(comments$user_name) == 0, 
    "No comments",
    paste0(
      "<div class='well'>",
      icon("user-tie"), " ", "user: ", comments$user_name, ", ", 
      icon("user-shield"), " ", "role: ", comments$user_role, ", ",
      icon("calendar-alt"), " ", "date: ", comments$added_on,
      br(), br(), 
      comments$comment,
      "</div>",
      collapse = ""
    )
  )
}
