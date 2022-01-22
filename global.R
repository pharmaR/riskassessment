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

# Creates introjs help button.
addHelpButton <- function(id){
  fluidRow(
    style = "float: right",
    column(
      width = 3,
      actionBttn(id, "",
                 color = "success",
                 icon = icon("question-circle"),
                 block = FALSE,
                 style = "simple",
                 size = "md")
    ))
}

# Displays message when no package is selected.
showSelectPackageMessage <- function(message = "Please select a package"){
  h6(message,
     style = 
     "text-align: center;
     color: gray;
     padding-top: 50px;")
}

# Displays formatted comments.
showComments <- function(pkg_name, comment_type){
  comments <- db_fun(
    glue(
      "SELECT user_name, user_role, comment, added_on
      FROM comments
      WHERE comm_id = '{pkg_name}' AND comment_type = '{comment_type}'"
    )
  ) |>
    map(rev)
  
  ifelse(
    length(comments) == 0, 
    "No comments",
    paste(
      "<div class='col-sm-12 comment-border-bottom single-comment-div'><i class='fa fa-user-tie fa-4x'></i><h3 class='ml-3'><b class='user-name-color'>",
      comments$user_name,
      "(",
      comments$user_role,
      ")",
      "</b><sub>",
      comments$added_on,
      "</sub></h3><h4 class='ml-3 lh-4'>",
      comments$comment,
      "</h4></div>"
    )
  )
}

