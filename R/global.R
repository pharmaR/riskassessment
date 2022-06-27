library_all <- FALSE

if(library_all) {
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



app_version <- 'beta'
passphrase <- 'somepassphrase'
database_name <- "database.sqlite"
credentials_name <- "credentials.sqlite"


# Create db if it doesn't exist.
if(!file.exists(database_name)) create_db()

# Create credentials db if it doesn't exist.
if(!file.exists(credentials_name)) create_credentials_db()

# Start logging info.
loggit::set_logfile("loggit.json")

theme <- bslib::bs_theme(
  bootswatch = "lux",
  version = 5,
  # bg = "white", 
  # fg = "#023967",
  primary = "#24305E",
  secondary = "#F76C6C",
  # success = "orange",
  # info = "yellow",
  # warning = "pink"
)

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
