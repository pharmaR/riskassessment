# Building a Prod-Ready, Robust Shiny Application.
# 
# README: each step of the dev files is optional, and you don't have to 
# fill every dev scripts before getting started. 
# 01_start.R should be filled at start. 
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
# 
# 
########################################
#### CURRENT FILE: ON START SCRIPT #####
########################################

usethis::use_description(check_name = F)

## Fill the DESCRIPTION ---- didn't run
## Add meta data about your application
# golem::fill_desc(
#   pkg_name = "riskassessment", # The Name of the package containing the App 
#   pkg_title = "riskassessment: a web app designed to interface with the `riskmetric` package",
#   pkg_description = 
#     "The `riskassessment` application allows users to define a list of R
#     packages to assesss against metrics engineered by the `riskmetric`
#     package. In general, those metrics evaluate package development best practices,
#     code documentation, community engagement, and sustainability that culiminate in a
#     quantitative risk score. The app allows users to select and review one package
#     at a time to consume interactive visuals, download reports for sharing, submit
#     comments for group collaboration, and even tweak metric weights used to
#     calculate the packages' risk scores. When deployed in the context of an
#     organization, the app serves as a platform for package validation, offering
#     an authentication wall where roles are assigned to various users and 'final
#     decisions' can be made at the package level.
#   ", 
#   author_first_name = "Marly", # Your First Name
#   author_last_name = "Gotti", # Your Last Name
#   author_email = c("marly.cormar@biogen.com"), # Your Email
#   repo_url = "https://github.com/pharmaR/risk_assessment" # The URL of the GitHub Repo (optional) 
# )     

# ## Set {golem} options ---- ran
# golem::set_golem_options()

## Create Common Files ---- ran
## See ?usethis for more information
usethis::use_mit_license(copyright_holder = "2020 Fission Labs and R Validation Hub contributors")

# # usethis::use_mit_license( name = "Maya Gans; Aaron Clark; Robert Krajcik; Nate Mockler" )  # You can set another license here
# usethis::use_readme_rmd( open = T )
# usethis::use_code_of_conduct()
# usethis::use_lifecycle_badge( "Experimental" ) #Experimental, Maturing, Stable, Superseded, Archived, Dormant, Questioning
# usethis::use_news_md( open = FALSE )
# 
# ## Use git ---- ran
# usethis::use_git("AC Initial Commit")
# # $ git remote add origin https://github.com/biogen-inc/tidyCDISC.git
# 
# ## Init Testing Infrastructure ---- ran
# ## Create a template for tests
# # Call `use_test()` to initialize a basic test file and open it for editing.
# golem::use_recommended_tests()
# 
# ## Use Recommended Packages ---- ran slightly altered
# # By Default, this will add “shiny”, “DT”, “attempt”, “glue”, “htmltools”, and “golem” as a dependency 
# # to our package. Since we don't need all of those, we'll adjusted the recommended vector...
# golem::use_recommended_deps(recommended = c("shiny", "DT",  "glue", "golem")) #"attempt", "htmltools",

# ## Favicon ---- ran
# # If you want to change the favicon (default is golem's one)
# golem::remove_favicon()
# golem::use_favicon("inst/app/www/app_FAVICON.ico") # path = "path/to/ico". Can be an online file. 
# # Favicon is automatically linked in app_ui via `golem_add_external_resources()`
# 
# ## Add helper functions ---- ran. Go checkout new files
# golem::use_utils_ui() # File created at ~tidyCDISC/R/golem_utils_ui.R
# golem::use_utils_server() # tidyCDISC/R/golem_utils_server.R
# 


# Packages needed for the app.
raa_pkgs = c(
              "shiny"
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
             # ,"tidyverse"
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
)
use_package_v <- Vectorize(usethis::use_package)
use_package_v(raa_pkgs)
usethis::use_package("tidyverse")
tidypkgs <- tidyverse::tidyverse_packages(include_self = F)
use_package_v(tidypkgs)

usethis::use_package("glue")
usethis::use_dev_package("riskmetric")

# Used this to create new golemized project so I could steal files and mimic app structure
# golem::create_golem("riskassessment")
