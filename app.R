###############################################################################
# Project: R Validation Hub - R Package Risk Assessment App
# Author: K Aravind Reddy
# Date: July 13th, 2020
# License: MIT License
# You can run the application by executing 'runApp()' command.
###############################################################################


# Create db if it doesn't exist.
if(!file.exists(database_name)) create_db()

if(!file.exists(credentials_name)) create_credentials_db()

# Start logging info.
set_logfile("loggit.json")

hidden(p(id = "assessment_criteria_bttn"))

# Set spinner options for the tabs.
options(
  spinner.color = "#0275D8",
  spinner.color.background = "#ffffff",
  spinner.size = 1
)


