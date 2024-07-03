# Set options here
options(golem.app.prod = FALSE) # TRUE = production mode, FALSE = development mode
options(shiny.fullstacktrace = TRUE)
options(dplyr.summarise.inform = FALSE) # suppress summarise() has grouped output by..."
# options(shiny.autoload.r=FALSE)

#Detach all loaded packages and clean your environment
golem::detach_all_attached()

# Document and reload your package, which runs these three functions...
golem::document_and_reload()

# Run the application  
run_app(pre_auth_user = 'admin')
# run_app(pre_auth_user = 'lead')
# run_app(pre_auth_user = 'reviewer')
# run_app(pre_auth_user = 'viewer')

# # turn off any options
# options(shiny.autoload.r=NULL)


# devtools::load_all()
# upload_pkg_lst("Tplyr")
# upload_pkg_lst(c("tidyr", "IDEAFilter", "tidyCDISC", "ggplot2", "xportr"))

