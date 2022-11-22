# Set options here
options(golem.app.prod = FALSE) # TRUE = production mode, FALSE = development mode
options(shiny.fullstacktrace = FALSE)
options(dplyr.summarise.inform = FALSE) # suppress summarise() has grouped output by..."
# options(shiny.autoload.r=FALSE)

#Detach all loaded packages and clean your environment
golem::detach_all_attached()

# Document and reload your package, which runs these three functions...
golem::document_and_reload()

# Run the application -- these credentials have been entered already
run_app(pre_auth_user = TRUE, login_creds = list(user_id = "UberAdmin", user_pwd = "ASDfgh1"))

run_app()

# # turn off any options
# options(shiny.autoload.r=NULL)


