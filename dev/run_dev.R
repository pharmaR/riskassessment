# Set options here
options(golem.app.prod = FALSE) # TRUE = production mode, FALSE = development mode
options(shiny.fullstacktrace = FALSE)
options(dplyr.summarise.inform = FALSE) # suppress summarise() has grouped output by..."
# options(shiny.autoload.r=FALSE)

#Detach all loaded packages and clean your environment
golem::detach_all_attached()

# Document and reload your package, which runs these three functions...
golem::document_and_reload()

# Run the application 
run_app(
  pre_auth_user = 'admin',
  login_note = shiny::HTML('<em>Note:</em> To log in for the first time, use the admin user:
                          <u>admin</u> with password <u>QWERTY1</u>.'))

# # turn off any options
# options(shiny.autoload.r=NULL)


