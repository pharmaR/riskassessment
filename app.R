# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file

pkgload::load_all(export_all = FALSE,helpers = FALSE,attach_testthat = FALSE)
options( "golem.app.prod" = TRUE)
riskassessment::run_app(login_note = HTML('<em>Note:</em> To log in for the first time, use the admin user:
                          <u>admin</u> with password <u>QWERTY1</u>.')) # add parameters here (if any)
