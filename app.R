# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file

pkgload::load_all(export_all = FALSE,helpers = FALSE,attach_testthat = FALSE)
options( "golem.app.prod" = TRUE)
riskassessment::run_app(pre_auth_user = TRUE, login_note = shiny::HTML(
  "<em>Note:</em><br>
  To run as ADMIN, type username 
  <span style='text-transform:none;'><u>demo_admin</u></span> with password <u><span style='text-transform:none;'>Admin@1</span></u> above.
  <br><br>
  To run as NONADMIN, type username
    <span style='text-transform:none;'><u>demo_nonadmin</u></span> with password <u><span style='text-transform:none;'>Nonadmin@1</span></u> above.")) 
