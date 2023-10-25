# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file

Sys.setenv(GOLEM_CONFIG_ACTIVE = "demo")
# options(rsconnect.max.bundle.files = 20000)
options(shiny.fullstacktrace = TRUE)
pkgload::load_all(export_all = FALSE,helpers = FALSE,attach_testthat = FALSE)
options( "golem.app.prod" = TRUE)
riskassessment::run_app(
  login_note = shiny::HTML(
  "<em>Note:</em><br>
  Use the following credentials to log on with varying roles & privileges in the app, expanded upon <a href='https://pharmar.github.io/riskassessment/articles/User_Roles_and_Privileges.html#roles' target='_blank'>here</a>:
  <br><br>
    U: <span style='text-transform:none;'><u>demo_admin</u></span>; &nbsp;&nbsp;&nbsp; P: <u><span style='text-transform:none;'>Admin@1</span></u>
  <br>
    U: <span style='text-transform:none;'><u>demo_lead</u></span>; &nbsp;&nbsp;&nbsp;&nbsp;&nbsp; P: <u><span style='text-transform:none;'>Lead@1</span></u>
  <br>
    U: <span style='text-transform:none;'><u>demo_reviewer</u></span>; P: <u><span style='text-transform:none;'>Reviewer@1</span></u>
    <br>
    U: <span style='text-transform:none;'><u>demo_viewer</u></span>; &nbsp;&nbsp;  P: <u><span style='text-transform:none;'>Viewer@1</span></u>")
  ) 
