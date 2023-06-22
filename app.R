# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file

pkgload::load_all(export_all = FALSE,helpers = FALSE,attach_testthat = FALSE)
options( "golem.app.prod" = TRUE)
riskassessment::run_app(
  login_note = shiny::HTML(
  "<em>Note:</em><br>
  To run as ADMIN, type username 
  <span style='text-transform:none;'><u>demo_admin</u></span> with password <u><span style='text-transform:none;'>Admin@1</span></u> above.
  <br><br>
  To run as LEAD, type username
    <span style='text-transform:none;'><u>demo_lead</u></span> with password <u><span style='text-transform:none;'>Lead@1</span></u> above.
  <br><br>
  To run as REVIEWER, type username
    <span style='text-transform:none;'><u>demo_reviewer</u></span> with password <u><span style='text-transform:none;'>Reviewer@1</span></u> above.")) 
