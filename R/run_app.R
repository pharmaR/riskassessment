#' Run the Shiny Application
#'
#' @param app_ver a "global" variable that is passed to several modules &
#'   reports which details the installed package version when not specified. It
#'   can be overwritten to include a specific version name as a text string.
#' @param login_note a text string to display underneath the auth screen's login
#'   button, provided to guide users
#' @param ... arguments to pass to golem_opts. See `?golem::get_golem_options`
#'   for more details.
#' @inheritParams shiny::shinyApp
#' @return a shiny app object
#' 
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(
  onStart = initialize_raa,
  options = list(),
  enableBookmarking = NULL,
  uiPattern = "/",
  app_ver = NULL,
  login_note = NULL,
  ...
) {
  # Pre-process some run-app inputs
  if(is.null(app_ver)) app_ver <- paste0(packageVersion("riskassessment"))
  assessment_db_name <- get_db_config('assessment_db')
  if(is.null(assessment_db_name)) assessment_db_name <- "database.sqlite"
  credentials_db_name <- get_db_config('credential_db')
  if(is.null(credentials_db_name)) credentials_db_name <- "credentials.sqlite"
  decisions <- get_db_config('decisions')
  decision_categories <- if(is.null(decisions) || is.null(decisions$categories)) c("Low Risk", "Medium Risk", "High Risk") else decisions$categories
  
  if(is.null(login_note)) {
    # https://github.com/rstudio/fontawesome/issues/99
    # Here, we make sure user has a functional version of fontawesome
    fa_v <- packageVersion("fontawesome")
    if(!file.exists(credentials_db_name)) {
      login_note <- HTML('<em>Note:</em> To log in for the first time, use the admin user:
                          <u>admin</u> with password <u>QWERTY1</u>.')
    } else if(fa_v == '0.4.0') {
      login_note <- HTML(glue::glue("<em>Note:</em> HTML reports will not render with {{fontawesome}} v0.4.0. You currently have v{fa_v} installed. If the report download fails, please install a more stable version. We recommend v.0.5.0 or higher."))
    }
  }
  
  # Note that this overrides other credential set up
  login_creds <- NULL
  pre_auth_user <- NULL
  if (isFALSE(getOption("golem.app.prod"))) {
    arg_lst <- as.list(match.call())
  
    login_creds <- list(user_id = "",
                        user_pwd = "cxk1QEMYSpYcrNB")
  if (!is.null(arg_lst$pre_auth_user)) {
    pre_auth_user <- arg_lst$pre_auth_user
    if (isTRUE(pre_auth_user) || pre_auth_user == "admin") {
      login_creds$user_id <- "admin"
    } else {
      login_creds$user_id <- pre_auth_user
    }
    credentials_db_name <- gsub("\\.sqlite", "_dev\\.sqlite", credentials_db_name)
  }
  }
  
  # Run the app
  with_golem_options(
    app = shinyApp(
      ui = add_shinymanager_auth(app_ui, app_ver, login_note),
      server = app_server,
      onStart = onStart,
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list(app_version = app_ver,
                      credentials_db_name = credentials_db_name,
                      assessment_db_name = assessment_db_name,
                      pre_auth_user = pre_auth_user,
                      login_creds = login_creds,
                      decision_categories = decision_categories,
                      ...)
  )
}

