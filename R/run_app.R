#' Run the Shiny Application
#'
#' @param app_ver a "global" variable that is passed to several modules &
#'   reports which details the installed package version when not specified. It
#'   can be overwritten to include a specific version name as a text string.
#' @param login_note a text string to display underneath the auth screen's login
#'   button, provided to guide users
#' @param credentials_db_name a text string that names the credentials databse.
#'   Please make sure name ends with '.sqlite'. For example: 'cred_db.sqlite'.
#' @param assessment_db_name text string that names the credentials databse.
#'   Please make sure name ends with '.sqlite'. For example: 'assess_db.sqlite'.
#' @param pre_auth_user if `TRUE` or 'admin', run as admin, if 'nonadmin' run as
#'   non-admin
#' @param ... arguments to pass to golem_opts. See `?golem::get_golem_options`
#'   for more details.
#' @inheritParams shiny::shinyApp
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
  credentials_db_name = NULL,
  assessment_db_name = NULL,
  pre_auth_user = NULL, # TODO: Erase when pushing to master
  ...
) {
  # Pre-process some run-app inputs
  if(is.null(app_ver)) app_ver <- paste0(packageVersion("riskassessment"))
  if(is.null(assessment_db_name)) assessment_db_name <- "database.sqlite"
  if(is.null(credentials_db_name)) credentials_db_name <- "credentials.sqlite"
  if(is.null(login_note)) {
    # TODO: Remove temporary warning once bug in fa v0.4.0 is fixed.
    # https://github.com/rstudio/fontawesome/issues/99
    # Here, we make sure user has a functional version of fontawesome
    fa_v <- packageVersion("fontawesome") #TODO: Remove once bug is fixed
    if(!file.exists(credentials_db_name)) {
      login_note <- HTML('<em>Note:</em> To log in for the first time, use the admin user:
                          <u>admin</u> with password <u>QWERTY1</u>.')
    } else if(fa_v != '0.3.0') { #TODO: Remove once bug is fixed
      login_note <- HTML(glue::glue("<em>Note:</em> HTML reports may require fontawesome v0.3.0 to render. You currently have v{fa_v} installed. If the report download fails, please run: `remotes::install_version('fontawesome', version = '0.3.0', repos = 'http://cran.us.r-project.org')`"))
    }
  }
  
  # TODO: Erase when pushing to master
  # Note that this overrides other credential set up
  login_creds <- NULL
  if (!is.null(pre_auth_user)) {
    if (isTRUE(pre_auth_user) || pre_auth_user == "admin") {
      credentials_db_name <- "credentials_dev.sqlite"
      login_creds <- list(user_id = "admin",
                          user_pwd = "cxk1QEMYSpYcrNB")
    } else if (pre_auth_user == "nonadmin") {
      credentials_db_name <- "credentials_dev.sqlite"
      login_creds <- list(user_id = "nonadmin",
                          user_pwd = "Bt0dHK383lLP1NM")
    }
  }
  
  # this skips authentication if the application is running in test mode
  if (!isTRUE(getOption("shiny.testmode"))) {
    # if not test mode, append shinymanager login w/ legacy behavior
    app_ui <- add_tags(shinymanager::secure_app(app_ui,
      tags_top = tags$div(
        tags$head(tags$style(HTML(readLines(system.file("app", "www", "css", "login_screen.css", package = "riskassessment"))))),
        tags$head(if (!get_golem_config("app_prod") && !is.null(golem::get_golem_options("pre_auth_user"))) {
          tags$script(HTML(glue::glue("$(document).on('shiny:connected', function () {{
            Shiny.setInputValue('auth-user_id', '{golem::get_golem_options('login_creds')$user_id}');
            Shiny.setInputValue('auth-user_pwd', '{golem::get_golem_options('login_creds')$user_pwd}');
            $('#auth-go_auth').trigger('click');
          }});")))
        }),
        id = "login_screen",
        tags$h2("Risk Assessment Application", style = "align:center"),
        tags$h3(glue::glue("**Version {app_ver}**"),
                style = "align:center; color: darkgray"
        )
      ),
      tags_bottom = tags$div(
        tags$h6(login_note, style = "color: white")
      ),
      enable_admin = TRUE, theme = app_theme()
    ))
  }
  
  
  # Run the app
  with_golem_options(
    app = shinyApp(
      ui = app_ui,
      server = app_server,
      onStart = onStart,
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list(app_version = app_ver,
                      credentials_db_name = credentials_db_name,
                      assessment_db_name = assessment_db_name,
                      pre_auth_user = pre_auth_user, # TODO: Erase when pushing to master
                      login_creds = login_creds, # TODO: Erase when pushing to master
                      ...)
  )
}

