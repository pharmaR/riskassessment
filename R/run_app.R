#' Run the Shiny Application
#'
#' @param app_ver a "global" variable that is passed to several modules &
#'   reports which details the installed package version when not specified.
#'   It can be overwritten to include a specific version name as a text string.
#' @param login_note a text string to display underneath the auth screen's login
#'   button, provided to guide users
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
  app_ver = paste(packageVersion("riskassessment")),
  login_note = HTML('<em>Note:</em> To log in for the first time, use either the admin user:
                        <u>admin</u> or the non-admin user: <u>nonadmin</u>, both with
                        password <u>QWERTY1</u>.'),
  ...
) {
  assign(x = "app_version", value = app_ver, envir = .GlobalEnv)
  with_golem_options(
    app = shinyApp(
      ui = add_tags(shinymanager::secure_app(app_ui,
        tags_top = tags$div(
            tags$head(tags$style(HTML(readLines(system.file("app", "www", "css", "login_screen.css", package = "riskassessment"))))),
            id = "login_screen",
            tags$h2("Risk Assessment Application", style = "align:center"),
            tags$h3(glue::glue('**Version {app_version}**'),
                    style = "align:center; color: darkgray")),
        tags_bottom = tags$div(
          tags$h6(login_note, style = 'color: white')),
          enable_admin = TRUE, theme = app_theme)),
      server = app_server,
      onStart = onStart,
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list(...)
  )
}
