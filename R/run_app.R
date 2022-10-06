#' Run the Shiny Application
#'
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(
  onStart = NULL,
  options = list(),
  enableBookmarking = NULL,
  uiPattern = "/",
  ...
) {
  with_golem_options(
    app = shinyApp(
      ui = add_tags(shinymanager::secure_app(app_ui,
                                    tags_top = tags$div(
                                      tags$link(rel = "stylesheet", type = "text/css",
                                                href = file.path('css', 'login_screen.css')),
                                      id = "login_screen",
                                      tags$h2("Risk Assessment Application", style = "align:center"),
                                      tags$h3(glue::glue('**Version {app_version}**'),
                                              style = "align:center; color: darkgray")),
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
