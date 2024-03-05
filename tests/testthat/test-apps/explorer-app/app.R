library(shiny)

ui <- fluidPage(
  riskassessment:::golem_add_external_resources(),
  tabsetPanel(
    id = "tabs",
    tabPanel(
      "src_expl_tab",
      riskassessment:::mod_pkg_explorer_ui("src_explorer")
    ),
    tabPanel(
      "fn_expl_tab",
      riskassessment:::mod_code_explorer_ui("fn_explorer")
    )
  )
)

server <- function(input, output, server) {
  shinyOptions(golem_options = list(assessment_db_name = "dplyr.sqlite"))
  
  selected_pkg <- list(name = reactiveVal("dplyr"), version = reactiveVal("1.1.2"))
  pkgarchive <- reactiveVal(archive::archive(file.path("tarballs", "dplyr_1.1.2.tar.gz")))
  user <- reactiveValues(
    name = "tester",
    role = "admin"
  )
  credential_config <- riskassessment:::get_db_config("credentials")
  
  riskassessment:::mod_pkg_explorer_server("src_explorer", selected_pkg,
                                           pkgarchive = pkgarchive,
                                           user = user,
                                           credentials = credential_config)
  
  riskassessment:::mod_code_explorer_server("fn_explorer", selected_pkg,
                                            pkgarchive = pkgarchive,
                                            user = user,
                                            credentials = credential_config)
}

shinyApp(ui, server)
