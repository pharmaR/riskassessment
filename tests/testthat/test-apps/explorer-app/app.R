library(shiny)

ui <- fluidPage(
  riskassessment:::golem_add_external_resources(),
  tabsetPanel(
    id = "tabs",
    tabPanel(
      "Source Explorer",
      id = "src_expl_tab",
      riskassessment:::mod_pkg_explorer_ui("src_explorer")
    ),
    tabPanel(
      "Function Explorer",
      id = "fn_expl_tab",
      riskassessment:::mod_code_explorer_ui("fn_explorer")
    )
  )
)

server <- function(input, output, server) {
  
  selected_pkg <- list(name = reactiveVal("magrittr"), version = reactiveVal("2.0.3"))
  pkgdir <- reactiveVal(file.path("source", "magrittr"))
  user <- reactiveValues(
    name = "tester",
    role = "admin"
  )
  credential_config <- riskassessment:::get_db_config("credentials")
  
  riskassessment:::mod_pkg_explorer_server("src_explorer", selected_pkg,
                                           pkgdir = pkgdir,
                                           user = user,
                                           credentials = credential_config)
  
  riskassessment:::mod_code_explorer_server("fn_explorer", selected_pkg,
                                            pkgdir = pkgdir,
                                            user = user,
                                            credentials = credential_config)
}

shinyApp(ui, server)
