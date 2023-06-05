#' user_roles UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
mod_user_roles_ui <- function(id){
  ns <- NS(id)
  column(
    width = 10, offset = 1,
    h3("Roles"),
    hr(),
    DT::dataTableOutput(ns("roles_table")),
    br(),
    h3("Privileges"),
    hr(),
    DT::dataTableOutput(ns("privileges_table"))
  )
}
    
#' user_roles Server Functions
#'
#' @noRd 
mod_user_roles_server <- function(id, credentials){
  if (missing(credentials))
    credentials <- get_golem_config("credentials", file = app_sys("db-config.yml"))
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$roles_table <-
      DT::renderDataTable(
        DT::datatable(
          dplyr::tibble(Role = credentials$roles) %>% 
            dplyr::rowwise() %>% 
            dplyr::mutate("Privilege(s)" = paste(credentials$privileges[[Role]], collapse = ", ")),
          escape = FALSE,
          class = "cell-border",
          selection = 'none',
          rownames = FALSE,
          options = list(
            dom = "t",
            iDisplayLength = -1,
            ordering = FALSE
          )
        )
      )
    
    output$privileges_table <-
      DT::renderDataTable(
        DT::datatable(
          privileges_tbl %>%
            dplyr::rowwise() %>%
            dplyr::mutate(`Applicable Role(s)` = purrr::map(credentials$privileges, ~ .x[.x == Privilege]) %>% purrr::compact() %>% names() %>% paste(collapse = ", ")),
          escape = FALSE,
          class = "cell-border",
          selection = 'none',
          rownames = FALSE,
          options = list(
            dom = "t",
            iDisplayLength = -1,
            ordering = FALSE
          )
        )
      )
 
  })
}
    
## To be copied in the UI
# mod_user_roles_ui("user_roles_1")
    
## To be copied in the server
# mod_user_roles_server("user_roles_1")
