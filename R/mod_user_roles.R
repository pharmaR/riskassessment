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
    div(
      style = "float: right;",
      actionButton(ns("edit_dropdown"), label = icon("gear"), class = "btn-circle", style = "margin-left: auto;"),
      tags$script(glue::glue("$('#{ns(\"edit_dropdown\")}').tooltip({{placement: 'left', title: 'Click here to edit the user roles and privileges.', html: false, trigger: 'hover'}});"))
    ),
    h3("Roles", style = "padding-top: 15px"),
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
    
    roles_dbtbl <- reactiveVal({
      dbSelect("SELECT * FROM roles") %>%
        {rownames(.) <- .$user_role; .} %>%
        dplyr::select(-id, -user_role) %>%
        t()
      })
    
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
    
    output$modal_table <- 
      DT::renderDataTable({
        DT::datatable(
          isolate(roles_dbtbl()),
          escape = FALSE,
          class = "cell-border",
          selection = 'none',
          rownames = TRUE,
          options = list(
            dom = "t",
            searching = FALSE,
            drawCallback = DT::JS(
              "function ( settings ) {",
              "  $(':checkbox[row]').on('click', function() {",
              "    var row = Number(this.getAttribute('row')) + 1",
              "    var col = Number(this.getAttribute('col'))",
              "    var value = $(this).is(':checked') ? 1 : 0",
              "    var info = [{row: row, col: col, value: value}]",
              glue::glue("    Shiny.setInputValue('{ns(\"modal_table\")}_cell_edit:DT.cellInfo', info)"),
              "    console.log(info)",
              "  })",
              "}"
              ),
            sScrollX = "100%",
            iDisplayLength = -1,
            ordering = FALSE,
            columnDefs = list(list(
                targets = "_all",
                render = DT::JS(
                  "function(data, type, row, meta) {",
                  "  if(meta.col != 0){",
                  "    return data ? `<input type=\"checkbox\" row=${meta.row} col=${meta.col} checked/>` : `<input type=\"checkbox\" row=${meta.row} col=${meta.col} />`;", 
                  "  }",
                  "  return data;",
                  "}"
                )
              ))
          ))
      })
    
    observeEvent(input$edit_dropdown, {
      
      showModal(modalDialog(
        size = "l",
        DT::DTOutput(ns("modal_table"))
      ))
    })
    proxy <- DT::dataTableProxy("modal_table")
    
    observeEvent(input$modal_table_cell_edit, {
      roles_dbtbl(DT::editData(roles_dbtbl(), input$modal_table_cell_edit))
      DT::replaceData(proxy, roles_dbtbl(), resetPaging = FALSE)
    })
 
  })
}
    
## To be copied in the UI
# mod_user_roles_ui("user_roles_1")
    
## To be copied in the server
# mod_user_roles_server("user_roles_1")
