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
    proxy_tbl <- reactiveVal()
    observeEvent(roles_dbtbl(), {
      proxy_tbl(roles_dbtbl())
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
          roles_dbtbl(),
          escape = FALSE,
          class = "cell-border",
          selection = 'none',
          rownames = TRUE,
          extensions = "FixedColumns",
          options = list(
            dom = "t",
            searching = FALSE,
            fixedColumns = list(leftColumns = 1),
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
        tags$label("Add Role", class = "control-label"),
        div(
          style = "display: flex",
          textInput(ns("add_col"), NULL),
          actionButton(ns("add_col_submit"), shiny::icon("angle-right"),
                       style = 'height: calc(1.5em + 1.5rem + 2px)')
          ),
        tags$label("Edit Role", class = "control-label"),
        div(
          style = "display: flex",
          selectInput(ns("select_edit_col"), NULL, choices = colnames(proxy_tbl())),
          textInput(ns("edit_col"), NULL),
          actionButton(ns("edit_col_submit"), shiny::icon("angle-right"),
                       style = 'height: calc(1.5em + 1.5rem + 2px)')
        ),
        tags$label("Delete Role", class = "control-label"),
        div(
          style = "display: flex",
          selectInput(ns("delete_col"), NULL, choices = colnames(proxy_tbl())),
          actionButton(ns("delete_col_submit"), shiny::icon("trash-can"),
                       style = 'height: calc(1.5em + 1.5rem + 2px)')
        ),
        DT::DTOutput(ns("modal_table"))
      ))
    })
    proxy <- DT::dataTableProxy("modal_table")
    
    observeEvent(input$modal_table_cell_edit, {
      proxy_tbl(DT::editData(proxy_tbl(), input$modal_table_cell_edit))
      DT::replaceData(proxy, proxy_tbl(), resetPaging = FALSE)
    })
    
    observeEvent(input$add_col_submit, {
      req(input$add_col)
      req(!input$add_col %in% c(""))
      
      tbl <- cbind(proxy_tbl(), 0)
      colnames(tbl) <- c(colnames(proxy_tbl()), input$add_col)
      roles_dbtbl(tbl)
      updateTextInput(session, "add_col", value = "")
      updateSelectInput(session, "select_edit_col", choices = colnames(tbl))
      updateTextInput(session, "edit_col", value = "")
      updateSelectInput(session, "delete_col", choices = colnames(tbl))
    })
    
    observeEvent(input$edit_col_submit, {
      req(input$edit_col)
      req(input$edit_col != input$select_edit_col)
      req(!input$edit_col %in% c(""))
      
      tbl <- proxy_tbl()
      i <- match(input$select_edit_col, colnames(tbl))
      colnames(tbl)[i] <- input$edit_col
      roles_dbtbl(tbl)
      updateTextInput(session, "add_col", value = "")
      updateSelectInput(session, "select_edit_col", choices = colnames(tbl))
      updateTextInput(session, "edit_col", value = "")
      updateSelectInput(session, "delete_col", choices = colnames(tbl))
    })
    
    observeEvent(input$delete_col_submit, {
      
      tbl <- proxy_tbl()
      i <- match(input$delete_col, colnames(tbl))
      roles_dbtbl(tbl[,-i])
      updateTextInput(session, "add_col", value = "")
      updateSelectInput(session, "select_edit_col", choices = colnames(tbl))
      updateTextInput(session, "edit_col", value = "")
      updateSelectInput(session, "delete_col", choices = colnames(tbl))
    })
 
  })
}
    
## To be copied in the UI
# mod_user_roles_ui("user_roles_1")
    
## To be copied in the server
# mod_user_roles_server("user_roles_1")
