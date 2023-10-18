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
mod_user_roles_server <- function(id, user, credentials){
  if (missing(credentials))
    credentials <- get_db_config("credentials")
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    initial_tbl <- get_roles_table()
    roles_dbtbl <- reactiveVal(initial_tbl)
    proxy_tbl <- reactiveVal()
    observeEvent(roles_dbtbl(), {
      proxy_tbl(roles_dbtbl())
    })
    
    user_table <- reactiveVal()
    
    role_changes <- reactiveVal(dplyr::tibble(old_role = colnames(initial_tbl), new_role = colnames(initial_tbl)))
    
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
        i <- match("admin", rownames(roles_dbtbl()))
        j <- match(role_changes() %>% dplyr::filter(old_role == user$role) %>% dplyr::pull(new_role), colnames(roles_dbtbl()))
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
                render = DT::JS(glue::glue(
                  "function(data, type, row, meta) {{",
                  "  if(meta.col != 0){{",
                  "    return `<input type=\"checkbox\" ${{meta.row == {i-1} & meta.col == {j} ? 'disabled' : ''}} row=${{meta.row}} col=${{meta.col}} ${{data ? 'checked' : ''}}/>`;", 
                  "  }}",
                  "  return data;",
                  "}}"
                ))
              ))
          ))
      }) %>%
      bindEvent(roles_dbtbl())
    
    observeEvent(input$edit_dropdown, {
      user_table(get_credentials_table(passphrase = passphrase))
      used_roles <- role_changes() %>% dplyr::filter(old_role %in% user_table()$role) %>% dplyr::pull(new_role)
      showModal(modalDialog(
        size = "l",
        footer = tagList(
          actionButton(ns("close_decision_modal"), "Close")
          ),
        div(
          style = "display: flex",
          span("Edit User Roles", style = "font-size: x-large; font-weight: bold"),
          actionButton(ns("tbl_reset"), label = icon("refresh"), class = "btn-circle-sm", style = "margin-left: auto;")
        ),
        tags$label("Add Role", class = "control-label"),
        div(
          style = "display: flex",
          textInput(ns("add_col"), NULL, width = "50%"),
          actionButton(ns("add_col_submit"), shiny::icon("plus"),
                       style = 'height: calc(1.5em + 1.5rem + 2px)')
          ),
        tags$label("Edit Role Name", class = "control-label"),
        div(
          style = "display: flex",
          selectInput(ns("select_edit_col"), NULL, choices = colnames(proxy_tbl()), width = "25%"),
          textInput(ns("edit_col"), NULL, width = "25%"),
          actionButton(ns("edit_col_submit"), shiny::icon("pen-to-square"),
                       style = 'height: calc(1.5em + 1.5rem + 2px)')
        ),
        tags$label("Delete Role", icon("circle-info", class = "fa-xs", title = "A role can only be deleted if no users are assigned to it. If the role is not visible, first ensure no users are assigned that role in the Credential Manager."), class = "control-label"),
        div(
          style = "display: flex",
          selectInput(ns("delete_col"), NULL, choices = setdiff(colnames(proxy_tbl()), used_roles), width = "50%") %>%
            tagAppendAttributes(class = if(length(setdiff(colnames(proxy_tbl()), used_roles)) == 0) "shinyjs-disabled"),
          actionButton(ns("delete_col_submit"), shiny::icon("trash-can"),
                       style = 'height: calc(1.5em + 1.5rem + 2px)')
        ),
        span("Edit Role Privileges", style = "font-size: x-large; font-weight: bold"),
        DT::DTOutput(ns("modal_table")),
        br(),
        actionButton(ns("submit_changes"), width = "100%", "Apply Changes to Roles & Privileges")
      ))
    })
    proxy <- DT::dataTableProxy("modal_table")
    
    observeEvent(input$close_decision_modal, {
      removeModal()
      shinyjs::runjs("document.body.setAttribute('data-bs-overflow', 'auto');")
    })
    
    observeEvent(input$tbl_reset, {
      reset_table <- get_roles_table()
      roles_dbtbl(reset_table)
      role_changes(dplyr::tibble(old_role = colnames(reset_table), new_role = colnames(reset_table)))
      used_roles <- role_changes() %>% dplyr::filter(old_role %in% user_table()$role) %>% dplyr::pull(new_role)
      
      updateTextInput(session, "add_col", value = "")
      updateSelectInput(session, "select_edit_col", choices = colnames(reset_table))
      updateTextInput(session, "edit_col", value = "")
      updateSelectInput(session, "delete_col", choices = setdiff(colnames(reset_table), used_roles))
      if (length(setdiff(colnames(reset_table), used_roles)) == 0)
        shinyjs::disable("delete_col")
      else
        shinyjs::enable("delete_col")
    })
    
    observeEvent(input$modal_table_cell_edit, {
      i <- match("admin", rownames(roles_dbtbl()))
      j <- match(user$role, colnames(roles_dbtbl()))
      req(i != input$modal_table_cell_edit$row || j != input$modal_table_cell_edit$col)
      
      proxy_tbl(DT::editData(proxy_tbl(), input$modal_table_cell_edit))
      DT::replaceData(proxy, proxy_tbl(), resetPaging = FALSE)
    })
    
    observeEvent(input$add_col_submit, {
      req(input$add_col)
      req(!input$add_col %in% c("", role_changes()$new_role))
      
      tbl <- cbind(proxy_tbl(), 0)
      colnames(tbl) <- c(colnames(proxy_tbl()), input$add_col)
      roles_dbtbl(tbl)
      role_changes(dplyr::add_row(role_changes(), new_role = input$add_col))
      used_roles <- role_changes() %>% dplyr::filter(old_role %in% user_table()$role) %>% dplyr::pull(new_role)
      
      updateTextInput(session, "add_col", value = "")
      updateSelectInput(session, "select_edit_col", choices = colnames(tbl))
      updateTextInput(session, "edit_col", value = "")
      updateSelectInput(session, "delete_col", choices = setdiff(colnames(tbl), used_roles))
      shinyjs::enable("delete_col")
      
    })
    
    observeEvent(input$edit_col_submit, {
      req(input$edit_col)
      req(!input$edit_col %in% c("", role_changes()$new_role))
      
      tbl <- proxy_tbl()
      i <- match(input$select_edit_col, colnames(tbl))
      colnames(tbl)[i] <- input$edit_col
      roles_dbtbl(tbl)
      role_changes(dplyr::mutate(role_changes(), 
                                 new_role = if_else(new_role == input$select_edit_col, 
                                                    input$edit_col, 
                                                    new_role)))
      used_roles <- role_changes() %>% dplyr::filter(old_role %in% user_table()$role) %>% dplyr::pull(new_role)
      
      updateTextInput(session, "add_col", value = "")
      updateSelectInput(session, "select_edit_col", choices = colnames(tbl))
      updateTextInput(session, "edit_col", value = "")
      updateSelectInput(session, "delete_col", choices = setdiff(colnames(tbl), used_roles))
    })
    
    observeEvent(input$delete_col_submit, {
      o_role <- role_changes() %>% dplyr::filter(new_role == input$delete_col) %>% dplyr::pull(name = old_role)
      req(!o_role %in% user_table()$role)
      
      tbl <- proxy_tbl()
      i <- match(input$delete_col, colnames(tbl))
      tbl <- tbl[,-i]
      roles_dbtbl(tbl)
      role_changes(dplyr::mutate(role_changes(), 
                                 new_role = if_else(new_role == input$delete_col, 
                                                    NA_character_, 
                                                    new_role)))
      used_roles <- role_changes() %>% dplyr::filter(old_role %in% user_table()$role) %>% dplyr::pull(new_role)
      
      updateTextInput(session, "add_col", value = "")
      updateSelectInput(session, "select_edit_col", choices = colnames(tbl))
      updateTextInput(session, "edit_col", value = "")
      updateSelectInput(session, "delete_col", choices = setdiff(colnames(tbl), used_roles))
      if (length(setdiff(colnames(tbl), used_roles)) == 0)
        shinyjs::disable("delete_col")
      else
        shinyjs::enable("delete_col")
    })
    
    observeEvent(input$submit_changes, {
      req("admin" %in% credentials$privileges[[user$role]])
      
      chng_lst <- dplyr::filter(role_changes(), paste(old_role) != paste(new_role))
      purrr::pmap(chng_lst, function(old_role, new_role) {
        cmd <- dplyr::case_when(
          is.na(old_role) ~ "INSERT INTO roles (user_role) VALUES ({new_role})",
          is.na(new_role) ~ "DELETE FROM roles WHERE user_role = {old_role}",
          TRUE ~ "UPDATE roles SET user_role = {new_role} WHERE user_role = {old_role}"
        )
        dbUpdate(cmd)
      })
      purrr::iwalk(as.data.frame(proxy_tbl()), ~ dbUpdate(glue::glue("UPDATE roles SET {paste(used_privileges, ' = ', .x, collapse = ', ')} WHERE user_role = '{.y}'")))
      
      updated_user_tbl <-
        user_table() %>%
        dplyr::rowwise() %>%
        dplyr::mutate(
          role = role_changes() %>% `[`(!is.na(.$old_role) & .$old_role == role, "new_role") %>% `[[`(1),
          admin = purrr::map(role, ~ dplyr::if_else(proxy_tbl()["admin", .x] == 1, 'TRUE', 'FALSE')) %>% unlist()
        )
      set_credentials_table(updated_user_tbl, passphrase = passphrase)
      
      user$role <- role_changes() %>% `[`(!is.na(.$old_role) & .$old_role == user$role, "new_role") %>% `[[`(1)
      
      update_tbl <- get_roles_table()
      roles_dbtbl(update_tbl)
      role_changes(dplyr::tibble(old_role = colnames(update_tbl), new_role = colnames(update_tbl)))
      
      purrr::iwalk(get_credential_config(), ~ `<-`(credentials[[.y]], .x))
      
      user_table(get_credentials_table(passphrase = passphrase))
      
      session$userData$trigger_events[["reset_sidebar"]] <- session$userData$trigger_events[["reset_sidebar"]] + 1
      
      removeModal()
      shinyjs::runjs("document.body.setAttribute('data-bs-overflow', 'auto');")
    })
 
  })
}
    
## To be copied in the UI
# mod_user_roles_ui("user_roles_1")
    
## To be copied in the server
# mod_user_roles_server("user_roles_1")
