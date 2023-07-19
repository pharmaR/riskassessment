#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' 
#' @importFrom shinyjs show hide delay runjs
#' @importFrom shinymanager secure_server check_credentials
#' @importFrom loggit loggit
#' @noRd
app_server <- function(input, output, session) {
  
  onStop(function() {
    unlink("source/*", recursive = TRUE)
    })
  
  # Collect user info.
  user <- reactiveValues()
  credential_config <- get_golem_config("credentials", file = app_sys("db-config.yml"))
  role_opts <- list(admin = purrr::imap(credential_config$privileges, ~ if ("admin" %in% .x) .y) %>% unlist(use.names = FALSE) %>% as.list())
  role_opts[["nonadmin"]] <- as.list(setdiff(credential_config$roles, unlist(role_opts$admin)))
  approved_roles <- credential_config[["privileges"]]
  trigger_events <- reactiveValues(
    reset_pkg_upload = 0,
    reset_sidebar = 0
  )
  
  
  # this skips authentication if the application is running in test mode
  if (isTRUE(getOption("shiny.testmode"))) {
    # mock what is returned by shinymanager::secure_server
    res_auth <- reactiveValues()
    res_auth[["admin"]] <- !isTRUE(golem::get_golem_options('nonadmin'))
    res_auth[["user"]] <- "test_user"
    res_auth[["role"]] <- ifelse(!isTRUE(golem::get_golem_options('nonadmin')), "admin", "reviewer")
  } else {
    # check_credentials directly on sqlite db
    res_auth <- shinymanager::secure_server(
      check_credentials = shinymanager::check_credentials(
        golem::get_golem_options('credentials_db_name'),
        passphrase = passphrase
      )
    )
  }

  
  observeEvent(res_auth$user, {
    dbUpdate("DELETE FROM _variables")
    dbUpdate("INSERT INTO _variables (user) VALUES ({res_auth$user})")
    
    req(res_auth$admin == TRUE | "weight_adjust" %in% approved_roles[[res_auth$role]])
    
      appendTab("apptabs",
                tabPanel(
                  title = "Administrative Tools",
                  icon = icon("gears"),
                  value = "admin-mode-tab",
                  h2("Administrative Tools & Options", align = "center", `padding-bottom`="20px"),
                  br(),
                  tabsetPanel(
                    id = "credentials",
                    if (res_auth$admin)
                      tabPanel(
                        id = "credentials_id",
                        title = "Credential Manager",
                        shinymanager:::admin_ui("admin")
                      ),
                    if (res_auth$admin)
                      tabPanel(
                        id = "privilege_id",
                        title = "Roles & Privileges",
                        mod_user_roles_ui("userRoles")
                      ),
                    if ("weight_adjust" %in% approved_roles[[res_auth$role]])
                      tabPanel(
                        id = "reweight_id",
                        title = "Assessment Reweighting",
                        reweightViewUI("reweightInfo")
                      )
                  ),
                  tags$script(HTML("document.getElementById('admin-add_user').style.width = 'auto';"))
                ))
  }, priority = 1)
  
  purrr::walk(paste("admin", c("edited_user", "edited_mult_user", "delete_selected_users", "delete_user", "changed_password", "changed_password_users"), sep = "-"),
              ~ observeEvent(input[[.x]], removeModal(), priority = 1))
  
  purrr::walk(c("admin-reseted_password", "admin-changed_password", "admin-added_user"),
              ~ observeEvent(input[[.x]], shinyjs::runjs("document.body.setAttribute('data-bs-overflow', 'auto');"), priority = -1))
  
  purrr::walk(paste("admin", c("edit_mult_user", "edit_user", "add_user"), sep = "-"),
              function(.x) {
                y <- ifelse(.x == "admin-edit_mult_user", "admin-edit_selected_users", .x)
                observeEvent(input[[y]], {
                  shinyjs::runjs(paste0("document.getElementById('", .x, c("-start-", "-expire-", "-user-"), "label').innerHTML = ", c("'Start Date'", "'Expiration Date'", "'User Name'"), collapse = ";\n"))
                  role_lst <- list(id = .x, role_opts = role_opts)
                  # Send the roles to Javascript side for processing
                  if (!grepl("edit_mult_user", .x))
                    session$sendCustomMessage("roles", role_lst)
                  else
                    shinyjs::runjs(glue::glue('$("#{.x}-role").closest("div").remove()'))
                }, priority = -1)
              })
  
  purrr::walk(paste("admin", c("edited_user", "edited_mult_user", "added_user", "changed_password", "reset_pwd", "changed_password_users"), sep = "-"),
              ~ observeEvent(input[[.x]], {
                shinyjs::delay(1000,
                               shinyjs::runjs("
                   var elements = document.getElementsByClassName('shiny-notification');
                   var sendToR = [];
                   for (var i = 0; i < elements.length; i++) {
                      sendToR.push(elements[i].id);
                   }
                   Shiny.onInputChange('shinyjs-returns', sendToR)
                   "))
              }, priority = -2))
  
  observeEvent(input$`shinyjs-returns`, {
    purrr::walk(input$`shinyjs-returns`, ~ removeNotification(stringr::str_remove(.x, "shiny-notification-")))
  })
  
  observeEvent(input$`table_users-returns`, {
    shinyjs::runjs("
                   $($('#admin-table_users').find('table').DataTable().column(0).header()).text('user name');
                   $($('#admin-table_users').find('table').DataTable().column(1).header()).text('start date');
                   $($('#admin-table_users').find('table').DataTable().column(2).header()).text('expiration date');")
  })
  
  observeEvent(input$`table_pwds-returns`, {
    shinyjs::runjs("
                   $($('#admin-table_pwds').find('table').DataTable().column(0).header()).text('user name');
                   $($('#admin-table_pwds').find('table').DataTable().column(3).header()).text('date last changed');")
  })
  
  # Save user name and role.  
  observeEvent(res_auth$user, {
    if (res_auth$admin == TRUE)
      loggit::loggit("INFO", glue::glue("User {res_auth$user} signed on as admin"))
    
    user$name <- trimws(res_auth$user)
    user$admin <- isTRUE(res_auth$admin) || res_auth$admin == "TRUE"
    user$role <- trimws(res_auth$role)
  })
  
  mod_user_roles_server("userRoles", credential_config)
  
  # Load server of the reweightView module.
  metric_weights <- reweightViewServer("reweightInfo", user, auto_decision$rules, approved_roles, trigger_events)
  
  # Load server of the uploadPackage module.
  auto_decision <- mod_decision_automation_server("automate", user, approved_roles)
  uploaded_pkgs <- uploadPackageServer("upload_package", user, auto_decision$rules, approved_roles, trigger_events)
  
  # Load server of the sidebar module.
  selected_pkg <- sidebarServer("sidebar", user, uploaded_pkgs, approved_roles, trigger_events)

  changes <- reactiveVal(0)
  observe({
    changes(changes() + 1)
  }) %>%
    bindEvent(selected_pkg$decision(), selected_pkg$overall_comment_added())
  
  # Load server of the assessment criteria module.
  assessmentInfoServer("assessmentInfo", metric_weights = metric_weights)
  
  # Load server of the database view module.
  #parentSession <- .subset2(session, "parent")
  databaseViewServer("databaseView", user, uploaded_pkgs,
                     metric_weights = metric_weights, changes, parent = session)
  
  # Gather maintenance metrics information.
  maint_metrics <- reactive({
    req(selected_pkg$name())
    req(selected_pkg$name() != "-")
    
    # Collect all the metric names and values associated to package_id.
    get_mm_data(selected_pkg$id())
  })
  
  
  # Gather community usage metrics information.
  community_usage_metrics <- reactive({
    req(selected_pkg$name())
    req(selected_pkg$name() != "-")
    
    get_comm_data(selected_pkg$name())
  })
  
  create_src_dir <- eventReactive(input$tabs, input$tabs == "Source Explorer")
  
  mod_pkg_explorer_server("pkg_explorer", selected_pkg, create_dir = create_src_dir)
  
  # Load server for the maintenance metrics tab.
  maintenance_data <- maintenanceMetricsServer('maintenanceMetrics',
                                               selected_pkg,
                                               maint_metrics,
                                               user,
                                               approved_roles,
                                               parent = session)
  
  # Load server for the community metrics tab.
  community_data <- communityMetricsServer('communityMetrics',
                                           selected_pkg,
                                           community_usage_metrics,
                                           user,
                                           approved_roles)
  
  # Load server of the report preview tab.
  reportPreviewServer(id = "reportPreview",
                      selected_pkg = selected_pkg,
                      maint_metrics = maint_metrics,
                      com_metrics = community_data$cards,
                      com_metrics_raw = community_usage_metrics,
                      mm_comments = maintenance_data$comments,
                      cm_comments = community_data$comments,
                      downloads_plot_data = community_data$downloads_plot_data,
                      user = user,
                      approved_roles,
                      app_version = golem::get_golem_options('app_version'),
                      metric_weights = metric_weights)
  
  output$auth_output <- renderPrint({
    reactiveValuesToList(res_auth)
  })
}
