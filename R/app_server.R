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
  
  old <- options()
  onStop(function() {
    unlink("source/*", recursive = TRUE)
    options(old)
    })
  options(repos = get_db_config("package_repo"))
  
  # Collect user info.
  user <- reactiveValues()
  credential_config <- do.call(reactiveValues, get_credential_config())
  role_opts <- reactiveValues()
  observe({
    role_opts[["admin"]] <- purrr::imap(credential_config$privileges, ~ if ("admin" %in% .x) .y) %>% unlist(use.names = FALSE) %>% as.list()
    role_opts[["nonadmin"]] <- as.list(setdiff(credential_config$roles, unlist(role_opts$admin)))
  }) %>%
    bindEvent(credential_config$roles, credential_config$privileges)
  session$userData$trigger_events <- reactiveValues(
    reset_pkg_upload = 0,
    reset_sidebar = 0,
    upload_pkgs = NULL,
    update_report_pref_inclusions = 0
  )
  session$userData$repo_pkgs <- reactiveVal()
  
  
  # this skips authentication if the application is running in test mode
  if (isTRUE(getOption("shiny.testmode"))) {
    # mock what is returned by shinymanager::secure_server
    res_auth <- reactiveValues()
    res_auth[["admin"]] <- !isTRUE(golem::get_golem_options('nonadmin'))
    res_auth[["user"]] <- "test_user"
    res_auth[["role"]] <- ifelse(!isTRUE(golem::get_golem_options('nonadmin')), "admin", "reviewer")
  } else if (isFALSE(get_db_config("use_shinymanager"))) {
    res_auth <- reactiveValues()
    res_auth[["admin"]] <- FALSE
    res_auth[["user"]] <- session$user %||% "anonymous"
    res_auth[["role"]] <- intersect(unlist(session$groups, use.names = FALSE), dbSelect("select user_role from roles")[[1]]) %||% "default"
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
    req(res_auth$admin == TRUE || any(c("admin", "weight_adjust") %in% unlist(credential_config$privileges[res_auth$role])))
    
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
                    if (res_auth$admin == TRUE || "admin" %in% unlist(credential_config$privileges[res_auth$role]))
                      tabPanel(
                        id = "privilege_id",
                        title = "Roles & Privileges",
                        mod_user_roles_ui("userRoles")
                      ),
                    if ("weight_adjust" %in% unlist(credential_config$privileges[res_auth$role]))
                    tabPanel(
                      id = "reweight_id",
                      title = "Assessment Reweighting",
                      reweightViewUI("reweightInfo")
                    )
                  ),
                  tags$script(HTML("document.getElementById('admin-add_user').style.width = 'auto';"))
                ))
  }, priority = 1)
  
  observeEvent(credential_config$privileges, {
    req(user$role)
    
    if ("weight_adjust" %in% unlist(credential_config$privileges[user$role]))
      showTab("credentials", "Assessment Reweighting")
    else
      hideTab("credentials", "Assessment Reweighting")
  })
  
  purrr::walk(paste("admin", c("edited_user", "edited_mult_user", "delete_selected_users", "delete_user", "changed_password", "changed_password_users"), sep = "-"),
              ~ observeEvent(input[[.x]], removeModal(), priority = 1))
  
  purrr::walk(c("admin-reseted_password", "admin-changed_password", "admin-added_user"),
              ~ observeEvent(input[[.x]], shinyjs::runjs("document.body.setAttribute('data-bs-overflow', 'auto');"), priority = -1))
  
  purrr::walk(paste("admin", c("edit_mult_user", "edit_user", "add_user"), sep = "-"),
              function(.x) {
                y <- ifelse(.x == "admin-edit_mult_user", "admin-edit_selected_users", .x)
                observeEvent(input[[y]], {
                  shinyjs::runjs(paste0("document.getElementById('", .x, c("-start-", "-expire-", "-user-"), "label').innerHTML = ", c("'Start Date'", "'Expiration Date'", "'User Name'"), collapse = ";\n"))
                  role_lst <- list(id = .x, role_opts = reactiveValuesToList(role_opts))
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
  
  mod_user_roles_server("userRoles", user, credential_config)
  
  # Load server of the reweightView module.
  metric_weights <- reweightViewServer("reweightInfo", user, auto_decision$rules, credential_config)
  
  # Load server of the uploadPackage module.
  auto_decision <- mod_decision_automation_server("automate", user, credential_config)
  uploaded_pkgs <- uploadPackageServer("upload_package", user, auto_decision$rules, credential_config, parent = session)
  
  # Load server of the sidebar module.
  selected_pkg <- sidebarServer("sidebar", user, uploaded_pkgs, credential_config)

  changes <- reactiveVal(0)
  observe({
    changes(changes() + 1)
  }) %>%
    bindEvent(selected_pkg$decision(), selected_pkg$overall_comment_added())
  
  session$userData$user_report <- reactiveValues()
  observe({
    req(user$name)
    default_dir <- get_db_config("report_prefs")[["directory"]]
    if(!file.exists(default_dir)) dir.create(default_dir)
    
    # retrieve user data, if it exists.  Otherwise use rpt_choices above.
    session$userData$user_report$user_file <- file.path(default_dir, glue::glue("report_prefs_{user$name}.txt"))
    if (file.exists(session$userData$user_report$user_file)) {
      session$userData$user_report$report_includes <- readLines(session$userData$user_report$user_file)
    } else {
      session$userData$user_report$report_includes <- rpt_choices
    }
  })
  
  observeEvent(input$apptabs, {
    req(input$apptabs %in% c("risk-assessment-tab", "database-tab"))
    session$userData$trigger_events$update_report_pref_inclusions <- session$userData$trigger_events$update_report_pref_inclusions + 1
  })
  
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
    
    # Collect all the metric names and values associated to package name.
    get_metric_data(selected_pkg$name(), metric_class = "maintenance")
  })
  
  # Gather community usage metrics information.
  community_usage_metrics <- reactive({
    req(selected_pkg$name())
    req(selected_pkg$name() != "-")
    
    get_comm_data(selected_pkg$name())
  })
  
  create_src_dir <- eventReactive(input$tabs, input$tabs == "Source Explorer")
  pkgdir <- reactiveVal()
  observe({
    req(selected_pkg$name() != "-")
    req(create_src_dir())
    req(file.exists(file.path("tarballs", glue::glue("{selected_pkg$name()}_{selected_pkg$version()}.tar.gz"))))
    
    src_dir <- file.path("source", selected_pkg$name())
    if (dir.exists(src_dir)) {
      pkgdir(src_dir)
    } else {
      withProgress(
        utils::untar(file.path("tarballs", glue::glue("{selected_pkg$name()}_{selected_pkg$version()}.tar.gz")), exdir = "source"),
        message = glue::glue("Unpacking {selected_pkg$name()}_{selected_pkg$version()}.tar.gz"),
        value = 1
      )
      pkgdir(src_dir)
    }
  }) %>% 
    bindEvent(selected_pkg$name(), create_src_dir())
  
  
  mod_pkg_explorer_server("pkg_explorer", selected_pkg,
                          pkgdir = pkgdir,
                          creating_dir = create_src_dir,
                          user = user,
                          credentials = credential_config)
  
  mod_code_explorer_server("code_explorer", selected_pkg,
                          pkgdir = pkgdir,
                          creating_dir = create_src_dir,
                          user = user,
                          credentials = credential_config)
  
  # Load server for the maintenance metrics tab.
  maintenance_data <- maintenanceMetricsServer('maintenanceMetrics',
                                               selected_pkg,
                                               maint_metrics,
                                               user,
                                               credential_config,
                                               parent = session)
  
  # Load server for the community metrics tab.
  community_data <- communityMetricsServer('communityMetrics',
                                           selected_pkg,
                                           community_usage_metrics,
                                           user,
                                           credential_config)
  
  # Load server of the report preview tab.
  reportPreviewServer(id = "reportPreview",
                      selected_pkg = selected_pkg,
                      maint_metrics = maint_metrics,
                      com_metrics = community_data$cards,
                      com_metrics_raw = community_usage_metrics,
                      mm_comments = maintenance_data$comments,
                      cm_comments = community_data$comments,
                      # se_comments = src_explorer_data$comments, # not an arg
                      downloads_plot_data = community_data$downloads_plot_data,
                      user = user,
                      credential_config,
                      app_version = golem::get_golem_options('app_version'),
                      metric_weights = metric_weights)
  
  # Load server for the package dependencies tab.
  dependencies_data <- packageDependenciesServer('packageDependencies',
                                               selected_pkg,
                                               user,
                                               parent = session)
  
  output$auth_output <- renderPrint({
    reactiveValuesToList(res_auth)
  })
}
