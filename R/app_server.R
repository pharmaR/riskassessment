#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  
  # check_credentials directly on sqlite db
  res_auth <- secure_server(
    check_credentials = check_credentials(
      file.path("credentials.sqlite"),
      passphrase = key_get("R-shinymanager-key", getOption("keyring_user"))
    )
  )
  
  output$auth_output <- renderPrint({
    reactiveValuesToList(res_auth)
  })
  
  # Load reactive values into values.
  values <- reactiveValues()
  values$current_screen <- "dashboard_screen" 
  values$uploaded_file_status <- "no_status"
  values$upload_complete <- "upload_incomplete"
  values$select_pack <- "Select"
  
  observeEvent(res_auth$user,{
    # log any admin sign-ons
    if (res_auth$admin == TRUE) {
      loggit("INFO", paste("User", res_auth$user, "signed on as admin"))
    }
    name <- res_auth$user
    values$name <- trimws(name)
    role <- ifelse(res_auth$admin == TRUE, "admin", "user")
    values$role <- trimws(role)
  })
  
  # Load Server Source module file of Package Review History.
  source(file.path("Server", "db_dash_screen.R"), local = TRUE)
  source(file.path("Server", "assessment_criteria.R"), local = TRUE)
  
  # Load Server Source module file of Sidebar.
  source(file.path("Server", "sidebar.R"), local = TRUE)
  
  # Load Source files of UI and Server modules of Upload Package Tab.
  source(file.path("UI", "uploadpackage.R"), local = TRUE)
  source(file.path("Server", "uploadpackage.R"), local = TRUE)
  
  # Load Source files of UI and Server modules of Report Preview Tab
  source(file.path("UI", "reportpreview.R"), local = TRUE)
  source(file.path("Server", "reportpreview.R"), local = TRUE)
  
  # Load Source files of UI and Server modules of Maintenance Metrics Tab.
  source(file.path("UI", "maintenance_metrics.R"), local = TRUE)
  source(file.path("Server", "maintenance_metrics.R"), local = TRUE)
  
  # Load Source files of UI and Server modules of Community Usage Tab.
  source(file.path("UI", "communityusage_metrics.R"), local = TRUE)
  source(file.path("Server", "communityusage_metrics.R"), local = TRUE)
  
  # Start of the observes
  # 1. Observe to Load Source files of UI module of selected screen (Package
  # Dashboard, DB Dashboard, or Login Screen).
  observeEvent(input$db_dash_bttn,{
    values$current_screen <- "db_dash_screen"
  })
  
  observeEvent(values$current_screen, {
    if(values$current_screen == "db_dash_screen") {
      source(file.path("UI", "db_dash_screen.R"), local = TRUE)
      shinyjs::hide("assessment_criteria_bttn")
    } else{
      source(file.path("UI", "dashboard_screen.R"), local = TRUE)
      shinyjs::show("assessment_criteria_bttn")
    }
  })
  
  # 2. Observe to select the package, score, decision and load the data into
  # a reactive variable.
  observe({
    values$mm_tab_redirect <- "redirect"
    
    values$selected_pkg <-
      db_fun(
        paste0(
          "SELECT name, score, decision FROM package WHERE name = '",
          input$select_pack,
          "'"
        )
      )
  })
  
  # Observe Event to load the source file of UI module when we click on the
  # Assessment Criteria action Link.
  observeEvent(input$assessment_criteria_bttn, {
    source(file.path("UI", "assessment_criteria.R"), local = TRUE)
  })
  
}
