# Load required packages.
source("global.R")

# Load source files.
source(file.path("Modules", "dbupload.R"))
source(file.path("Modules", "file_upload_error_handling.R"))
source(file.path("Utils", "utils.R"))
source(file.path("Utils", "cum_utils.R"))

# Create db if it doesn't exist.
if(!file.exists(database_name)) create_db()

# Create credentials db if it doesn't exist.
if(!file.exists(credentials_name)) create_credentials_db()

# Start logging info.
set_logfile("loggit.json")

hidden(p(id = "assessment_criteria_bttn"))

# Set spinner options for the tabs.
options(
  spinner.color = "#0275D8",
  spinner.color.background = "#ffffff",
  spinner.size = 1
)

# Create User Interface (UI).
ui <- dashboardPage(
  title = "R Packages Risk Assessment App",
  dashboardHeader(
    title = list(
      tags$a(
        title = "PharmaR Home Page",
        href = "https://www.pharmar.org/",
        target = "_blank"
        #tags$img(src = "logo.png", class = "logo_1"),
        #tags$img(src = "logo_no_text.png", class = "logo_2")
      )
    ),
    titleWidth = 320,
    tags$li(
      class = "dropdown",
      actionLink(
        inputId = "assessment_criteria_bttn",
        class = "assessment_criteria_bttn_class",
        HTML('<div class="tooltip-help">
        <i class="fas fa-info-circle fa-2x asmnt-help-icon"></i>
        <span class="tooltiptext-help">Assessment Criteria Details</span>
        </div>'))
    )
  ),
  
  dashboardSidebar(disable = TRUE),
  
  dashboardBody(
    
    introjsUI(),
    
    # Include js scripts.
    tags$head(
      tags$script(src = "helperScript.js"),
      # Include main.css to add the styles and enhancements to the app.
      tags$link(rel = "stylesheet", type = "text/css", href = "css/main.css")
    ),
    
    useShinyjs(),
    
    # UI screen to load the required screen
    uiOutput("screen")
  )
)

ui <- shinymanager::secure_app(
  ui, 
  # customize top and bottom of login screen
  tags_top = tags$div(
    id = "login_screen",
    #tags$img(src = "logo_no_text.png", style = "align:center"),
    tags$h2("Risk Assessment Application", style = "align:center")),
  enable_admin = TRUE, theme = "css/login_screen.css")

# Create Server Code.
server <- function(session, input, output) {
  
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

shinyApp(ui = ui, server = server)
