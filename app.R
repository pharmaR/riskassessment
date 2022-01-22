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

theme <- bs_theme(
  version = 5,
  bg = "white", 
  fg = "#023967",
  bootswatch = "lux",
  primary = "#2E86C1",
  secondary = "#1D7BCC",
  success = "orange",
  info = "yellow",
  warning = "pink"
)

# Create User Interface (UI).
ui <- fluidPage(
  introjsUI(),
  useShinyjs(),
  
  theme = theme,
  
  includeCSS(path = "www/css/main.css"),
  includeScript(path = "www/js/popper.js"),
  includeScript(path = "www/js/tooltip.js"),
  
  tabsetPanel(
    tabPanel(
      title = "Risk Assessment",
      icon = icon("clipboard-list"),
      
      titlePanel(
        windowTitle = "Risk Assessment",
        title = div(id = "page-title", "R Package Risk Assessment App")
      ),
      
      sidebarLayout(
        sidebarPanel = sidebarPanel(
          width = 4,
          sidebarUI("sidebar")
        ),
        
        mainPanel = mainPanel(
          width = 8,
          tags$li(
            class = "dropdown",
            style = "float: right; padding-right: 75px; padding-top: 25px;",
            actionLink("db_dash_bttn",
                       HTML('<div class="tooltip-help">
                      <i class="fas fa-database fa-2x database-icon"></i>
                      <span class="tooltiptext-help fa-database-tooltiptext-help">Database</span>
                      </div>'))
          ),
          tabsetPanel(
            id = "tabs",
            tabPanel(
              id = "upload_tab_id",
              value = "upload_tab_value",
              title = "Upload Package",
              uiOutput("upload_package")  # UI for upload package tab panel.
            ),
            tabPanel(
              id = "reportPreview_tab_id",
              value = "reportPreview_tab_value",
              title = "Report Preview",
              withSpinner(uiOutput("report_preview"), type = 2)  # UI for Report Preview tab Panel
            ),
            tabPanel(
              id = "mm_tab_id",
              value = "mm_tab_value",
              title = "Maintenance Metrics",
              withSpinner(uiOutput("maintenance_metrics"), type = 2) # UI for Maintenance Metrics tab panel.
            ),
            tabPanel(
              id = "cum_tab_id",
              value = "cum_tab_value",
              title = "Community Usage Metrics",
              withSpinner(uiOutput("community_usage_metrics"), type = 2)  # UI for Community Usage Metrics tab panel.
            )
          )
        )
      )
    ), 
    
    tabPanel(
      title = "Database",
      icon = icon("database"),
      databaseViewUI("databaseView")
    ),
    
    tabPanel(
      title = "Assessment Criteria",
      icon = icon("info-circle"),
      assessmentInfoUI("assessmentInfo")
    ),
    
    footer = wellPanel(
      id = "footer",
      "Checkout the app's code!",
      tags$a(href = "https://github.com/pharmaR/risk_assessment",
             icon("github-alt"), target = "_blank")
    )
  )
)

ui <- shinymanager::secure_app(
  ui, 
  # customize top and bottom of login screen
  tags_top = tags$div(
    tags$link(rel = "stylesheet", type = "text/css", href = "css/login_screen.css"),
    id = "login_screen",
    tags$h2("Risk Assessment Application", style = "align:center")),
  enable_admin = TRUE, theme = theme)

# Create Server Code.
server <- function(session, input, output) {
  
  # Sidebar module.
  sidebarServer("sidebar")
  
  # Assessment criteria information tab.
  assessmentInfoServer("assessmentInfo")
  
  # Database view.
  databaseViewServer("databaseView")
  
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
  
  # Observe Event to load the source file of UI module when we click on the
  # Assessment Criteria action Link.
  observeEvent(input$assessment_criteria_bttn, {
    source(file.path("UI", "assessment_criteria.R"), local = TRUE)
  })
  
}

shinyApp(ui = ui, server = server)
