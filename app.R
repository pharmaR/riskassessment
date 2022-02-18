# Load required packages.
source("global.R")

# Load source files.
source(file.path("R", "introJSText.R")) # introJS text.
source(file.path("R", "viewComments.R"))
source(file.path("R", "metricBox.R"))
source(file.path("R", "metricGrid.R"))
source(file.path("R", "dbupload.R"))
source(file.path("R", "utils.R"))

# Create db if it doesn't exist.
if(!file.exists(database_name)) create_db()

# Create credentials db if it doesn't exist.
if(!file.exists(credentials_name)) create_credentials_db()

# Start logging info.
set_logfile("loggit.json")

hidden(p(id = "assessment_criteria_bttn"))

theme <- bs_theme(
  bootswatch = "lux",
  version = 5,
  # bg = "white", 
  # fg = "#023967",
  primary = "#24305E",
  secondary = "#F76C6C",
  # success = "orange",
  # info = "yellow",
  # warning = "pink"
)

# Create User Interface (UI).
ui <- fluidPage(
  introjsUI(),
  useShinyjs(),
  waiter::use_waitress(),
  
  theme = theme,
  
  includeCSS(path = "www/css/main.css"),
  
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
          tabsetPanel(
            id = "tabs",
            tabPanel(
              id = "upload_tab_id",
              title = "Upload Package",
              uiOutput("upload_package")  # UI for upload package tab panel.
            ),
            tabPanel(
              id = "mm_tab_id",
              value = "mm_tab_value",
              title = "Maintenance Metrics",
              uiOutput("maintenance_metrics") # UI for Maintenance Metrics tab panel.
            ),
            tabPanel(
              id = "cum_tab_id",
              value = "cum_tab_value",
              title = "Community Usage Metrics",
              uiOutput("community_usage_metrics")  # UI for Community Usage Metrics tab panel.
            ),
            tabPanel(
              id = "reportPreview_tab_id",
              title = "Report Preview",
              uiOutput("report_preview")  # UI for Report Preview tab Panel
            ),
            tabPanel(
              id = "admin_mode_id",
              title = "Administrator mode",
              # only using a div seems to work here
              div(id = "admin_mode",
              shinymanager:::admin_ui(id="admin")  )
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
  
  # Load reactive values into values.
  # TODO: remove it! Need to check where it is used and replace it. 
  values <- reactiveValues()
  
  # Collect user info.
  user <- reactiveValues()
  
  # check_credentials directly on sqlite db
  res_auth <- secure_server(
    check_credentials = check_credentials(
      file.path("credentials.sqlite"),
      passphrase = key_get("R-shinymanager-key", getOption("keyring_user"))
    )
  )
  
  # Save user name and role.  
  observeEvent(res_auth$user, {
    if (res_auth$admin == TRUE)
      loggit("INFO", glue("User {res_auth$user} signed on as admin"))
    
    # TODO: Redundant.
    values$name <- trimws(res_auth$user)
    values$role <- trimws(ifelse(res_auth$admin == TRUE, "admin", "user"))
    
    user$name <- trimws(res_auth$user)
    user$role <- trimws(ifelse(res_auth$admin == TRUE, "admin", "user"))
    
    if (user$role == "user") shinyjs::hide("admin_mode") 
    
  })
  
  # Sidebar module.
  selected_pkg <- sidebarServer("sidebar", uploaded_pkgs)
  
  # Assessment criteria information tab.
  assessmentInfoServer("assessmentInfo")
  
  # Database view.
  databaseViewServer("databaseView")
  
  # Gather maintenance metrics information.
  maint_metrics <- reactive({
    req(selected_pkg$name())
    req(selected_pkg$name() != "-")
    
    # Collect all the metric names and values associated to package_id.
    dbSelect(glue(
      "SELECT metric.name, metric.long_name, metric.description, metric.is_perc,
      metric.is_url, package_metrics.value
      FROM metric
      INNER JOIN package_metrics ON metric.id = package_metrics.metric_id
      WHERE package_metrics.package_id = '{selected_pkg$id()}' AND 
      metric.class = 'maintenance' ;"))
  })
  
  # Gather community usage metrics information.
  community_usage_metrics <- reactive({
    req(selected_pkg$name())
    req(selected_pkg$name() != "-")
    
    dbSelect(glue(
      "SELECT * 
      FROM community_usage_metrics
      WHERE id = '{selected_pkg$name()}'")
    )
  })
  
  output$auth_output <- renderPrint({
    reactiveValuesToList(res_auth)
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
}

shinyApp(ui = ui, server = server)
