# options
options(shiny.fullstacktrace = FALSE) # TRUE for more descriptive debugging msgs

# Load required packages.
source("global.R")

# suppress dplyr summarize msg "summarise()` has grouped output by..."
options(dplyr.summarise.inform = FALSE)

# Load source files.
source(file.path("R", "introJSText.R")) # introJS text.
source(file.path("R", "dbupload.R"))
source(file.path("R", "utils.R"))

# Create db if it doesn't exist.
if(!file.exists(database_name)) create_db()

# Create credentials db if it doesn't exist.
if(!file.exists(credentials_name)) create_credentials_db()

# Start logging info.
set_logfile("loggit.json")

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
  includeCSS(path = "www/css/community_metrics.css"),
  
  tabsetPanel(
    id = "apptabs",
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
              uploadPackageUI("upload_package")
            ),
            tabPanel(
              id = "mm_tab_id",
              title = "Maintenance Metrics",
              maintenanceMetricsUI('maintenanceMetrics')
            ),
            tabPanel(
              id = "cum_tab_id",
              title = "Community Usage Metrics",
              communityMetricsUI('communityMetrics')
            ),
            tabPanel(
              id = "reportPreview_tab_id",
              title = "Report Preview",
              reportPreviewUI("reportPreview")  # UI for Report Preview tab Panel
            )
          )
        )
      )
    ), 
    
    tabPanel(
      title = div(id = "database-tab", icon("database"), "Database"),
      databaseViewUI("databaseView")
    ),
    
    tabPanel(
      title = div(id = "assessment-criteria-tab", icon("info-circle"), "Assessment Criteria"),
      assessmentInfoUI("assessmentInfo")
    )
  ),
  
  footer =
    wellPanel(
    id = "footer",
    "Checkout the app's code!",
    tags$a(href = "https://github.com/pharmaR/risk_assessment",
           icon("github-alt"), target = "_blank")
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

    user$name <- trimws(res_auth$user)
    user$role <- trimws(ifelse(res_auth$admin == TRUE, "admin", "user"))
  })

  # Load server of the uploadPackage module.
  uploaded_pkgs <- uploadPackageServer("upload_package")
  
  # Load server of the sidebar module.
  selected_pkg <- sidebarServer("sidebar", user, uploaded_pkgs$names)
  
  # Load server of the assessment criteria module.
  assessmentInfoServer("assessmentInfo")
  
  # Load server of the database view module.
  databaseViewServer("databaseView", uploaded_pkgs$names)
  
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
  
  # Load server of the report preview tab.
  reportPreviewServer("reportPreview", selected_pkg, maint_metrics,
                      community_usage_metrics,
                      mm_comment_added, com_comment_added)
  
  # Load server for the maintenance metrics tab.
  mm_comment_added <- maintenanceMetricsServer('maintenanceMetrics',
                                               selected_pkg,
                                               maint_metrics,
                                               user)
  
  # Load server for the community metrics tab.
  communityMetricsServer('communityMetrics',
                         selected_pkg,
                         community_usage_metrics,
                         user)
  
  output$auth_output <- renderPrint({
    reactiveValuesToList(res_auth)
  })
}

shinyApp(ui = ui, server = server)
