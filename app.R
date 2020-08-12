#####################################################################################################################
# Project: R Validation Hub - R Package Risk Assessment App
# Author: K Aravind Reddy
# Date: July 13th, 2020
# License: MIT License
# You can run the application by executing 'runApp()' command.
#####################################################################################################################

# Step 1 -- Loading all required packages.

source("./setup.R")

# Step 2 -- Load source files.

source("./Modules/dbupload.R")
source("./Modules/file_upload_error_handling.R")
source("./Utils/utils.R")

set_logfile("loggit.json")

hidden(p(id = "assessment_criteria_bttn"))

options(
  spinner.color = "#0275D8",
  spinner.color.background = "#ffffff",
  spinner.size = 1
)

# Step 3 -- Create User Interface (UI).

ui <- dashboardPage(
  dashboardHeader(
    title = list(
      "R Package Risk Assessment App",
      tags$a(
        title = "Pharmar Home Page",
        href = "https://www.pharmar.org/",
        target = "_blank",
        tags$img(src = "logo.png", class = "logo_1"),
        tags$img(src = "logo_no_text.png", class = "logo_2")
      )
    ),
    titleWidth = 420,
    tags$li(
      class = "dropdown",
      HTML(
        '<i class="fas fa-info-circle fa-2x float-right cursor-help asmnt-help-icon" title="Click to open Assessment Criteria Modal"></i>'
      ),
      actionLink("assessment_criteria_bttn", class = "assessment_criteria_bttn_class", "Assessment Criteria")
    )
  ),
  
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    tags$head(tags$style(
      HTML(
        '.myClass {
        font-size: 25px;
        line-height: 60px;
        text-align: center;
        font-family: "Georgia", Times, "Times New Roman", serif;
        font-weight: bold;
        padding: 0 335px;
        overflow: hidden;
        color: white;
       }'
      )
    )),
    
    tags$script(
      HTML(
        '$(document).ready(function() {
         $("header").find("nav").append(\'<span class="myClass"> R Package Risk Assessment App </span>\');
         })'
      )
    ),

    # Including main.css to add the styles and enhancements to the app.
    
    includeCSS("./www/main.css"),
    
    useShinyjs(),
    
    # UI screen to load the required screen
    uiOutput("screen")
    
  )
)

# Step 4 -- Create Server Code .

server <- function(session, input, output) {
  # Load reactive values into values.
  
  values <- reactiveValues()
  values$current_screen <- "login_screen"
  values$uploaded_file_status <- "no_status"
  values$upload_complete <- "upload_incomplete"
  values$select_pack <- "Select"
  
  # Load Source files of UI and Server modules of Login Screen.
  
  source(file.path("Server", "login_screen.R"), local = TRUE)$value
  
  source(file.path("Server", "assessment_criteria.R"), local = TRUE)$value
  
  # Load Server Source module file of Sidebar.
  
  source(file.path("Server", "sidebar.R"), local = TRUE)$value
  
  # Load Source files of UI and Server modules of Upload Package Tab.
  
  source(file.path("UI", "uploadpackage.R"), local = TRUE)$value
  
  source(file.path("Server", "uploadpackage.R"), local = TRUE)$value
  
  
  # Load Source files of UI and Server modules of Report Preview Tab
  
  source(file.path("UI", "reportpreview.R"), local = TRUE)$value
  
  source(file.path("Server", "reportpreview.R"), local = TRUE)$value
  
  
  
  # Load Source files of UI and Server modules of Maintenance Metrics Tab.
  
  source(file.path("UI", "maintenance_metrics.R"), local = TRUE)$value
  
  source(file.path("Server", "maintenance_metrics.R"), local = TRUE)$value
  
  
  
  # Load Source files of UI and Server modules of Community Usage Tab. -
  
  source(file.path("UI", "communityusage_metrics.R"), local = TRUE)$value
  
  source(file.path("Server", "communityusage_metrics.R"), local = TRUE)$value
  
  
  
  # Load Source files of UI and Server modules of Testing Metrics Tab.
  
  source(file.path("UI", "testing_metrics.R"), local = TRUE)$value
  
  source(file.path("Server", "testing_metrics.R"), local = TRUE)$value
  
  
  # Start of the observe's'
  # 1. Observe to Load Source files of UI module of slected screen (Dashboard or Login Screen).
  
  observe({
    if (values$current_screen != "dashboard_screen") {
      source(file.path("UI", "login_screen.R"), local = TRUE)$value
      shinyjs::hide("assessment_criteria_bttn")
    } else{
      source(file.path("UI", "dashboard_screen.R"), local = TRUE)$value
      shinyjs::show("assessment_criteria_bttn")
    }
  })  # End of the selected screen observe.
  
  # 2. Observe to select the package,score,decision and load the data into reactive variable.
  observe({
    values$selected_pkg <-
      db_fun(
        paste0(
          "SELECT package, score, decision FROM Packageinfo WHERE package = '",
          input$select_pack,
          "'"
        )
      )
  })  # End of the observe for reactive table.
  # End of the observe's'
  
  # Observe Event to load the source file of UI module when we click on the Assessment Criteria action Link.
  
  observeEvent(input$assessment_criteria_bttn, {
    source(file.path("UI", "assessment_criteria.R"), local = TRUE)$value
  })  # End of the Observe Event
  
  
}  # End of the Server Code.


shinyApp(ui = ui, server = server)

# End of the app.R file
