#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom shinymanager secure_app
#' 
#' @noRd
app_ui <- function(request) {
  # Your application UI logic
  ui <- fluidPage(
    
    theme = theme, # defined in global.R
    
    # not needed any more. Automatically bundled below
    # includeCSS(path = file.path('www', 'css', 'main.css')),
    # includeCSS(path = file.path('www', 'css', 'community_metrics.css')),
    
    tabsetPanel(
      id = "apptabs",
      tabPanel(
        title = "Risk Assessment",
        icon = icon("clipboard-list"),
        
        titlePanel(
          windowTitle = "Risk Assessment - v0.0.1",
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
                maintenanceMetricsUI("maintenanceMetrics")
              ),
              tabPanel(
                id = "cum_tab_id",
                title = "Community Usage Metrics",
                communityMetricsUI("communityMetrics")
              ),
              tabPanel(
                id = "reportPreview_tab_id",
                title = "Report Preview",
                reportPreviewUI("reportPreview")
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
        assessmentInfoUI("assessmentInfo"),
      )
    ),
    
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
      tags$link(rel = "stylesheet", type = "text/css",
                href = file.path('css', 'login_screen.css')),
      id = "login_screen",
      tags$h2("Risk Assessment Application", style = "align:center"),
      tags$h3(glue::glue('**Version {app_version}**'),
              style = "align:center; color: darkgray")),
    enable_admin = TRUE, theme = theme)
  
  ui <- add_tags(ui)
  
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    ui
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @importFrom rintrojs introjsUI
#' @importFrom shinyjs useShinyjs
#' @importFrom waiter use_waitress
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "riskassessment"
    ),
    # Add here other external resources
    rintrojs::introjsUI(),
    shinyjs::useShinyjs(),
    waiter::use_waitress(),
  )
}
