#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @rawNamespace import(shiny, except = c(dataTableOutput, renderDataTable))
#' @importFrom utils head installed.packages packageVersion write.csv zip 
#' @importFrom shinymanager secure_app
#' 
#' @noRd
app_ui <- function(request) {
  # Your application UI logic
  ui <- fluidPage(
    
    theme = app_theme(), # defined in data-raw/interanl-data.R
    
    div(id = "raa-logo", shiny::a(img(src="www/raa-image-ug.png"), href = "https://pharmar.github.io/riskassessment/", target="_blank")),
    
    tabsetPanel(
      id = "apptabs",
      tabPanel(
        title = "Risk Assessment",
        icon = icon("clipboard-list"),
        value = "risk-assessment-tab",
        
        titlePanel(
          windowTitle = "riskassessment app",
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
                id = "metric_tab_id",
                title = "Package Metrics",
                br(),
                selectInput("metric_type", NULL, 
                            choices = c("Maintenance Metrics" = "mm", 
                                        "Community Usage Metrics" = "cum",
                                        "Package Dependencies" = "dep")) %>%
                  tagAppendAttributes(class = "metric_select"),
                conditionalPanel(
                  "input.metric_type == 'mm'",
                  maintenanceMetricsUI("maintenanceMetrics")
                ),
                conditionalPanel(
                  "input.metric_type == 'cum'",
                  communityMetricsUI("communityMetrics")
                ),
                conditionalPanel(
                  "input.metric_type == 'dep'",
                  packageDependenciesUI("packageDependencies")
                )
              ),
              tabPanel(
                id = "src_explorer_id",
                title = "Source Explorer",
                br(),
                selectInput("explorer_type", NULL,
                            choices = c("File Browser" = "fb",
                                        "Function Explorer" = "fe")) %>%
                  tagAppendAttributes(class = "metric_select"),
                conditionalPanel(
                  "input.explorer_type == 'fb'",
                  mod_pkg_explorer_ui("pkg_explorer")
                ),
                conditionalPanel(
                  "input.explorer_type == 'fe'",
                  mod_code_explorer_ui("code_explorer")
                )
              ),
              tabPanel(
                id = "reportPreview_tab_id",
                title = "Build Report",
                reportPreviewUI("reportPreview")
              )      
            )
          )
        )
      ), 
      
      tabPanel(
        title = "Database",
        icon = icon("database"),
        databaseViewUI("databaseView"),
        value = "database-tab"
      ),
      
      tabPanel(
        title = "Assessment Criteria",
        icon = icon("circle-info"),
        assessmentInfoUI("assessmentInfo"),
        value = "assessment-criteria-tab"
      )
    ),
    
    wellPanel(
      id = "footer",
      "Checkout the app's code!",
      tags$a(href = "https://github.com/pharmaR/riskassessment",
             icon("github-alt"), target = "_blank")
    )
  )

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
#' 
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @importFrom rintrojs introjsUI
#' @importFrom shinyjs useShinyjs
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
  )
}
