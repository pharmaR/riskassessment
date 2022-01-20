#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`. DO NOT REMOVE.
#' @import shiny
#' @importFrom shinydashboard dashboardPage dashboardHeader dashboardSidebar
#'   dashboardBody
#' @importFrom shinymanager secure_app
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    
    # Your application UI logic 
    # shinymanager::secure_app(
      dashboardPage(
        title = "R Package Risk Assessment App",
        dashboardHeader(
          title = list(
            tags$a(
              title = "PharmaR Home Page",
              href = "https://www.pharmar.org/",
              target = "_blank",
              tags$img(src = "logo.png", class = "logo_1"),
              tags$img(src = "logo_no_text.png", class = "logo_2")
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
          
          # rintrojs::introjsUI(), # don't need here. belongs below
          
          # Include js scripts.
          # tags$head(tags$script(src = "inst/app/www/script.js")),
          
          # Include main.css to add the styles and enhancements to the app.
          # includeCSS("inst/app/www/main.css"), # do I need this
          
          # shinyjs::useShinyjs(), # don't need here. belongs below
          
          # UI screen to load the required screen
          uiOutput("screen")
        )
      )#, 
    # customize top and bottom of login screen
    #   tags_top =
    #     tags$div(
    #       tags$h2("Risk Assessment Application", style = "align:center"),
    #       tags$img(
    #         src = "logo_no_text.png",
    #         width = 100, style = "align:center"
    #       )),
    #   enable_admin = TRUE
    # )
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
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'riskassessment'
    ),
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
    rintrojs::introjsUI(),
    shinyjs::useShinyjs()
  )
}

