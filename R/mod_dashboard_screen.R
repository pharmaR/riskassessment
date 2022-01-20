# No UI needed given current renderUI structure in server-side logic
# #' dashboard_screen UI Function
# #'
# #' @description A shiny Module.
# #'
# #' @param id,input,output,session Internal parameters for {shiny}.
# #'
# #' @noRd 
# #'
# #' @importFrom shiny NS tagList 
# mod_dashboard_screen_ui <- function(id){
#   ns <- NS(id)
#   tagList(
#  
#   )
# }
    
#' dashboard_screen Server Functions
#' 
#' @import shiny
#'
#' @noRd 
mod_dashboard_screen_server <- function(input, output, session = getDefaultReactiveDomain()
                            ){ # id removed & added: input, output, session!
  # moduleServer( id, function(input, output, session){
  #   ns <- session$ns
  # })
  output$screen <- renderUI({
    fluidRow(
      class = "main-container ml-2 mr-2 p-1",
      HTML('<i class="fas fa-bars fa-3x float-left cursor-pointer sidebar-menu" id="sidebar-menu-id"></i>'),
      tags$div(
        class = "sidebar w-24 p-3",
        tags$div(
          class = "container sidebar-container",
          tags$div(
            class = "sidebar-header",
            tags$h5("Package Control Panel", class = "sidebar-header text-center")
          ),
          tags$hr(class = "bg-color"),
          tags$div(class = "sidebar-content",
                   tags$div(
                     sidebarPanel(
                       width = 12,
                       uiOutput("sel_pack"), # UI for select package.
                       uiOutput("sel_ver"), # UI for version of the selected package.
                       # uiOutput("status"), # Display the status of the package.
                       fixedRow(
                         column(6, wellPanel(
                           id = "diyValBoxStatus",
                           style = "height: 150px; border-radius: 25px; border-style: dotted; background-color: #C0C0C0 !important; color: #FFFFFF !important;",
                           uiOutput("status"), # Display the score of the package.
                           h3("Status")
                         )),
                         column(6, wellPanel(
                           id = "diyValBoxScore",
                           style = "height: 150px; border-radius: 25px; border-style: dotted; background-color: #1E90FF !important; color: #FFFFFF !important;",
                           uiOutput("score"), # Display the score of the package.
                           h3("Risk Score")
                           # shinydashboard::valueBoxOutput("score", width = 12),
                         ))
                       ),
                       
                       
                       textAreaInput(
                         "overall_comment",
                         h3("Overall Comment"),
                         width = "100%",
                         rows = 5
                       ),
                       # Action Button to Submit Overall Comment for selected Package.
                       actionButton("submit_overall_comment",
                                    class = "submit_overall_comment_class btn-secondary",
                                    "Submit Comment"),
                       tags$div(
                         class = "col-sm-12 decision_div",
                         HTML("<i class='fas fa-info-circle fa-2x float-right txt-color cursor-help' title='Once submitted the decision cannot be reverted and comments in group and package level will be frozen'></i>"),
                         # Slider input to select the decision for selected package.
                         shinyWidgets::sliderTextInput(
                           "decision",
                           h3("Overall Risk"),
                           selected = NULL,
                           grid = TRUE,
                           c("Low", "Medium", "High")
                         ),
                         # Action button to submit decision for selected package.
                         actionButton("submit_decision", class = "submit_decision_class btn-secondary", "Submit Decision")
                       )
                     ))
          )
        )),
      tags$div(class = "main-component justify-content-center text-center",
               mainPanel(
                 class="main-component-child",
                 width = 12,
                 
                 tags$li(
                   class = "dropdown",
                   style = "float: right; padding-right: 75px; padding-top: 25px;",
                   actionLink("db_dash_bttn",
                              HTML('<div class="tooltip-help">
                      <i class="fas fa-database fa-2x database-icon"></i>
                      <span class="tooltiptext-help fa-database-tooltiptext-help">Database</span>
                      </div>'))
                 ),
                 br(),
                 
                 tabsetPanel(
                   id = "tabs",
                   tabPanel(
                     id = "upload_tab_id",
                     value = "upload_tab_value",
                     tags$b("Upload Package"),
                     uiOutput("upload_package")  # UI for upload package tab panel.
                   ),
                   tabPanel(
                     id = "reportPreview_tab_id",
                     value = "reportPreview_tab_value",
                     tags$b("Report Preview"),
                     shinycssloaders::withSpinner(uiOutput("report_preview"), type = 2)  # UI for Report Preview tab Panel
                   ),
                   tabPanel(
                     id = "mm_tab_id",
                     value = "mm_tab_value",
                     tags$b("Maintenance Metrics"),
                     shinycssloaders::withSpinner(uiOutput("maintenance_metrics"), type = 2) # UI for Maintenance Metrics tab panel.
                   ),
                   tabPanel(
                     id = "cum_tab_id",
                     value = "cum_tab_value",
                     tags$b("Community Usage Metrics"),
                     shinycssloaders::withSpinner(uiOutput("community_usage_metrics"), type = 2)  # UI for Community Usage Metrics tab panel.
                   )
                   # tabPanel(
                   #   id = "tm_tab_id",
                   #   value = "tm_tab_value",
                   #   tags$b("Testing Metrics"),
                   #   shinycssloaders::withSpinner(uiOutput("testing_metrics"), type = 2)  # UI for Testing Metrics tab panel.
                   # )
                 )
               ))
    )
  })
  
}
    
## To be copied in the UI
# mod_dashboard_screen_ui("dashboard_screen_ui_1")
    
## To be copied in the server
# mod_dashboard_screen_server("dashboard_screen_ui_1")
