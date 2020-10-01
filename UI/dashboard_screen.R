#####################################################################################################################
# dashboard_screen.R - The dashboard page of the app
# Author: K Aravind Reddy
# Date: July 13th, 2020
# License: MIT License
#####################################################################################################################
# Start of the dashboard_screen Source file for UI Module.

# Render Output UI for Dashboard Screen.

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
                   uiOutput("sel_pack"),  # UI for select package.
                   uiOutput("sel_ver"),  # UI for version of the selected Package.
                   htmlOutput("status"),  # Display the status of the Package.
                   htmlOutput("score"),  # Display the Score of the Package.
                   textAreaInput(
                     "overall_comment",
                     h3("Leave Your Overall Comment:"),
                     width = "100%",
                     rows = 5,
                     placeholder = paste("Current Comment:")
                   ),
                   # Action Button to Submit Overall Comment for selected Package.
                   actionButton("submit_overall_comment", class = "submit_overall_comment_class btn-secondary", "Submit Comment"),
                   tags$div(
                     class = "col-sm-12 decision_div",
                     HTML("<i class='fas fa-info-circle fa-2x float-right txt-color cursor-help' title='Once submitted the decision cannot be reverted and comments in group and package level will be frozen'></i>"),
                     # Slider input to select the decision for selected package.
                     sliderTextInput(
                       "decision",
                       h3("Overall Risk:"),
                       selected = NULL,
                       grid = TRUE,
                       c("Low", "Medium", "High")
                     ),
                     # Action button to submit decision for selected package.
                     actionButton("submit_decision", class = "submit_decision_class btn-secondary", "Submit Decision")
                   )
                 ),
               ),)
    )
  ),
  tags$div(class = "main-component justify-content-center text-center",
           mainPanel(
            class="main-component-child",
             width = 12,
               
             tags$li(
              class = "dropdown",
              style = "float: right; padding-right: 75px; padding-top: 25px;",
              actionLink("db_dash_bttn", class = "assessment_criteria_bttn_class",
                         HTML('<div class="tooltip-help">
                      <i class="fas fa-history fa-2x history-help-icon"></i>
                      <span class="tooltiptext-help">Package Review History</span>
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
                 withSpinner(uiOutput("report_preview"), type = 2)  # UI for Report Preview tab Panel
               ),
               tabPanel(
                 id = "mm_tab_id",
                 value = "mm_tab_value",
                 tags$b("Maintenance Metrics"),
                 withSpinner(uiOutput("maintenance_metrics"), type = 2) # UI for Maintenance Metrics tab panel.
               ),
               tabPanel(
                 id = "cum_tab_id",
                 value = "cum_tab_value",
                 tags$b("Community Usage Metrics"),
                 withSpinner(uiOutput("community_usage_metrics"), type = 2)  # UI for Community Usage Metrics tab panel.
               ),
               tabPanel(
                 id = "tm_tab_id",
                 value = "tm_tab_value",
                 tags$b("Testing Metrics"),
                 withSpinner(uiOutput("testing_metrics"), type = 2)  # UI for Testing Metrics tab panel.
               )
             )
            ,
             tags$head(tags$script(src = "helperScript.js"))
           ))
  )
})

# End of the dashboard_screen Source file for UI Module.
