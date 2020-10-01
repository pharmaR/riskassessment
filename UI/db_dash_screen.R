#####################################################################################################################
# login.R - The login page of the app
# Author: K Aravind Reddy
# Date: July 13th, 2020
# License: MIT License
#####################################################################################################################

# Start of the login screen Source file for UI Module.

output$screen <- renderUI({
  fluidPage(
    fluidRow(
      br(),br(),br(),br(),
      column(2,""),
      column(8,
        tags$style(".back2dash-icon {float: left;}"),
        actionButton("back2dash",strong("Return to Package Dashboard"),
                     # icon = icon("arrow-circle-left")
                     )
      ),
      column(2,"")
    ),
    fluidRow( #class = "first_screen",
     column(2,""),
     column(
       align="center",
       width = 8,
         tags$h2("History of Packages Reviewed", class = "card-title text-center txt-color font-weight-bold"),
         tags$hr(class = "bg-color"),
         id = "db_dash_screen",
         tags$section(
           br(),
           fluidRow(
             class="float-right r_p_format_row",
             tags$div(
               class="col-sm W-40 text-left float-right",
               selectInput("report_formats", "Select Format", c("html", "docx")),  # Select input to select the format for report.
             ),
             tags$div(
               class="col-sm float-right",
               downloadButton("dwnld_sel_db_pkgs_btn", "Dwnld Report for Selection(s)", class = "download_report_btn_class btn-secondary"),  # Download button to export the report.
             ),
           ),
           br(),
           DT::dataTableOutput("db_pkgs"),
           br()
         ) # end tags$section()
       ) # column
     ,column(2,""),
    ) # fluidRow
  ) # fluidPage
})


# End of the login screen Source File for UI module.
