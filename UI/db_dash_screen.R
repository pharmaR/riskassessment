###############################################################################
# db_dash_screen - UI functions for the database dashboard.
# Author: Aaron Clark
# Date: September 22nd, 2020
# License: MIT License
###############################################################################

output$screen <- renderUI({
  fluidPage(
    fluidRow(
      column(8, offset = 2, tags$style(".back2dash-icon {float: left;}"),
             actionButton("back2dash", strong("Return to Dashboard"),
                          icon = icon("arrow-circle-left"))),
    ),
    fluidRow(
      column(
        width = 8, offset = 2, align = "center",
        br(),
        h2("Database Overview", class = "card-title text-center txt-color font-weight-bold"),
        hr(class = "bg-color"),
        tags$section(
          br(), br(),
          box(width = 12, collapsible = TRUE, status = "primary",
              title = h3("Uploaded Packages", style = "margin-top: 5px"),
              solidHeader = TRUE,
              DT::dataTableOutput("db_pkgs"),
              br(),
              fluidRow(
                column(
                  width = 6,
                  tags$div(
                    style = "float: right",
                    # Download button to export report(s).
                    downloadButton("dwnld_sel_db_pkgs_btn",
                                   "Download Report(s)",
                                   class = "download_report_btn_class btn-secondary")
                  )),
                column(
                  width = 6,
                  tags$div(
                    style = "float: left; width: 150px",
                    # Select report format.
                    selectInput("report_formats", "Select Format", c("html", "docx"))
                  )
                )
              ))),
        br(), br(),
        uiOutput("admins_view")
      ))
  )
})
