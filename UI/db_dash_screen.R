###############################################################################
# db_dash_screen - UI functions for the database dashboard.
# Author: Aaron Clark
# Date: September 22nd, 2020
# License: MIT License
###############################################################################

output$screen <- renderUI({
  fluidPage(
    fluidRow(
      br(), br(), br(), br(),
      column(2, ""),
      column(8,
             tags$style(".back2dash-icon {float: left;}"),
             actionButton("back2dash", strong("Return to Package Dashboard"),
                          icon = icon("arrow-circle-left"))),
      column(2, "")
    ),
    fluidRow(
      column(2, ""),
      column(
        align = "center",
        width = 8,
        tags$h2("History of Packages Reviewed",
                class = "card-title text-center txt-color font-weight-bold"),
        tags$hr(class = "bg-color"),
        id = "db_dash_screen",
        tags$section(
          br(),
          fluidRow(
            class = "float-right r_p_format_row",
            tags$div(
              class = "col-sm W-40 text-left float-right",
              # Select report format.
              selectInput("report_formats",
                          "Select Format",
                          c("html", "docx"))
            ),
            tags$div(
              class = "col-sm float-right",
              # Download button to export the report.
              downloadButton("dwnld_sel_db_pkgs_btn",
                             "Dwnld Report for Selection(s)",
                             class = "download_report_btn_class btn-secondary")
            )
          ),
          br(),
          DT::dataTableOutput("db_pkgs"),
          br()
        )
      ),
      column(2, "")
    )
  )
})
