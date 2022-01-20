#####################################################################################################################
# mm_report.R - Maintenance Metrics to show the info box's to show the information and multiple comments 
#               for users and display the comments by users at Report Preview Section.
# Author: K Aravind Reddy
# Date: July 13th, 2020
# License: MIT License
#####################################################################################################################

fluidRow(
  div(style = "height:50px;"),
  class = "mm-main-row text-center m-0",
  h3(tags$b("Maintenance Metrics"), class = "text-left"),
  fluidRow(
    class = "mm-row-1",
    shinydashboard::infoBoxOutput("has_vignettes1"),  # Info box for 'has_vignettes' metric.
    shinydashboard::infoBoxOutput("has_website1"),  # Info box for 'has_website' metric.
    shinydashboard::infoBoxOutput("has_news1"),  # Info box for 'has_news' metric.
  ),
  fluidRow(
    class = "mm-row-2",
    shinydashboard::infoBoxOutput("news_current1"),  # Info box for 'news_current' metric.
    shinydashboard::infoBoxOutput("has_bug_reports_url1"),  # Info box for 'has_bug_reports_url' metric.
    shinydashboard::infoBoxOutput("bugs_status1"),  # Info box for 'bugs_status' metric.
  ),
  fluidRow(
    class = "mm-row-3",
    shinydashboard::infoBoxOutput("export_help1"),  # Info box for 'export_help' metric.
    shinydashboard::infoBoxOutput("has_source_control1"),  # Info box for 'has_source_control' metric.
    shinydashboard::infoBoxOutput("has_maintainer1"),  # Info box for 'has_maintainer' metric.
  ),
  fluidRow(
    class = "mm-row-comments",
    column(
      width = 12,
      align = "left",
      h3(tags$b(paste0('Comments(',nrow(values$comment_mm2),'):'))),
      htmlOutput("mm_commented1")  # html output to show the comments on application.
    )
  )
)
