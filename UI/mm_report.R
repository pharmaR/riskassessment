fluidRow(
  div(style = "height:50px;"),
  class = "mm-main-row text-center m-0",
  h3(tags$b("Maintenance Metrics"), class = "text-left"),
  fluidRow(
    class = "mm-row-1",
    infoBoxOutput("has_vignettes1"),  # Info box for 'has_vignettes' metric.
    infoBoxOutput("has_website1"),  # Info box for 'has_website' metric.
    infoBoxOutput("has_news1"),  # Info box for 'has_news' metric.
  ),
  fluidRow(
    class = "mm-row-2",
    infoBoxOutput("news_current1"),  # Info box for 'news_current' metric.
    infoBoxOutput("has_bug_reports_url1"),  # Info box for 'has_bug_reports_url' metric.
    infoBoxOutput("bugs_status1"),  # Info box for 'bugs_status' metric.
  ),
  fluidRow(
    class = "mm-row-3",
    infoBoxOutput("export_help1"),  # Info box for 'export_help' metric.
    infoBoxOutput("has_source_control1"),  # Info box for 'has_source_control' metric.
    infoBoxOutput("has_maintainer1"),  # Info box for 'has_maintainer' metric.
  ),
  viewCommentsUI("mm_report_comments")
)
