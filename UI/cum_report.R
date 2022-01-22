fluidRow(
  div(style = "height:25px;"),
  class = "c_u_m_row_main text-center m-0",
  h3(tags$b("Community Usage Metrics"), class = "text-left"),
  fluidRow(
    class = "c_u_m_row_1",
    infoBoxOutput("time_since_first_release1", width = 4),  # Info box to show the time since First release.
    infoBoxOutput("time_since_version_release1", width = 4),  # Info box to show the time since version release.
    infoBoxOutput("dwnlds_last_yr1", width = 4)  # Info box to show the total # of Downloads in the last year.
  ),
  fluidRow(
    class = "c_u_m_row_graph",
    column(width = 1, ),
    column(width = 10,
           class = "w-90",
           plotly::plotlyOutput("no_of_downloads1")
           ),
    column(width = 1, )
  ),
  fluidRow(
    class = "c_u_m_row_comments",
    column(
      width = 12,
      align = "left",
      h3(tags$b(paste0('Comments(',nrow(values$comment_cum2),'):'))),
      htmlOutput("cum_commented1")  # html output to show the comments on applicaiton.
    ))
)

# End of the UI