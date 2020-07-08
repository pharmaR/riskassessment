fluidRow(
  div(style = "height:25px;"),
  class = "c_u_m_row_main text-center m-0",
  h3("Community Usage Metrics", class = "text-left"),
  fluidRow(
    class = "c_u_m_row_1",
    infoBoxOutput("time_since_first_release1", width = 5),  # Info box to show the time since First release.
    infoBoxOutput("time_since_version_release1", width = 5)  # Infor box to show the time since versoin release.
  ),
  fluidRow(
    class = "c_u_m_row_graph",
    column(width = 2, ),
    column(width = 8,
           class = "w-90",
           highchartOutput("no_of_downloads1")),
    column(width = 2, )
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