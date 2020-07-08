fluidRow(
  div(style = "height:25px;"),
  class = "t_m_main_row m-0",
  h3("Testing Metrics"),
  fluidRow(
    h3("TEST COVERAGE(%)", class = "text-center"),
    div(style = "height:25px;"),
    amChartsOutput(outputId = "test_coverage1")  #  amchart to display the test converage.
  ),
  fluidRow(
    class = "t_m_comments_row",
    column(
      width = 12,
      align = "left",
      h3(tags$b(paste0('Comments(',nrow(values$comment_tm2),'):'))),
      htmlOutput("tm_commented1", inline = TRUE)  # showing comments on application.
    ))
)