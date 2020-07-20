#####################################################################################################################
# tm_report.R - Testing Metrics to show the info box's to show the information and leave multiple comments 
#               for users and display the comments by users at Report Preview Section.
# Author: K Aravind Reddy
# Date: July 13th, 2020
# License: MIT License
#####################################################################################################################

# Start of the UI for infobox's 

fluidRow(
  div(style = "height:25px;"),
  class = "t_m_main_row m-0",
  h3(tags$b("Testing Metrics")),
  fluidRow(
    h3("TEST COVERAGE(%)", class = "text-center"),
    div(style = "height:25px;"),
    amChartsOutput(outputId = "test_coverage1")  #  amchart to display the test coverage.
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

# End of the UI