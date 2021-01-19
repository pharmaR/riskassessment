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
    uiOutput("myboxes1")
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
