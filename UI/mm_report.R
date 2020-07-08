fluidRow(
  div(style = "height:50px;"),
  class = "mm-main-row text-center m-0",
  h3("Maintenance Metrics", class = "text-left"),
  fluidRow(
    class = "mm-row-1",
    infoBoxOutput("vignette1"),  # Info box to show the information on VIGNETTE Content.
    infoBoxOutput("website1"),  # Info box to show the information on Package Has Website.
    infoBoxOutput("hasnews1"),  # Info box to show the Package Has News? Content.
  ),
  fluidRow(
    class = "mm-row-2",
    infoBoxOutput("newscurrent1"),  # Info box to show the information for News is Current?
    infoBoxOutput("bugtrack1"),  # Info box to show the information for Does the package have Bug Report?
    infoBoxOutput("bugstatus1"),  # Info box to show the information on Bugs Status.
  ),
  fluidRow(
    class = "mm-row-3",
    infoBoxOutput("exporthelp1"),  # Info box to show the information on Export help.
    infoBoxOutput("source_pub1"),  # Info box to show the information on source code is public?
    infoBoxOutput("pack_maint1"),  # Info box to show the information on Has a package maintainer?
  ),
  fluidRow(
    class = "mm-row-comments",
    column(
      width = 12,
      align = "left",
      h3(tags$b(paste0('Comments(',nrow(values$comment_mm2),'):'))),
      htmlOutput("mm_commented1")  # html output to show the comments on applicaiton.
    )
  )
)