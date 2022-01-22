# Module to display comments for the specified metric.
# The comments will update as the user inserts more comments.

viewCommentsUI <- function(id) {
  fluidRow(
    style = "margin-top: 50px",
    column(
      width = 12,
      align = "left",
      h5('Current Comments', style = "padding-bottom:10px;"),
      wellPanel(htmlOutput(NS(id, "view_comments")))
    )
  )
}

viewCommentsServer <- function(id, pkg_name, comment_type,
                               comment_added = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    # Show the comments on the package.
    output$view_comments <- renderText({
      if(!is.null(comment_added))
        comment_added()
      showComments(pkg_name = pkg_name(), comment_type = comment_type)
    })
  })
}