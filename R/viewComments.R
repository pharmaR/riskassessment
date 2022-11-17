#' The UI for the 'viewComment' module
#'
#' Module to display comments for the specified metric. The comments will update
#' as the user inserts more comments.
#' 
#' @param id a module id name
#' 
#' 
#' 
viewCommentsUI <- function(id) {
  fluidRow(
    style = "margin-top: 50px",
    column(
      width = 12,
      align = "left",
      uiOutput(NS(id, 'view_comments'))
    )
  )
}

#' viewComment module's server logic
#' 
#' @param id the module id name
#' @param pkg_name string name of the package
#' @param comments data.frame comments table entry
#' @param sring default: Current Comments 
#' 
#' 
#' 
viewCommentsServer <- function(id, pkg_name, comments, label = 'Current Comments') {
  moduleServer(id, function(input, output, session) {
    # Show the comments on the package.
    output$view_comments <- renderUI({
      req(pkg_name())
      
      tagList(
        h5(label, style = "padding-bottom:10px;"),
        wellPanel(
          HTML(showComments(pkg_name = pkg_name(), comments = comments()))
        )
      )
    })
  })
}
