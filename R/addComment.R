#' The UI for the 'addComment' module
#'
#' Module to display comments for the specified metric. The comments will update
#' as the user inserts more comments.
#' 
#' @param id a module id name
#' 
#' 
#' 
addCommentUI <- function(id) {
  fluidRow(
    column(
      width = 12,
      uiOutput(NS(id, "add_comment_ui")),
      actionButton(NS(id, "submit_comment"), "Submit")
    )
  )
}

#' addComment module's server logic
#' 
#' @param id a module id name
#' @param metric_abrv placeholder
#' @param user_name placeholder
#' @param user_role placeholder
#' @param pkg_name string name of the package
#' 
#' 
#' @import dplyr
#' @importFrom glue glue
#' @importFrom stringr str_replace_all
#' 
addCommentServer <- function(id, metric_abrv, user_name, user_role, pkg_name) {
  moduleServer(id, function(input, output, session) {
    
    output$add_comment_ui <- renderUI({
      
      metric_name <- ifelse(metric_abrv == "mm",
                            "Maintenance Metrics",
                            "Community Usage Metrics")
      
      textAreaInput(
        session$ns("add_comment"),
        h5(glue::glue("Add Comment for {metric_name}")),
        width = "100%",
        rows = 4,
        placeholder = glue::glue(
          "Commenting as user: {user_name()}, role: {user_role()}"
        )
      )
    })
    
    observeEvent(input$submit_comment, {
      req(input$add_comment)
      
      comment <- trimws(input$add_comment)
      
      if (comment != "") {
        
        # TODO: comments can't contain "'". Check for other invalid
        # characters.
        # if(str_count(string = comment, pattern = "'") != 0)
        #   validate("Invalid character: comments cannot contain single
        #            quotes (')")
        
        comment <- stringr::str_replace_all(comment, "'", "''")

        dbUpdate(glue::glue(
        "INSERT INTO comments values('{pkg_name()}', '{user_name()}', 
        '{user_role()}', '{comment}', '{metric_abrv}',
        '{getTimeStamp()}')")
        )
        
        updateTextAreaInput(session, "add_comment", value = "")
      }
    })
    
    reactive(input$add_comment)
  })
}
