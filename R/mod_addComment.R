#' The UI for the 'addComment' module
#'
#' Module to display comments for the specified metric. The comments will update
#' as the user inserts more comments.
#' 
#' @param id a module id name
#' @keywords internal
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
#' @keywords internal
#' 
addCommentServer <- function(id, metric_abrv, user, approved_roles, pkg_name) {
  if (missing(approved_roles))
    approved_roles <- get_golem_config("credentials", file = app_sys("db-config.yml"))[["privileges"]]
  
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
          "Commenting as user: {user$name}, role: {user$role}"
        )
      )
    })
    
    observeEvent(input$submit_comment, {
      req(input$add_comment)
      
      comment <- trimws(input$add_comment)
      
      if (comment != "") {
        
        dbUpdate(
        "INSERT INTO comments values({pkg_name()}, {user$name}, 
        {user$role}, {comment}, {metric_abrv},
        {getTimeStamp()})"
        )
        
        updateTextAreaInput(session, "add_comment", value = "")
      }
    })
    
    reactive(input$add_comment)
  })
}
