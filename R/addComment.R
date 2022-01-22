# Module to display comments for the specified metric.
# The comments will update as the user inserts more comments.
addCommentUI <- function(id) {
  fluidRow(
    column(
      width = 12,
      uiOutput(NS(id, "add_comment_ui")),
      actionButton(NS(id, "submit_comment"), "Submit")
    )
  )
}

addCommentServer <- function(id, metric_abrv, user_name, user_role, pkg_name) {
  moduleServer(id, function(input, output, session) {
    
    output$add_comment_ui <- renderUI({
      
      metric_name <- ifelse(metric_abrv == "mm",
                            "Maintenance Metrics",
                            "Community Usage Metrics")
      
      textAreaInput(
        NS(id, "add_comment"),
        h5(glue("Add Comment for {metric_name}")),
        width = "100%",
        rows = 4,
        placeholder = glue(
          "Commenting as user: {user_name()}, role: {user_role()}"
        )
      )
    })
    
    observeEvent(input$submit_comment, {
      req(input$add_comment)
      
      if (trimws(input$add_comment) != "") {
        db_ins(glue(
        "INSERT INTO comments values('{pkg_name()}', '{user_name()}', 
        '{user_role()}', '{input$add_comment}', '{metric_abrv}',
        '{TimeStamp()}')")
        )
        
        updateTextAreaInput(session, "add_comment", value = "")
      }
    })
    
    reactive(input$add_comment)
  })
}