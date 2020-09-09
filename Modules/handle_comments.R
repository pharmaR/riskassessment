upd_comments <- function(cmnt) {
  
  # extract the characters between $ and _ using lookbehind and lookahead
  cm_type <- str_extract(deparse(substitute(cmnt)), regex("(?<=\\$)([a-z]+)(?=_)") )
  
  if (trimws(cmnt) != "") {
    db_ins(
      paste0(
        "INSERT INTO Comments values('",
        input$select_pack,
        "',",
        "'",
        values$name,
        "'," ,
        "'",
        values$role,
        "',",
        "'",
        cmnt,
        "',",
        "'",
        cm_type,
        "',",
        "'",
        TimeStamp(),
        "'"  ,
        ")"
      )
    )
    if (cm_type == "mm")  values$mm_comment_submitted <- "yes"
    if (cm_type == "tm")  values$tm_comment_submitted <- "yes"
    if (cm_type == "cum") values$cum_comment_submitted <- "yes"
    
    txt <- paste0(cm_type,"_comment")
    updateTextAreaInput(session, txt , value = "")
  }
}    

toListen <- reactive({
  list(input$submit_tm_comment,input$submit_mm_comment, input$submit_cum_comment)
})
observeEvent(toListen(), {
  req(input$select_pack != "")
  
  values$mm_comment_submitted <- "no"
  values$tm_comment_submitted <- "no"
  values$cum_comment_submitted <- "no"
  
  if (!is.null(input$submit_mm_comment)  && input$submit_mm_comment  > 0) upd_comments(input$mm_comment)
  if (!is.null(input$submit_tm_comment)  && input$submit_tm_comment  > 0) upd_comments(input$tm_comment)
  if (!is.null(input$submit_cum_comment) && input$submit_cum_comment > 0) upd_comments(input$cum_comment)
  
}, ignoreInit = TRUE)
