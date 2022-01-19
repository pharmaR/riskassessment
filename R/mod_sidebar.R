
# No UI needed given current renderUI structure in server-side logic
# #' sidebar UI Function
# #'
# #' @description A shiny Module.
# #'
# #' @param id,input,output,session Internal parameters for {shiny}.
# #'
# #' @noRd 
# #'
# #' @importFrom shiny NS tagList 
# mod_sidebar_ui <- function(id){
#   ns <- NS(id)
#   tagList(
#  
#   )
# }
    

#' sidebar Server Functions
#'
#' @noRd 
mod_sidebar_server <- function(input, output, session
                          ){ # id removed & added: input, output, session!
  # moduleServer( id, function(input, output, session){
  #   ns <- session$ns
  # })
  
  # Update the sidebar if a decision was previously made.
  observeEvent(c(input$select_pack, values$selected_pkg), {
    # Suppose package has been selected with a previously made decision.
    req(input$select_pack != "Select")
    # Update the risk slider using the info saved.
    updateSliderTextInput(
      session,
      "decision",
      choices = c("Low", "Medium", "High"),
      selected = values$selected_pkg$decision
    )
  })
  
  # Disable/enable the comments depending package selected and no decision made yet.
  observeEvent(list(input$select_pack, values$selected_pkg$decision), {
    req(input$select_pack)
    if (input$select_pack != "Select" && (is_empty(values$selected_pkg$decision) || values$selected_pkg$decision == "")) {
      enable("decision")
      enable("submit_decision")
      enable("overall_comment")
      enable("submit_overall_comment")
      
    } else{
      disable("decision")
      disable("submit_decision")
      disable("overall_comment")
      disable("submit_overall_comment")
      
    }
  }, ignoreInit = TRUE)
  
  
  # Output a dropdown ui with available packages.
  output$sel_pack <- renderUI({
    values$packsDB <- db_fun("SELECT name FROM package")
    selectizeInput(
      "select_pack",
      h3("Select Package"),
      choices = c("Select", values$packsDB$name),
      selected = "Select"
    )
  })
  
  
  # Output a dropdown ui with the available versions given the selected package.
  output$sel_ver <- renderUI({
    req(input$select_pack)
    res2 <-
      db_fun(
        paste0(
          "SELECT name, version FROM package WHERE name = '",
          input$select_pack,
          "'"
        )
      )
    
    if (input$select_pack == "Select") {
      Choices <- "Select"
    } else{
      Choices <- res2$version[[1]] 
    }
    
    selectizeInput("select_ver",
                   h3("Select Version"),
                   choices =  Choices,
                   selected = Choices)
  })
  
  
  
  
  
  
  
  # Display the review status of the selected package.
  
  output$status <- renderUI({
    req(input$select_pack)
    # status_output <-
    if(input$select_pack == "Select"){
      h1(strong("NA"))
    } else {
      if(values$selected_pkg$decision == ""){
        h3("Under Review")
      } else {
        h2("Reviewed")
      }
    }
  })
  
  # change the color of the status wellPanel's font
  observe({
    req(input$select_pack)
    valBoxColor <- case_when(
      input$select_pack == "Select" ~ "white",
      !is_empty(values$selected_pkg$decision) && values$selected_pkg$decision == "" ~ "black",
      TRUE ~ "darkblue"
    )
    runjs(sprintf("
                document.getElementById('%s').style.color = '%s';
                ", "diyValBoxStatus", valBoxColor))
  })
  
  
  # Required for shinyhelper to work.
  observe_helpers()
  
  # Display the risk score of the selected package.
  output$score <- renderUI({
    req(input$select_pack)
    
    # Score defaults to NA.
    score_output <- ifelse(input$select_pack != "Select", values$selected_pkg$score, "NA")
    h1(strong(score_output))
  })
  
  
  
  # change the color of the wellPanel
  observe({
    req(input$select_pack)
    score_output_num <- ifelse(input$select_pack != "Select", values$selected_pkg$score, NA_integer_)
    valBoxColor <- ifelse(is.na(score_output_num), "#1E90FF", colfunc(100)[round(as.numeric(score_output_num)*100)])
    runjs(sprintf("
            document.getElementById('%s').style.backgroundColor = '%s';
        ", "diyValBoxScore", valBoxColor))
  })
  
  
  
  # 1. Observe Event for select package
  observeEvent(input$select_pack, {
    
    if (trimws(input$select_pack) != "Select" && trimws(input$select_pack) != "") {
      pack_ver <- db_fun(paste0("SELECT version FROM package WHERE name = '", input$select_pack, "'"))
      updateSelectizeInput(
        session,
        "select_ver",
        selected = pack_ver
      )
      
      if (values$mm_tab_redirect == "redirect") {
        updateTabsetPanel(session, "tabs",
                          selected = "mm_tab_value")
        values$mm_tab_redirect <- "no redirect"
      }
      values$comment_occ <-
        db_fun(
          paste0(
            "SELECT comment FROM Comments WHERE comm_id = '", input$select_pack, "'
          AND user_name = '", input$name, "'
          AND user_role = '", input$role, "'
          AND comment_type = 'o'"
          )
        ) 
      updateTextAreaInput(session, "overall_comment", placeholder = values$comment_occ$comment)
    }
  })
  
  # Show a confirmation modal when submitting a decision.
  observeEvent(input$submit_decision, {
    if (!is.null(input$decision)) {
      showModal(tags$div(
        id = "confirmation_id",
        modalDialog(
          title = h2("Submit Decision", class = "mb-0 mt-0 txt-color"),
          h2("Please confirm your decision", class = "mt-0"),
          h3("Decision:", strong(input$decision)),
          h5(strong("Note:"), "Once submitted the decision cannot be reverted and
           comments in group and package level will be frozen.", class = "mt-25 mb-0"),
          footer = tagList(
            actionButton("submit_confirmed_decision", "Submit",
                         class = "submit_confirmed_decision_class btn-secondary"),
            actionButton("edit", "Cancel", class = "edit_class btn-unsuccess")
          )
        )
      ))
    } else{
      showModal(modalDialog(
        title = h3("WARNING!", class = 'txt-danger'),
        h4("Please select a Decision!")
      ))
    }
  })
  
  # Update database info after decision is submitted.
  observeEvent(input$submit_confirmed_decision, {
    db_ins(
      paste0(
        "UPDATE package SET decision = '",
        input$decision,
        "' WHERE name = '",
        values$selected_pkg$name,
        "'"
      )
    )
    values$selected_pkg$decision <- input$decision
    removeModal()
    loggit("INFO", paste("decision for the package", values$selected_pkg$name,
                         "is", input$decision, 
                         "by", values$name, "(", values$role, ")"))
    
    # After decision submitted, update db dash.
    values$db_pkg_overview <- update_db_dash()
    
  })
  
  # 4. Observe Event to edit the decision if user need to change.
  observeEvent(input$edit, {
    removeModal()
  })
  
  
  # 5. Observe Event to update overall comment.
  
  values$o_comment_submitted <- "no"
  observeEvent(input$submit_overall_comment, {
    
    overall_comment <- input$overall_comment
    values$overall_comments <- trimws(overall_comment)
    if (values$overall_comments != "") {
      
      comments_submitted <-
        db_fun(
          paste0(
            "SELECT * FROM Comments WHERE comment_type = 'o' AND comm_id = '",
            values$selected_pkg$name,
            "'"
          )
        )
      if (values$name %in% comments_submitted$user_name &&
          values$role %in% comments_submitted$user_role) {
        comment_submitted <-
          filter(
            comments_submitted,
            comments_submitted$user_name == values$name &
              comments_submitted$user_role == values$role
          )
        showModal(modalDialog(
          title = h2("Update Comment", class = "mb-0 mt-0 txt-color"),
          h3("Do you want to update your previous comment?", class = "mt-0"),
          br(),
          h4(strong("Previous Comment:")),
          h5(comment_submitted$comment),
          h4(strong("Current Comment:")),
          h5(values$overall_comments),
          HTML(
            "<h5 class = 'mt-25 mb-0'><b>Note: </b> <br>Yes - Overwrites the previous comment.<br>Edit - Go back to editing the comment.<br>No - Exits from window and removes the text in comment box.</h5>"
          ),
          footer = tagList(
            actionButton("submit_overall_comment_yes", "Yes", class = "submit_overall_comment_yes btn-success"),
            actionButton("submit_overall_comment_edit", "Edit", class = "submit_overall_comment_yes btn-secondary"),
            actionButton("submit_overall_comment_no", "No", class = "submit_overall_comment_no btn-unsuccess")
          )
        ))
      } else{
        db_ins(
          paste0(
            "INSERT INTO Comments values('", values$selected_pkg$name, "',",
            "'", values$name, "'," ,
            "'", values$role, "',",
            "'", values$overall_comments, "',",
            "'o',",
            "'", TimeStamp(), "'" ,
            ")"
          )
        )
        values$o_comment_submitted <- "yes"
        updateTextAreaInput(session, "overall_comment", value = "")
        updateTextAreaInput(session, "overall_comment", placeholder = values$overall_comments)
      }
      
      # After comment added to Comments table, update db dash
      values$db_pkg_overview <- update_db_dash()
    }
  })
  
  # 6. Observe Event to update overall comment.
  observeEvent(input$submit_overall_comment_yes, {
    db_ins(
      paste0(
        "UPDATE Comments SET comment = '",
        values$overall_comments,
        "', added_on = '",
        TimeStamp(),
        "' WHERE comm_id = '",
        values$selected_pkg$name,
        "' AND user_name = '",
        values$name,
        "' AND user_role = '",
        values$role,
        "' AND comment_type = 'o'"
      )
    )
    values$o_comment_submitted <- "yes"
    updateTextAreaInput(session, "overall_comment", value = "")
    updateTextAreaInput(session, "overall_comment", placeholder = values$overall_comments)
    removeModal()
  })
  
  # 7. Observe Event to edit overall comment.
  
  observeEvent(input$submit_overall_comment_edit, {
    removeModal()
  })
  
  # 8. Observe Event to remove text in comment box.
  
  observeEvent(input$submit_overall_comment_no, {
    updateTextAreaInput(session, "overall_comment", value = "")
    removeModal()
  })
  
}
    
## To be copied in the UI
# mod_sidebar_ui("sidebar_ui_1")
    
## To be copied in the server
# mod_sidebar_server("sidebar_ui_1")
