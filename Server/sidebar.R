#####################################################################################################################
# sidebar.R - Side Bar to display the Input widgets to load the pacakge to application.
# Author: K Aravind Reddy
# Date: July 13th, 2020
# License: MIT License
#####################################################################################################################


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


# Disable/enable the comments depending on wether a decision has been made.
observe({
  req(values$selected_pkg$name)
  if (values$selected_pkg$decision != "") {
    # Disable all the decision-related choices.
    disable("decision")
    disable("submit_decision")
    disable("overall_comment")
    disable("submit_overall_comment")
  } else{
    enable("decision")
    enable("submit_decision")
    enable("overall_comment")
    enable("submit_overall_comment")
  }
})


observe({
  req(input$select_pack)
  if (input$select_pack == "Select") {
    disable("decision")
    disable("submit_decision")
    disable("overall_comment")
    disable("submit_overall_comment")
  } else{
    enable("decision")
    enable("submit_decision")
    enable("overall_comment")
    enable("submit_overall_comment")
  }
})


# Output a dropdown ui with available packages.
output$sel_pack <- renderUI({
  values$packsDB <- db_fun("SELECT name FROM package")
  selectizeInput(
    "select_pack",
    h3("Select Package:"),
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
  
  if (input$select_pack == "Select" || nrow(res2) > 1) {
    Choices <- c("Select", c(res2$version))
  } else{
    Choices <- c(res2$version)
  }
  
  selectInput("select_ver",
              h3("Select Version:"),
              choices = Choices,
              selected = "Select")
})


# Display the review status of the selected package.
output$status <- renderUI({
  if (!is.null(input$select_pack)) {
    
    # Defaults to NA.
    status_output <- "NA"
    
    if (input$select_pack != "Select") {
        status_output <- ifelse(
          values$selected_pkg$decision == "",
          "Under Review",
          "Reviewed")
    }
    
    h3("Status:", strong(status_output))
  }
})


# Display the risk score of the selected package.
output$score <- renderUI({
  if (!is.null(input$select_pack)) {
    if (input$select_pack != "Select") {
      paste("<h3>Score: <b>", values$selected_pkg$score, "</b></h3>")
    } else{
      paste("<h3>Score: <b>NA</b></h3>")
    }
  }
})

# End of the Render Output's'.

# Start of the Observe Event.

# 1. Observe Event for select package

observeEvent(input$select_pack, {
  
  if (trimws(input$select_pack) != "Select" && trimws(input$select_pack) != "") {
    pack_ver <- db_fun(paste0("SELECT version FROM package WHERE name = '", input$select_pack, "'"))
    updateSelectizeInput(
      session,
      "select_ver",
      choices = pack_ver[1,1]
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
    updateTextAreaInput(session, "overall_comment", placeholder = paste("current comment:", values$comment_occ$comment))
  }
})

# 2. Observe Event to submit the decision for selected package.

observeEvent(input$submit_decision, {
  if (!is.null(input$decision)) {
    showModal(tags$div(
      id = "confirmation_id",
      modalDialog(
        title = h1("CONFIRMATION!", class = "mb-0 mt-0 txt-color"),
        tags$h2("Please confirm your decision", class = "mt-0"),
        HTML(
          paste("<h3>Decision:", "<b>", input$decision, "</b></h3>")
        ),
        HTML(
          "<h5 class = 'mt-25 mb-0'><b>Note: </b>Once submitted the decision cannot be reverted and comments in group and package level will be frozen.</h5>"
        ),
        footer = tagList(
          actionButton("submit_confirmed_decision", "Submit", class = "submit_confirmed_decision_class btn-secondary"),
          actionButton("edit", "Cancel", class = "edit_class btn-unsuccess")
        )
      )
    ))
  } else{
    showModal(modalDialog(
      title = HTML("<h3 class = 'txt-danger'>WARNING!</h3>"),
      tags$h4("Please select a Decision!")
    ))
  }
})  # End of the Observe Event.

# 3. Observe Event for submit the decision.

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
  
})  # End of the Observe Event.

# 4. Observe Event to edit the decision if user need to change.

observeEvent(input$edit, {
  removeModal()
})  # End of the Observe Event.


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
        title = h1("CONFIRMATION!", class = "mb-0 mt-0 txt-color"),
        HTML(
          paste("<b><h3>Previous Comment:</b>", "<br>", "<h4>", comment_submitted$comment, "</h4>")
        ),
        tags$h3("Do you want to update your previous comment?", class = "mt-0"),
        HTML(
          paste("<b><h3>Current Comment:</b>", "<br>", "<h4>", values$overall_comments, "</h4>")
        ),
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
      updateTextAreaInput(session, "overall_comment", placeholder = paste("current comment:", values$overall_comments))
    }
    
    # After comment added to Comments table, update db dash
    values$db_pkg_overview <- update_db_dash()
  }
  
})  # End of the observe Event.

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
  updateTextAreaInput(session, "overall_comment", placeholder = paste("current comment:", values$overall_comments))
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
