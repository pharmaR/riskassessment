#####################################################################################################################
# sidebar.R - Side Bar to display the Input widgets to load the pacakge to application.
# 
# Author: Aravind
# Created: 02/06/2020.
#####################################################################################################################

# Start of the Observe's'.

# 1. Observe to update the radiobutton for decistion of the package.
observe({
  req(input$select_pack)
  if (!identical(values$selected_pkg$decision, character(0))) {
    if (values$selected_pkg$decision == "") {
      updateRadioButtons(
        session,
        "decision",
        choices = c("Yes", "No"),
        inline = TRUE,
        selected = character(0)
      )
    } else if (values$selected_pkg$decision != "") {
      updateRadioButtons(session, "decision", selected = values$selected_pkg$decision)
    }
  }
})  # End of the Observe.
# 2. Observe to disable and enable the text area comment box's' if decision of the package is empty.
observe({
  req(values$selected_pkg$package)
  if (values$selected_pkg$decision != "") {
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
})  # End of the Observe.
# 3. Observe to disable and enable to side bar elements for select pacakge input.
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
})  # End of the observe.

# Start of the Render Output's'.
# 1. Render Output to show the select input to select the package from dropdown.
output$sel_pack <- renderUI({
  res1 <- db_fun("SELECT select_packages FROM Select_packages")
  values$packsDB <- res1$select_packages
  selectizeInput(
    "select_pack",
    h3("Select Package:"),
    choices = c("Select", values$packsDB),
    selected = "Select"
  )
})  # End of the render Output.

# 2. Render Output to show the select input to select the version of the selected package.
output$sel_ver <- renderUI({
  req(input$select_pack)
  res2 <-
    db_fun(
      paste0(
        "SELECT package, version FROM Packageinfo WHERE package = '",
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
})  # End of the render Output.

# 3. Render Output to dispaly the status of the selected package.
output$status <- renderText({
  if (!is.null(input$select_pack)) {
    if (input$select_pack != "Select") {
      if (!identical(values$selected_pkg$decision, character(0))) {
        if (values$selected_pkg$decision != "") {
          paste("<h3>Status: <b>Reviewed</b></h3>")
        } else{
          paste("<h3>Status: <b>Under Review</b></h3>")
        }
      }
    } else{
      paste("<h3>Status: <b>NA</b></h3>")
    }
  }
})  # End of the render Output.

# 4. Render Output to display the score of the selected package.

output$score <- renderText({
  if (!is.null(input$select_pack)) {
    if (input$select_pack != "Select") {
      paste("<h3>Score: <b>", values$selected_pkg$score, "</b></h3>")
    } else{
      paste("<h3>Score: <b>NA</b></h3>")
    }
  }
})  # End of the render output.

# End of the Render Output's'.

# Start of the Observe Event.

# 1. Observe Event for select package
observeEvent(input$select_pack, {
  
  if (trimws(input$select_pack) != "Select" && trimws(input$select_pack) != "") {
    pkgs_in_db<-db_fun(
      paste0(
        "SELECT package FROM Packageinfo"
      )
    )
    if(!(input$select_pack %in% pkgs_in_db$package) || identical(pkgs_in_db$package, character(0))){
      get_packages_info_from_web(input$select_pack) 
      metric_mm_tm_Info_upload_to_DB(input$select_pack)
    }
    pack_ver<-db_fun(paste0("SELECT version FROM Packageinfo WHERE package = '", input$select_pack, "'"))
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
  }
})  # End of the observe Event.

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
          "<h5 class = 'mt-25 mb-0'><b>Note: </b>Once submitted the decision cannot be reverted and comments in group and package level will be freezed.</h5>"
        ),
        footer = tagList(
          actionButton("submit_confirmed_decision", "Submit", class = "submit_confirmed_decision_class btn-secondary"),
          actionButton("edit", "Edit", class = "edit_class btn-success")
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
  db_fun(
    paste(
      "UPDATE Packageinfo SET decision = '",
      input$decision,
      "' WHERE package = '",
      values$selected_pkg$package,
      "'",
      sep = ""
    )
  )
  # Render Output to display the decision to confirm the decision.
  output$decision_display <- renderText({
    des <-
      db_fun(
        paste(
          "SELECT decision FROM Packageinfo WHERE package = '",
          values$selected_pkg$package,
          "'",
          sep = ""
        )
      )
    req(des$decision)
    if (des$decision != "") {
      paste("<br>",
            "<h3>Decision: ",
            "<b>",
            des$decision,
            "</b></h3>")
    }
  })  # End of the render Output.
  
  values$selected_pkg$decision <- "yes/no"
  removeModal()
})  # End of the Observe Event.

# 4. Observe Event to edit the decision if user need to change.
observeEvent(input$edit, {
  removeModal()
})  # End of the Observe Evenet.


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
          values$selected_pkg$package,
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
      db_fun(
        paste0(
          "INSERT INTO Comments values('", values$selected_pkg$package, "',",
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
    }
  }
  
})  # End of the observe Event.

# 6. Observe Event to update overall comment.

observeEvent(input$submit_overall_comment_yes, {
  db_fun(
    paste0(
      "UPDATE Comments SET comment = '",
      values$overall_comments,
      "', added_on = '",
      TimeStamp(),
      "' WHERE comm_id = '",
      values$selected_pkg$package,
      "' AND user_name = '",
      values$name,
      "' AND user_role = '",
      values$role,
      "' AND comment_type = 'o'"
    )
  )
  values$o_comment_submitted <- "yes"
  updateTextAreaInput(session, "overall_comment", value = "")
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


# End of the Observe Event's'

# End of the Sidebar server Module.
