sidebarUI <- function(id) {
  tagList(
    tags$b(h4("Package Control Panel", style = "text-align: center;")),
    
    hr(),
    
    selectizeInput(
      inputId = NS(id, "select_pkg"),
      label = h5("Select Package"),
      choices = "-",
      selected = "-"
    ),
    
    selectizeInput(
      inputId = NS(id, "select_ver"),
      label = h5("Select Version"),
      choices = "-",
      selected = "-"
    ),
    
    br(), br(),
    
    fluidRow(
      column(6, wellPanel(
        h5("Status"),
        htmlOutput(NS(id, "status"))
      )),
      column(6, wellPanel(
        h5("Risk"),
        htmlOutput(NS(id, "score"))
      ))
    ),
    
    textAreaInput(
      inputId = NS(id, "overall_comment"),
      h5("Select Overall Comment"),
      rows = 5,
      placeholder = paste("Current Comment:")
    ),
    
    # Submit Overall Comment for selected Package.
    actionButton(NS(id, "submit_overall_comment"), "Submit Comment"),
    
    div(
      HTML("<i class='fas fa-info-circle fa-2x float-right txt-color cursor-help' title='Once submitted the decision cannot be reverted and comments in group and package level will be frozen'></i>"),
      # Slider input to select the decision for selected package.
      sliderTextInput(
        inputId = NS(id, "decision"),
        h3("Overall Risk:"),
        selected = NULL,
        grid = TRUE,
        c("Low", "Medium", "High")
      ),
      
      # Action button to submit decision for selected package.
      actionButton(NS(id, "submit_decision"), "Submit Decision")
    )
  )
}

sidebarServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Required for shinyhelper to work.
    observe_helpers()
    
    # Get information about selected package.
    selected_pkg <- reactive({
      req(input$select_pkg)
      req(input$select_ver)
  
      db_fun(glue(
        "SELECT name, score, decision, version
        FROM package
        WHERE name = '{input$select_pkg}'"))
    })

    # Update package list (just once).
    observeEvent(input$select_pkg, {
      updateSelectizeInput(
        inputId = "select_pkg",
        choices = c("-", db_fun("SELECT name as package FROM package")),
        selected = "-"
      )
    }, once = TRUE)
    
    # Update package version.
    observeEvent(input$select_pkg, {
      req(input$select_pkg)
      req(input$select_ver)
      
      version <- ifelse(input$select_pkg == "-", "-", selected_pkg()$version)
      
      updateSelectizeInput(
        session,
        "select_ver",
        choices = version,
        selected = version
      )
    })
    
    # Display the review status of the selected package.
    output$status <- renderUI({
      req(input$select_pkg)
      req(input$select_ver)
      
      if(input$select_pkg == "-")
        validate("Please select a package")
      if(input$select_ver == "-")
        validate("Please select a version")
      
      
      if(selected_pkg()$decision == "") h2("Under Review")
      else h2("Reviewed")
    })
    
    # Update the sidebar if a decision was previously made.
    observeEvent(input$select_pkg, {
      # Suppose package has been selected with a previously made decision.
      req(input$select_pkg != "-")
      # Update the risk slider using the info saved.
      updateSliderTextInput(
        session,
        "decision",
        choices = c("Low", "Medium", "High"),
        selected = selected_pkg()$decision
      )
      
      if (input$select_pkg != "Select" && (is_empty(selected_pkg()$decision) || selected_pkg()$decision == "")) {
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
    
    # Display the risk score of the selected package.
    output$score <- renderUI({
      req(input$select_pkg)
      
      # Score defaults to NA.
      score_output <- ifelse(input$select_pkg != "Select", selected_pkg()$score, "NA")
      h1(strong(score_output))
    })
    

    
    observeEvent(input$select_pkg, {
      if (trimws(input$select_pkg) != "Select" && trimws(input$select_pkg) != "") {

        comments <- db_fun(glue(
          "SELECT comment FROM Comments WHERE comm_id = '", input$select_pkg, "'
          AND user_name = '", input$name, "'
          AND user_role = '", input$role, "'
          AND comment_type = 'o'"))
        
        updateTextAreaInput(session, "overall_comment", placeholder = paste("current comment:", comments$comment))
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
      db_ins(glue(
          "UPDATE package
          SET decision = '{input$decision}'
          WHERE name = '{selected_pkg()$name}'"
        )
      )
      
      removeModal()
      
      loggit("INFO",
             glue("decision for the package {input$decisione} is {input$decision}
                  by {selected_pkg$name} ({values$role})"))
    })
    
    # 4. Observe Event to edit the decision if user need to change.
    observeEvent(input$edit, {
      removeModal()
    })
    
    observeEvent(input$submit_overall_comment, {
      
      overall_comment <- input$overall_comment
      values$overall_comments <- trimws(overall_comment)
      if (values$overall_comments != "") {
        
        comments_submitted <-
          db_fun(
            paste0(
              "SELECT * FROM Comments WHERE comment_type = 'o' AND comm_id = '",
              selected_pkg$name,
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
              "INSERT INTO Comments values('", selected_pkg()$name, "',",
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
    })
    
    # 6. Observe Event to update overall comment.
    observeEvent(input$submit_overall_comment_yes, {
      db_ins(glue(
          "UPDATE Comments
          SET comment = '{values$overall_comments}' added_on = '{TimeStamp()}'
          WHERE comm_id = '{selected_pkg()$name}' AND
          user_name = '{values$name}' AND
          user_role = '{values$role}' AND
          comment_type = 'o'"
        )
      )

      updateTextAreaInput(session, "overall_comment", value = "")
      updateTextAreaInput(session, "overall_comment", placeholder = paste("current comment:", values$overall_comments))
      removeModal()
    })
    
    # Remove modal that edits overall comment.
    observeEvent(input$submit_overall_comment_edit, {
      removeModal()
    })
    
    # Modal to remove text in comment box.
    observeEvent(input$submit_overall_comment_no, {
      updateTextAreaInput(session, "overall_comment", value = "")
      removeModal()
    })
  })
}