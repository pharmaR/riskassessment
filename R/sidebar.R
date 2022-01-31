sidebarUI <- function(id) {
  tagList(
    tags$b(h4("Package Control Panel", style = "text-align: center;")),
    
    hr(),
    
    uiOutput(NS(id, 'select_pkg_ui')),
    
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
    
    br(), br(),
    
    sliderTextInput(
      inputId = NS(id, "decision"),
      h5("Select Overall Risk"), 
      selected = NULL,
      grid = TRUE,
      c("Low", "Medium", "High")
    ),
    
    # Action button to submit decision for selected package.
    actionButton(NS(id, "submit_decision"), "Submit Decision", width = "100%"),
    
    br(), br(),
    
    textAreaInput(
      inputId = NS(id, "overall_comment"),
      h5("Select Overall Comment"),
      rows = 5,
      placeholder = ""
    ),
    
    # Submit Overall Comment for selected Package.
    actionButton(NS(id, "submit_overall_comment"), "Submit Comment", width = "100%")
  )
}

sidebarServer <- function(id, uploaded_pkgs) {
  moduleServer(id, function(input, output, session) {
    
    # Required for shinyhelper to work.
    observe_helpers()
    
    # Create list of packages.
    output$select_pkg_ui <- renderUI({
      selectizeInput(
        inputId = NS(id, "select_pkg"),
        label = h5("Select Package"),
        choices = c("-", db_fun('SELECT name FROM package')),
        selected = "-"
      )
    })
    
    # Create list of packages.
    observeEvent(uploaded_pkgs(), {
      req(input$select_pkg)
      
      updateSelectizeInput(
        inputId = "select_pkg",
        #label = h5("Select Package"),
        choices = c("-", db_fun('SELECT name FROM package')$name),
        selected = "-"
      )
      
    }, ignoreNULL = TRUE)
    
    # Get information about selected package.
    selected_pkg <- reactive({
      req(input$select_pkg)
      req(input$select_ver)
      
      db_fun(glue(
        "SELECT name, score, decision, version, id
        FROM package
        WHERE name = '{input$select_pkg}'"))
    })
    
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
      
      status <- ifelse(selected_pkg()$decision == "", "Under Review", "Reviewed")
      h5(status)
    })
    
    # Display the risk score of the selected package.
    output$score <- renderUI({
      req(input$select_pkg)
      req(input$select_ver)
      
      if(input$select_pkg == "-")
        validate("Please select a package")
      if(input$select_ver == "-")
        validate("Please select a version")
      
      req(selected_pkg())
      
      h5(selected_pkg()$score)
    })
    
    # Display comments for selected package.
    observeEvent(input$select_pkg, {
      req(input$select_pkg)
      req(input$select_ver)
      
      if(input$select_pkg == "-")
        validate("Please select a package")
      if(input$select_ver == "-")
        validate("Please select a version")
      
      comments <- db_fun(glue(
        "SELECT comment FROM comments
          WHERE comm_id = '{input$select_pkg}'
          AND comment_type = 'o'"))
      
      updateTextAreaInput(session, "overall_comment", placeholder = comments$comment)
    })
    
    # Update db if comment is submitted.
    observeEvent(input$submit_overall_comment, {
      
      current_comment <- trimws(input$overall_comment)
      
      if(current_comment == "")
        validate("Please enter a comment.")
      
      previous_comments <- 
        db_fun(glue(
          "SELECT *
            FROM comments
            WHERE comment_type = 'o' AND comm_id = '{selected_pkg()$name}'"))
      
      if (nrow(previous_comments) != 0) {
        showModal(modalDialog(
          title = h2("Update Comment"),
          h3("Do you want to update your previous comment?"),
          br(),
          h5(strong("Previous Comment:")),
          h5(previous_comments$comment),
          h5(strong("Current Comment:")),
          h5(current_comment),
          HTML("Yes - Overwrites the previous comment.<br>
               Edit - Go back to editing the comment.<br>
               No - Exits from window and removes the text in comment box."
          ),
          footer = tagList(
            actionButton("submit_overall_comment_yes", "Yes", class = "btn-success"),
            actionButton("submit_overall_comment_edit", "Edit", class = "btn-secondary"),
            actionButton("submit_overall_comment_no", "No", class = "btn-unsuccess")
          )
        ))
      } else{
        print(glue("values('{selected_pkg()$name}', '{input$user$name}', '{input$user$rule}', '{current_comment}', 'o', '{TimeStamp()}')"))
        db_ins(glue("INSERT INTO comments
                    values('{selected_pkg()$name}', '{input$user$name}', '{input$user$rule}', '{current_comment}', 'o', '{TimeStamp()}')"))
        
        updateTextAreaInput(session, "current_comment", placeholder = paste("current comment:", current_comment))
      }
    })
    
    # Update overall comment by user's request.
    observeEvent(input$submit_overall_comment_yes, {
      db_ins(glue(
        "UPDATE comments
          SET comment = '{input$overall_comment}' added_on = '{TimeStamp()}'
          WHERE comm_id = '{selected_pkg()$name}' AND
          user_name = '{input$user$name}' AND
          user_role = '{input$user$rule}' AND
          comment_type = 'o'"
      ))
      
      updateTextAreaInput(session, "overall_comment", placeholder = paste("current comment:", overall_comment))
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
          h5("Please select a Decision!")
        ))
      }
    })
    
    # Update database info after decision is submitted.
    observeEvent(input$submit_confirmed_decision, {
      db_ins(glue(
        "UPDATE package
          SET decision = '{input$decision}'
          WHERE name = '{selected_pkg()$name}'")
      )
      
      removeModal()
      
      loggit("INFO",
             glue("decision for the package {input$decisione} is {input$decision}
                  by {selected_pkg()$name} ({input$user$rule})"))
    })
    
    # 4. Observe Event to edit the decision if user need to change.
    observeEvent(input$edit, {
      removeModal()
    })
    
    # Output package id, name, and version.
    list(
      id = reactive(db_fun(glue(
        "SELECT id
        FROM package
        WHERE name = '{selected_pkg()$name}';"))$id),
      name = reactive(input$select_pkg),
      version = reactive(input$select_ver)
    )
  })
}