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
      column(6, div(id = NS(id, "status-wp"), wellPanel(
        h5("Status"),
        htmlOutput(NS(id, "status"))
      ))),
      column(6, div(id = NS(id, "score-wp"), wellPanel(
        h5("Risk"),
        htmlOutput(NS(id, "score"))
      ))
    ),
    
    br(), br(),
    
    disabled(
      div(id = NS(id, "decision-grp"),
        sliderTextInput(
          inputId = NS(id, "decision"),
          h5("Select Overall Risk"), 
          selected = NULL,
          grid = TRUE,
          c("Low", "Medium", "High")
        ),
        
        # Action button to submit decision for selected package.
        actionButton(NS(id, "submit_decision"), "Submit Decision", width = "100%")
      ),
      br(), br(),
      
      div(id = NS(id, "overall-comment-grp"),
        textAreaInput(
          inputId = NS(id, "overall_comment"),
          h5("Write Overall Comment"),
          rows = 5,
          placeholder = ""
        ),
        
        # Submit Overall Comment for selected Package.
        actionButton(NS(id, "submit_overall_comment"),
                     "Submit Comment", width = "100%")
        )
      )
    )
  )
}

sidebarServer <- function(id, user, uploaded_pkgs) {
  moduleServer(id, function(input, output, session) {
    
    # Required for shinyhelper to work.
    observe_helpers()
    
    # Create list of packages.
    output$select_pkg_ui <- renderUI({
      selectizeInput(
        inputId = NS(id, "select_pkg"),
        label = h5("Select Package"),
        choices = c("-", dbSelect('SELECT name FROM package')$name),
        selected = "-"
      )
    })
    
    # Create list of packages.
    observeEvent(uploaded_pkgs(), {
      req(input$select_pkg)
      
      updateSelectizeInput(
        inputId = "select_pkg",
        choices = c("-", dbSelect('SELECT name FROM package')$name),
        selected = "-"
      )
      
    }, ignoreNULL = TRUE)
    
    # Get information about selected package.
    selected_pkg <- reactiveVal()
    
    observe({
      req(input$select_pkg)
      req(input$select_ver)

      selected_pkg(dbSelect(glue(
        "SELECT *
        FROM package
        WHERE name = '{input$select_pkg}'")))
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
    }, ignoreInit = TRUE)
    
    # Display the review status of the selected package.
    output$status <- renderUI({
      req(input$select_pkg)
      req(input$select_ver)
      
      if(input$select_pkg == "-")
        validate("Please select a package")
      if(input$select_ver == "-")
        validate("Please select a version")
      
      status <- ifelse(selected_pkg()$decision == '', "Under Review", "Reviewed")
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
    
    # Display/update overall comments for selected package/version.
    observeEvent(input$select_ver, {
      req(input$select_pkg)
      req(input$select_ver)

      # If no package/version is selected, then clear comments.
      if(input$select_pkg == "-" || input$select_ver == "-"){
        updateTextAreaInput(session, "overall_comment",
                            placeholder = 'Please select a package and a version.')
      }
      else {
        # Display package comments if a package and version are selected.
        comments <- dbSelect(glue(
          "SELECT comment FROM comments
          WHERE id = '{input$select_pkg}'
          AND comment_type = 'o'"))$comment
        
        updateTextAreaInput(session, "overall_comment",
                            placeholder = glue('Current Overall Comment: {comments}'))
      }
    })
    
    # Update db if comment is submitted.
    observeEvent(input$submit_overall_comment, {
      
      current_comment <- trimws(input$overall_comment)
      
      if(current_comment == "")
        validate("Please enter a comment.")
      
      req(selected_pkg())
      
      previous_comments <- 
        dbSelect(glue(
          "SELECT *
            FROM comments
            WHERE comment_type = 'o' AND id = '{selected_pkg()$name}'"))
      
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
            actionButton(NS(id, "submit_overall_comment_yes"), "Yes"),
            actionButton(NS(id, "submit_overall_comment_edit"), "Edit"),
            actionButton(NS(id, "submit_overall_comment_no"), "No")
          )
        ))
      } else{
        dbUpdate(glue(
          "INSERT INTO comments
          VALUES ('{selected_pkg()$name}', '{user$name}', '{user$role}',
          '{current_comment}', 'o', '{getTimeStamp()}')"))
        
        updateTextAreaInput(session, "overall_comment",
                            placeholder = glue('Current Comment: {current_comment}'))
      }
    })
    
    observeEvent(input$submit_overall_comment_yes, {

      req(selected_pkg())

      dbUpdate(
        glue(
          "UPDATE comments
          SET comment = '{input$overall_comment}', added_on = '{getTimeStamp()}'
          WHERE id = '{selected_pkg()$name}' AND
          user_name = '{user$name}' AND
          user_role = '{user$role}' AND
          comment_type = 'o'"
        )
      )
      current_comment <- trimws(input$overall_comment)
      updateTextAreaInput(session, "overall_comment", value = "",
                          placeholder = glue('Current Comment: {current_comment}'))
      removeModal()
    })

    observeEvent(input$submit_overall_comment_edit, {
      removeModal()
    })
    
    observeEvent(input$submit_overall_comment_no, {
      updateTextAreaInput(session, "overall_comment", value = "")
      removeModal()
    })
    
    # Update decision when package is selected.
    observeEvent(input$select_ver, {
      req(input$select_pkg)
      req(input$select_ver)
      
      # Reset decision if no package/version is selected.
      if(input$select_pkg == "-" || input$select_ver == "-") {
        updateSliderTextInput(
          session,
          "decision",
          choices = c("Low", "Medium", "High"),
          selected = 'Low'
        )
        
        validate('Please select a package and a version.')
      }
      
      req(selected_pkg())
      
      # Update the risk slider using the info saved.
      updateSliderTextInput(
        session,
        "decision",
        choices = c("Low", "Medium", "High"),
        selected = selected_pkg()$decision
      )
    })
    
    # Enable/disable sidebar decision and comment.
    observeEvent(input$select_ver, {
      if (input$select_pkg != "-" && input$select_pkg != "-" &&
          (is_empty(selected_pkg()$decision) || selected_pkg()$decision == "")) {
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
      req(input$decision)
      
      showModal(modalDialog(
        size = "l",
        easyClose = TRUE,
        h5("Submit Decision", style = 'text-align: center !important'),
        hr(),
        br(),
        fluidRow(
          column(
            width = 12,
            'Please confirm your choosen risk: ', span(class = 'text-info', input$decision),
            br(),
            br(),
            em('Note: Once submitted, this final risk cannot be reverted.')
          )
        ),
        br(),
        footer = tagList(
          actionButton(NS(id, 'submit_confirmed_decision'), 'Submit'),
          actionButton(NS(id, 'cancel'), 'Cancel')
        )))
    })
    
    # Close modal if user cancels decision submission.
    observeEvent(input$cancel, {
      removeModal()
    })
    
    # Update database info after decision is submitted.
    observeEvent(input$submit_confirmed_decision, {
      dbUpdate(glue(
        "UPDATE package
          SET decision = '{input$decision}'
          WHERE name = '{selected_pkg()$name}'")
      )
      
      disable("decision")
      disable("submit_decision")
      disable("overall_comment")
      disable("submit_overall_comment")
      
      removeModal()
      
      loggit("INFO",
             glue("decision for the package {selected_pkg()$name} is {input$decision}
                  by {user$name} ({user$role})"))
    })
    
    # Output package id, name, and version.
    # TODO: return the entire selected_pkg instead of doing this below.
    list(
      id = reactive(selected_pkg()$id),
      name = reactive(selected_pkg()$name),
      version = reactive(selected_pkg()$version),
      title = reactive(selected_pkg()$title),
      decision = reactive(selected_pkg()$decision),
      description = reactive(selected_pkg()$description),
      author = reactive(selected_pkg()$author),
      maintainer = reactive(selected_pkg()$maintainer),
      license = reactive(selected_pkg()$license),
      published = reactive(selected_pkg()$published),
      overall_comment_added = reactive(input$submit_overall_comment)
    )
  })
}