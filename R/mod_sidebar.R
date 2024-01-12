#' Sidebar UI
#' 
#' Also known as the 'Control Panel', and rightfully so, as it controls
#' most components of the app, central to it's function
#' 
#' @param id a module id
#' 
#' @importFrom shinyjs disabled
#' @keywords internal
sidebarUI <- function(id) {
  tagList(
    tags$b(h4("Package Control Panel", style = "text-align: center;")),
    
    hr(),

    selectizeInput(
      inputId = NS(id, "select_pkg"),
      label = h5("Package Name"),
      choices = "-",
      selected = "-"
    ) %>%
      shinyjs::disabled() %>%
      div(id = NS(id, "select_pkg_ui")),
    

    fluidRow(

      column(6,
        tags$label("Date Uploaded", class = c("control-label", "h5")),
        selectizeInput(
          inputId = NS(id, "select_date"),
          label = NULL, #h5("Date Uploaded"),
          choices = "-",
          selected = "-"
        ) %>%
          shinyjs::disabled()
      ),

      column(6,
        tags$label("Pkg Version",
                   icon("circle-info", class = "fa-xs",
                     title = "The most recent package version avaiable at date of upload will be used."),
                   class = c("control-label", "h5")),
        selectizeInput(
          inputId = NS(id, "select_ver"),
          label = NULL,
          choices = "-",
          selected = "-"
        ) %>%
          shinyjs::disabled(),
      )
    ),
    tags$label("Package Source", class = c("control-label", "h5")),
    verbatimTextOutput(NS(id, "repo_url"), placeholder = TRUE),
    br(),
    
    fluidRow(
      column(6, div(id = NS(id, "status-wp"), wellPanel(
        h5("Status"),
        htmlOutput(NS(id, "status"))
      ))),
      column(6, div(id = NS(id, "score-wp"), wellPanel(
        h5("Metric Risk"),
        htmlOutput(NS(id, "score"))
      ))
    ),
    
    br(), br(), br(),
    
    shinyjs::disabled(
      div(id = NS(id, "decision-grp"),
        shinyWidgets::sliderTextInput(
          inputId = NS(id, "decision"),
          h5("Select Overall Decision"), 
          selected = NULL,
          grid = TRUE,
          if (!is.null(golem::get_golem_options("decision_categories"))) golem::get_golem_options("decision_categories") else c("Low Risk", "Medium Risk", "High Risk"),
          force_edges = TRUE
        ),
        
        # Action button to reset decision for selected package.
        uiOutput(NS(id, "reset_decision_ui")),
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

#' Sidebar Server Logic
#' 
#' Also known as the 'Control Panel', and rightfully so, as it controls
#' most components of the app, central to it's function
#' 
#' @param id a module id
#' @param user a username
#' @param uploaded_pkgs a vector of packages
#' 
#' 
#' @importFrom shinyjs enable disable
#' @keywords internal
#' 
sidebarServer <- function(id, user, uploaded_pkgs, credentials) {
  if (missing(credentials))
    credentials <- get_credential_config()
  moduleServer(id, function(input, output, session) {
    
    # Required for shinyhelper to work.
    # shinyhelper::observe_helpers()
    
    # Create list of packages.
    observeEvent(uploaded_pkgs(), {
      req(input$select_pkg)
      
      updateSelectizeInput(
        inputId = "select_pkg",
        choices = c("-", dbSelect('SELECT name FROM package')$name),
        selected = "-"
      )
      
      shinyjs::enable("select_pkg")
      
    }, ignoreNULL = TRUE)
    
    # Get information about selected package.
    selected_pkg <- reactiveValues()
    
    observeEvent(req(input$select_pkg, session$userData$trigger_events$reset_sidebar), {
      pkg_selected <- get_pkg_info(input$select_pkg)

      pkg_selected %>%
        purrr::walk2(names(.), function(.x, .y) {selected_pkg[[.y]] <- .x})
    }, priority = 1)
    
    # Update package version.
    observeEvent(input$select_pkg, {
      req(input$select_pkg)
      req(input$select_ver)
      
      version <- ifelse(input$select_pkg == "-", "-", selected_pkg$version)
      date_added <- ifelse(input$select_pkg == "-", "-", selected_pkg$date_added)
      
      updateSelectizeInput(
        session,
        'select_ver',
        choices = version,
        selected = version
      )
      
      updateSelectizeInput(
        session,
        'select_date',
        choices = date_added,
        selected = date_added
      )
      
      session$userData$trigger_events$update_report_pref_inclusions <- session$userData$trigger_events$update_report_pref_inclusions + 1
      
    })
    
    output$repo_url <- renderText({
      req(input$select_pkg)
      req(input$select_pkg != "-")

      selected_pkg$url
    })
    
    # Display the review status of the selected package.
    output$status <- renderUI({
      req(input$select_pkg)
      req(input$select_ver)
      
      if(input$select_pkg == "-")
        validate("Please select a package")
      if(input$select_ver == "-")
        validate("Please select a version")
      
      status <- ifelse(is.na(selected_pkg$decision), "Under Review", "Reviewed")
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
      
      req(selected_pkg$score)
      
      h5(selected_pkg$score)
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
        comments <- dbSelect(
          "SELECT comment FROM comments
          WHERE id = {input$select_pkg}
          AND comment_type = 'o'")$comment
        
        updateTextAreaInput(session, "overall_comment",
                            placeholder = glue::glue('Current Overall Comment: {comments}'))
      }
    })
    
    # Update db if comment is submitted.
    observeEvent(input$submit_overall_comment, {
      req("overall_comment" %in% unlist(credentials$privileges[user$role], use.name = FALSE))
      
      current_comment <- trimws(input$overall_comment)
      
      if(current_comment == "")
        validate("Please enter a comment.")
      
      req(selected_pkg$name)
      
      previous_comments <- get_overall_comments(selected_pkg$name)
        
      
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
      } else {
        dbUpdate(
          "INSERT INTO comments
          VALUES ({selected_pkg$name}, {user$name}, {paste(user$role, collapse = ', ')},
          {current_comment}, 'o', {getTimeStamp()})")
        
        updateTextAreaInput(session, "overall_comment", value = "",
                            placeholder = glue::glue('Current Comment: {current_comment}'))
        
        showModal(modalDialog(
          title = h2("Overall Comment Submitted"),
          br(),
          h5(strong("Current Comment:")),
          h5(current_comment),
          easyClose = TRUE
        ))
      }
    })
    
    observeEvent(input$submit_overall_comment_yes, {

      req(selected_pkg$name)
      req("overall_comment" %in% unlist(credentials$privileges[user$role], use.name = FALSE))

      dbUpdate(
          "UPDATE comments
          SET comment = {input$overall_comment}, added_on = {getTimeStamp()}
          WHERE id = {selected_pkg$name} AND
          user_name = {user$name} AND
          user_role = {paste(user$role, collapse = ', ')} AND
          comment_type = 'o'"
      )
      current_comment <- trimws(input$overall_comment)
      updateTextAreaInput(session, "overall_comment", value = "",
                          placeholder = glue::glue('Current Comment: {current_comment}'))
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
    observeEvent(req(input$select_ver), {
      req(input$select_pkg)
      req(input$select_ver)
      
      # Reset decision if no package/version is selected.
      if(input$select_pkg == "-" || input$select_ver == "-" || is.na(selected_pkg$decision)) {
        shinyWidgets::updateSliderTextInput(
          session,
          "decision",
          choices = golem::get_golem_options("decision_categories"),
          selected = golem::get_golem_options("decision_categories")[1]
        )
        
        validate('Please select a package and a version.')
      }
      
      req(selected_pkg$decision)
      
      # Update the risk slider using the info saved.
      shinyWidgets::updateSliderTextInput(
        session,
        "decision",
        choices = golem::get_golem_options("decision_categories"),
        selected = selected_pkg$decision
      )
    })
    
    # Enable/disable sidebar decision and comment.
    observeEvent(req(input$select_ver, session$userData$trigger_events$reset_sidebar), {
      if (input$select_pkg != "-" && input$select_ver != "-" &&
          (rlang::is_empty(selected_pkg$decision) || is.na(selected_pkg$decision)) &&
          "overall_comment" %in% unlist(credentials$privileges[user$role], use.name = FALSE)) {
        shinyjs::enable("overall_comment")
        shinyjs::enable("submit_overall_comment")
        
      } else{
        shinyjs::disable("overall_comment")
        shinyjs::disable("submit_overall_comment")
      }
    }, ignoreInit = TRUE)
    
    observeEvent(req(input$select_ver, session$userData$trigger_events$reset_sidebar), {
      req("final_decision" %in% unlist(credentials$privileges[user$role], use.name = FALSE))

      if (input$select_pkg != "-" && input$select_ver != "-" &&
          (rlang::is_empty(selected_pkg$decision) || is.na(selected_pkg$decision))) {
        shinyjs::enable("decision")
        shinyjs::enable("submit_decision")

      } else{
        shinyjs::disable("decision")
        shinyjs::disable("submit_decision")
      }
    }, ignoreInit = TRUE)
    
    # Show reset final decision link if user is admin the a final decision has been made.
    observe({

      if (!(input$select_pkg == "-" && input$select_ver == "-" ||
          (rlang::is_empty(selected_pkg$decision) || is.na(selected_pkg$decision))) &&
          "revert_decision" %in% unlist(credentials$privileges[user$role], use.name = FALSE)) {
        shinyjs::hide("submit_decision")
      } else {
        shinyjs::show("submit_decision")
      }
    }) %>%
      bindEvent(selected_pkg$decision, session$userData$trigger_events$reset_sidebar,
                ignoreInit = TRUE)
    
    output$reset_decision_ui <- renderUI({
      req(user$role)
      req(credentials$privileges)
      req("revert_decision" %in% unlist(credentials$privileges[user$role], use.name = FALSE))
      req(!(input$select_pkg == "-" && input$select_ver == "-" ||
            (rlang::is_empty(selected_pkg$decision) || is.na(selected_pkg$decision))))
      
      actionButton(NS(id, "reset_decision"), "Reset Decision", width = "100%")
    })
    
    # Show a confirmation modal when submitting a decision.
    observeEvent(input$submit_decision, {
      req(input$decision)
      req("final_decision" %in% unlist(credentials$privileges[user$role], use.name = FALSE))
      
      showModal(modalDialog(
        size = "l",
        easyClose = TRUE,
        h5("Submit Decision", style = 'text-align: center !important'),
        hr(),
        br(),
        fluidRow(
          column(
            width = 12,
            'Please confirm your chosen risk: ', span(class = 'text-info', input$decision),
            br(),
            br(),
            em('Note: Once submitted, this final risk can only be reverted by an administrator.')
          )
        ),
        br(),
        footer = tagList(
          actionButton(NS(id, 'submit_confirmed_decision'), 'Submit'),
          actionButton(NS(id, 'cancel'), 'Cancel')
        )))
    })
    
    # Show a confirmation modal when resetting a decision
    observeEvent(input$reset_decision, {
      req(input$decision)
      req("revert_decision" %in% unlist(credentials$privileges[user$role], use.name = FALSE))
      
      showModal(modalDialog(
        size = "l",
        easyClose = TRUE,
        h5("Reset Decision", style = 'text-align: center !important'),
        hr(),
        br(),
        fluidRow(
          column(
            width = 12,
            'Please confirm to reset the risk decision: ', span(class = 'text-info', input$decision),
          )
        ),
        br(),
        footer = tagList(
          actionButton(NS(id, 'reset_confirmed_decision'), 'Reset'),
          actionButton(NS(id, 'cancel'), 'Cancel')
        )))
    })
    
    # Close modal if user cancels decision submission.
    observeEvent(input$cancel, {
      removeModal()
    })
    
    # Update database info after decision is submitted.
    observeEvent(input$submit_confirmed_decision, {
      req("final_decision" %in% unlist(credentials$privileges[user$role], use.name = FALSE))
      
      dbUpdate("UPDATE package
          SET decision_id = {match(input$decision, golem::get_golem_options(\"decision_categories\"))}, decision_by = {user$name}, decision_date = {get_Date()}
          WHERE name = {selected_pkg$name}"
      )
      
      selected_pkg$decision <- input$decision
      
      shinyjs::disable("decision")
      shinyjs::disable("submit_decision")
      shinyjs::disable("overall_comment")
      shinyjs::disable("submit_overall_comment")
      
      removeModal()
      
      loggit::loggit("INFO",
                     glue::glue("decision for the package {selected_pkg$name} is {input$decision}
                  by {user$name} ({paste(user$role, collapse = ', ')})"))
    })
    
    observeEvent(input$reset_confirmed_decision, {
      req("revert_decision" %in% unlist(credentials$privileges[user$role], use.name = FALSE))
      
      dbUpdate("UPDATE package
          SET decision_id = NULL, decision_by = '', decision_date = NULL
          WHERE name = {selected_pkg$name}"
      )
      # remove overall comment for this package 
      dbUpdate("DELETE FROM comments 
           WHERE comment_type = 'o'
           AND id IN (SELECT {selected_pkg$name} FROM package)")
      
      selected_pkg$decision <- NA_character_
      
      shinyjs::enable("decision")
      shinyjs::enable("submit_decision")
      shinyjs::enable("overall_comment")
      shinyjs::enable("submit_overall_comment")
      
      removeModal()
      
      loggit::loggit("INFO",
                     glue::glue("decision for the package {selected_pkg$name} is reset
                  by {user$name} ({paste(user$role, collapse = ', ')})"))
    })
    
    # Output package id, name, and version.
    # TODO: return the entire selected_pkg instead of doing this below.
    list(
      id = reactive(selected_pkg$id),
      name = reactive(selected_pkg$name),
      version = reactive(selected_pkg$version),
      date_added = reactive(selected_pkg$date_added),
      repo_url = reactive(selected_pkg$url),
      title = reactive(selected_pkg$title),
      decision = reactive(selected_pkg$decision),
      description = reactive(selected_pkg$description),
      author = reactive(selected_pkg$author),
      maintainer = reactive(selected_pkg$maintainer),
      license = reactive(selected_pkg$license),
      published = reactive(selected_pkg$published_on),
      overall_comment_added = reactive(c(input$submit_overall_comment, input$submit_overall_comment_yes)),
      score = reactive(selected_pkg$score)
    )
  })
}
