#####################################################################################################################
# testing_metrics.R - Testing Metrics to show the test coverage graph and leave multiple comments 
#                     for users and display the comments by users.
# Author: K Aravind Reddy
# Date: July 13th, 2020
# License: MIT License
#####################################################################################################################

# Start of the Testing_Metrics Source file for UI Module.

# Render Output UI for Testing Metrics.
output$testing_metrics <- renderUI({
  Sys.sleep(0.1)
  if (!is.null(values$packsDB$name) &&
      !identical(values$packsDB$name, character(0))) {
    if (input$select_pack != "Select") {
      fluidRow(
        div(style = "height:25px;"),
        class = "t_m_main_row",
        fluidRow(
          h3("TEST COVERAGE(%)"),
          div(style = "height:25px;"),
          amChartsOutput(outputId = "test_coverage")  #  amchart to display the test coverage.
        ),
        div(style = "height:25px;"),
        fluidRow(
          class = "t_m_comments_box_row",
          column(
            width = 8,
            class = "mb-4 label-float-left",
            # Text area box to leave comment for user.
            textAreaInput(
              "tm_comment",
              h3(tags$b("Leave Your Comment for Testing Metrics:")),
              width = "100%",
              rows = 4,
              placeholder = paste("Commenting as", values$name, "(", values$role, ")")
            ) %>%
              shiny::tagAppendAttributes(style = 'width: 100%;'),
            # submit button to submit the comments.
            actionButton("submit_tm_comment", class = "submit_tm_comment_class btn-secondary", "Submit")
          )
        ),
        fluidRow(
          class = "t_m_comments_row",
          column(
            width = 12,
            align = "left",
            h3(tags$b(paste0('Comments(',nrow(values$comment_tm2),'):'))),
            htmlOutput("tm_commented", inline = TRUE)  # showing comments on application.
          ))
      )
      
    } 
    # Show the select the package message if user not selected any package from dropdown in the application. 
    
    else{
      fluidRow(
        div(style = "height:150px;"),
        class = "",
        id = "Upload_mm",
        column(
          width = 12,
          align = "center",
          class = "",
          h1("Please select a package")
        )
      )
    }
  }
  # Show the upload a list of R packages message if application not loaded the packages from DB.
  
  else{
    fluidRow(
      div(style = "height:150px;"),
      class = "",
      id = "Upload",
      column(
        width = 12,
        align = "center",
        class = "",
        h1("Please upload a list of R packages to proceed")
      )
    )
  }
})

# End of the Testing_Metrics Source file for UI Module.
