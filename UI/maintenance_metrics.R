#####################################################################################################################
# maintenance_metrics.R - Maintenance Metrics to show the info box's to show the information and leave multiple comments 
# for users and display the comments by users.
# 
# Author: Aravind
# Created: 02/06/2020.
#####################################################################################################################

# Start of the Maintenance_Metrics Source file for UI Module.

# Render Output UI for Maintenance Metrics.

output$maintenance_metrics <- renderUI({
 Sys.sleep(0.1)
 if (!is.null(values$packsDB) &&
    !identical(values$packsDB, character(0))) {
  if (input$select_pack != "Select") {
    fluidRow(
      div(style = "height:25px;"),
      class = "mm-main-row",
      fluidRow(
        class = "mm-row-1",
        infoBoxOutput("vignette"),  # Info box to show the information on VIGNETTE Content.
        infoBoxOutput("website"),  # Info box to show the information on Package Has Website.
        infoBoxOutput("hasnews"),  # Info box to show the Package Has News? Content.
      ),
      fluidRow(
        class = "mm-row-2",
        infoBoxOutput("newscurrent"),  # Info box to show the information for News is Current?
        infoBoxOutput("bugtrack"),  # Info box to show the information for Does the package have Bug Report?
        infoBoxOutput("bugstatus"),  # Info box to show the information on Bugs Status.
      ),
      fluidRow(
        class = "mm-row-3",
        infoBoxOutput("exporthelp"),  # Info box to show the information on Export help.
        infoBoxOutput("source_pub"),  # Info box to show the information on source code is public?
        infoBoxOutput("pack_maint"),  # Info box to show the information on Has a package maintainer?
      ),
      fluidRow(
        class = "mm-row-comments-box",
        column(
          width = 8,
          class = "mb-4 label-float-left",
          # Text input box to leave the Maintenance Metrics Comments.
          textAreaInput(
            "mm_comment",
            h3(tags$b("Leave Your Comment for Maintenance Metrics:")),
            width = "100%",
            rows = 4,
            placeholder = paste("Commenting as", values$name, "(", values$role, ")")
          ) %>%
            shiny::tagAppendAttributes(style = 'width: 100%;'),
          # Action button to submit the comment.
          actionButton("submit_mm_comment", class = "submit_mm_comment_class btn-secondary", "Submit")
        )
      ),
      fluidRow(
        class = "mm-row-comments",
        column(
          width = 12,
          align = "left",
          h3(tags$b(paste0('Comments(',nrow(values$comment_mm2),'):'))),
          htmlOutput("mm_commented")  # html output to show the comments on applicaiton.
        )
      )
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
        h1("Please select the Package")
      )
    )
  }
 }
  # Show the upload a list of R packages message if application not loaded the pacakges from DB.

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

# End of the Maintenance_Metrics Source file for UI Module.
