# #' maintenance_metrics UI Function
# #'
# #' @description A shiny Module.
# #'
# #' @param id,input,output,session Internal parameters for {shiny}.
# #'
# #' @noRd 
# #'
# #' @importFrom shiny NS tagList 
# mod_maintenance_metrics_ui <- function(id){
#   ns <- NS(id)
#   tagList(
#  
#   )
# }

    
#' maintenance_metrics Server Functions
#'
#' @noRd 
mod_maintenance_metrics_server <- 
  function(input, output, session = getDefaultReactiveDomain(),
           values
     ){ # id removed & added: input, output, session!
  # moduleServer( id, function(input, output, session){
  #   ns <- session$ns
  # })
  # Render Output UI for Maintenance Metrics.
  
  output$maintenance_metrics <- renderUI({
    Sys.sleep(0.1)
    if (!is.null(values$packsDB$name) &&
        !identical(values$packsDB$name, character(0))) {
      if (input$select_pack != "Select") {
        shiny::tagList(
          br(),
          div(class = "row col-sm-12 u_p_heading_row",
              shinyWidgets::actionBttn("help_mm", "Need help?", color = "primary",
                         icon = icon("far fa-star"),
                         block = FALSE, style = "simple", size = "sm")),
          br(), br(),
          fluidRow(
            div(style = "height:25px;"),
            class = "mm-main-row",
            div(id = "mm_infoboxes", 
                fluidRow(
                  class = "mm-row-1",
                  infoBoxOutput("has_vignettes"),  # Info box for 'has_vignettes' metric.
                  infoBoxOutput("has_website"),  # Info box for 'has_website' metric.
                  infoBoxOutput("has_news"),  # Info box for 'has_news' metric.
                ),
                fluidRow(
                  class = "mm-row-2",
                  infoBoxOutput("news_current"),  # Info box for 'news_current' metric.
                  infoBoxOutput("has_bug_reports_url"),  # Info box for 'has_bug_reports_url' metric.
                  infoBoxOutput("bugs_status"),  # Info box for 'bugs_status' metric.
                ),
                fluidRow(
                  class = "mm-row-3",
                  infoBoxOutput("export_help"),  # Info box for 'export_help' metric.
                  infoBoxOutput("has_source_control"),  # Info box for 'has_source_control' metric.
                  infoBoxOutput("has_maintainer"),  # Info box for 'has_maintainer' metric.
                )
            ),
            fluidRow(
              id = "mm_add_comment",
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
              id = "mm_prev_comments",
              class = "mm-row-comments",
              column(
                width = 12,
                align = "left",
                h3(tags$b(paste0('Comments(',nrow(values$comment_mm2),'):'))),
                htmlOutput("mm_commented")  # html output to show the comments on application.
              )
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
  
  # Implement the intro logic. Sidebar steps are listed in global.r
  # this dataset is also static... perhaps it should be sourced from global.r?
  mm_steps <- reactive(
    data.frame(
      # Note that we access chooseCSVtext with '.' instead of '#', because we track its class and not its id.
      element = c("#mm_infoboxes", "#mm_add_comment", "#mm_prev_comments"),
      intro = c(
        "Several ways of measuring package maintenance best practices are assessed here. Please review!",
        "Have something to share within your organization? Add a comment.",
        "Keep track of the on-going conversation for this package's maintainence metrics"
      ),
      position = c(rep("top", 3))
    )
  )
  
  
  # Start introjs when help button is pressed.
  observeEvent(input$help_mm,
               rintrojs::introjs(session,
                       options = list(
                         steps = 
                           mm_steps() %>%
                           union(sidebar_steps),
                         "nextLabel" = "Next",
                         "prevLabel" = "Previous",
                         "skipLabel" = "Close"
                       )
               )
  )
  
  # Save each metric information into variables.
  observe({
    req(input$select_pack)
    if(input$tabs == "mm_tab_value"){
      if(input$select_pack != "Select"){
        
        package_id <- db_fun(paste0("SELECT id
                                  FROM package
                                  WHERE name = ", "'", input$select_pack, "';"))
        
        # Leave method if package not found.
        # TODO: save this to the json file.
        if(nrow(package_id) == 0){
          print("PACKAGE NOT FOUND.")
          return()
        }
        
        # Collect all the metric names and values associated to package_id.
        values$riskmetrics_mm <- db_fun(paste0(
          "SELECT metric.name, package_metrics.value
        FROM metric
        INNER JOIN package_metrics ON metric.id = package_metrics.metric_id
        WHERE package_metrics.package_id = ", "'", package_id, "'", " AND ",
        "metric.class = 'maintenance' ;"))
        
        runjs("setTimeout(function(){ capturingSizeOfInfoBoxes(); }, 500);")
        
        for(i in 1:nrow(values$riskmetrics_mm))
          values[[values$riskmetrics_mm$name[i]]] <- values$riskmetrics_mm$value[i]
        
        if (values$selected_pkg$decision != "") {
          runjs("setTimeout(function(){disableUI('mm_comment')}, 500);")
          runjs("setTimeout(function(){disableUI('submit_mm_comment')}, 500);")
        }
      }
    }
  })
  
  # Render infobox for has_vignettes metric.
  output$has_vignettes <- renderInfoBox({
    has_vignettes_infobox(values)
  })
  
  # Render infobox for has_website metric.
  output$has_website <- renderInfoBox({
    has_website_infobox(values)
  })
  
  # Render infobox for has_news metric.
  output$has_news <- renderInfoBox({
    has_news_infobox(values)
  })
  
  # Render infobox for news_current metric.
  output$news_current <- renderInfoBox({
    news_current_infobox(values)
  })
  
  # Render infobox for has_bug_reports_url metric.
  output$has_bug_reports_url <- renderInfoBox({
    has_bug_reports_url_infobox(values)
  })
  
  # Render infobox for bugs_status metric.
  output$bugs_status <- renderInfoBox({
    bugs_status_infobox(values)
  })
  
  # Render infobox for export_help metric.
  output$export_help <- renderInfoBox({
    export_help_infobox(values)
  })
  
  # Render infobox for has_source_control metric.
  output$has_source_control <- renderInfoBox({
    has_source_control_infobox(values)
  })
  
  # Render infobox for has_maintainer metric.
  output$has_maintainer <- renderInfoBox({
    has_maintainer_infobox(values)
  })
  
  # Show the comments on the package.
  output$mm_commented <- renderText({
    if (values$mm_comment_submitted == "yes" ||
        values$mm_comment_submitted == "no") {
      values$comment_mm1 <-
        db_fun(
          paste0(
            "SELECT user_name, user_role, comment, added_on  FROM Comments WHERE comm_id = '",
            input$select_pack,
            "' AND comment_type = 'mm'"
          )
        )
      values$comment_mm2 <- data.frame(values$comment_mm1 %>% purrr::map(rev))
      req(values$comment_mm2$comment)
      values$mm_comment_submitted <- "no"
      paste(
        "<div class='col-sm-12 comment-border-bottom single-comment-div'><i class='fa fa-user-tie fa-4x'></i><h3 class='ml-3'><b class='user-name-color'>",
        values$comment_mm2$user_name,
        "(",
        values$comment_mm2$user_role,
        ")",
        "</b><sub>",
        values$comment_mm2$added_on,
        "</sub></h3><h4 class='ml-3 lh-4'>",
        values$comment_mm2$comment,
        "</h4></div>"
      )
    }
  })  # End of the render Output.
  
  # End of the Render Output's'.
  
  values$mm_comment_submitted <- "no"
  
  # Observe event for submit button.
  
  observeEvent(input$submit_mm_comment, {
    if (trimws(input$mm_comment) != "") {
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
          input$mm_comment,
          "',",
          "'mm'," ,
          "'",
          TimeStamp(),
          "'"  ,
          ")"
        )
      )
      values$mm_comment_submitted <- "yes"
      updateTextAreaInput(session, "mm_comment", value = "")
      # After comment added to Comments table, update db dash
      values$db_pkg_overview <- update_db_dash()
    }
  })  # End of the Observe Event.
  
}
    
## To be copied in the UI
# mod_maintenance_metrics_ui("maintenance_metrics_ui_1")
    
## To be copied in the server
# mod_maintenance_metrics_server("maintenance_metrics_ui_1")
