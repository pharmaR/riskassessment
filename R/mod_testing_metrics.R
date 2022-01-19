
# No UI needed given current renderUI structure in server-side logic
# #' testing_metrics UI Function
# #'
# #' @description A shiny Module.
# #'
# #' @param id,input,output,session Internal parameters for {shiny}.
# #'
# #' @noRd 
# #'
# #' @importFrom shiny NS tagList 
# mod_testing_metrics_ui <- function(id){
#   ns <- NS(id)
#   tagList(
#  
#   )
# }
    
#' testing_metrics Server Functions
#'
#' @noRd 
mod_testing_metrics_server <- 
  function(input, output, session
    ){ # id removed & added: input, output, session!
    
  # moduleServer( id, function(input, output, session){
  #   ns <- session$ns
  # })
  
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
    
    # 1. Observe to load the columns from DB into below reactive values.
    
    observeEvent(input$tabs, {
      req(input$select_pack)
      if (input$tabs == "tm_tab_value") {
        if (input$select_pack != "Select") {
          
          package_id <- db_fun(paste0("SELECT id FROM package WHERE name = ", "'", input$select_pack, "'", ";"))
          metric_id <- db_fun(paste0("SELECT id FROM metric WHERE name = 'covr_coverage';"))
          
          values$covr_coverage <- db_fun(
            paste0("SELECT value FROM package_metrics WHERE ",
                   "package_id = ", package_id,
                   " AND ",
                   "metric_id = ", metric_id,
                   ";"
            )
          )
          values$covr_coverage <- values$covr_coverage$value
          
          if (!is.null(input$tm_comment)) {
            if(values$covr_coverage == "pkg_metric_error")
              runjs( "setTimeout(function(){ addTextToGaugeSVG('test_coverage');}, 500);" )
            
            req(values$selected_pkg$decision)
            
            if (values$selected_pkg$decision != "") {
              runjs("setTimeout(function(){ var ele = document.getElementById('tm_comment'); ele.disabled = true; }, 500);")
              runjs("setTimeout(function(){ var ele = document.getElementById('submit_tm_comment'); ele.disabled = true; }, 500);")
            }
          }
        }
      }
    })  # End of the observe.
    
    # End of the observe's'
    
    # Start of the render Output's'
    
    # 1. Render Output to show the test converage gauage.
    
    output$test_coverage <- renderAmCharts({
      bands = data.frame(
        start = c(0, 40, 80),
        end = c(40, 80, 100),
        color = ifelse(values$covr_coverage != "pkg_metric_error",
                       c("#ea3838", "#ffac29", "#00CC00"),
                       c("#808080", "#808080", "#808080")),
        stringsAsFactors = FALSE
      )
      bands2 = data.frame(
        start = c(0, 40, 80),
        end = c(40, 80, 100),
        color = ifelse(values$covr_coverage != "pkg_metric_error",
                       c("#ea3838", "#ffac29", "#00CC00"),
                       c("#808080", "#808080", "#808080")),
        stringsAsFactors = FALSE
      )
      amAngularGauge(
        x = as.numeric(ifelse(values$covr_coverage == "NA", 0, values$covr_coverage)),
        start = 0,
        end = 100,
        bands = bands,
        secondAxe = TRUE,
        start2 = 0,
        end2 = 100,
        bands2 = bands2
      )
    })  # End of the render Output.
    
    # 2. Render Output to show the comments for testing metrics on the application.
    
    output$tm_commented <- renderText({
      if (values$tm_comment_submitted == "yes" ||
          values$tm_comment_submitted == "no") {
        values$comment_tm1 <-
          db_fun(
            paste0(
              "SELECT user_name, user_role, comment, added_on FROM Comments WHERE comm_id = '",
              input$select_pack,
              "' AND comment_type = 'tm'"
            )
          )
        values$comment_tm2 <- data.frame(values$comment_tm1 %>% map(rev))
        req(values$comment_tm2$comment)
        values$tm_comment_submitted <- "no"
        paste(
          "<div class='col-sm-12 comment-border-bottom'><i class='fa fa-user-tie fa-4x'></i><h3 class='ml-3'><b class='user-name-color'>",
          values$comment_tm2$user_name,
          "(",
          values$comment_tm2$user_role,
          ")",
          "</b><sub>",
          values$comment_tm2$added_on,
          "</sub></h3><h4 class='ml-3 lh-4'>",
          values$comment_tm2$comment,
          "</h4></div>"
        )
      }
    })  # End of the render Output.
    
    # Observe event for submit button to submit the comments for testing metrics.
    
    values$tm_comment_submitted <- "no"
    observeEvent(input$submit_tm_comment, {
      if (trimws(input$tm_comment) != "") {
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
            input$tm_comment,
            "',",
            "'tm',",
            "'",
            TimeStamp(),
            "'" ,
            ")" 
          )
        )
        values$tm_comment_submitted <- "yes"
        updateTextAreaInput(session, "tm_comment", value = "")
        # After comment added to Comments table, update db dash
        values$db_pkg_overview <- update_db_dash()
      }
    })  # End of the observe event.
    
}
    
## To be copied in the UI
# mod_testing_metrics_ui("testing_metrics_ui_1")
    
## To be copied in the server
# mod_testing_metrics_server("testing_metrics_ui_1")
