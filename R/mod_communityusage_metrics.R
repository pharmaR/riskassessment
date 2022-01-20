
# No UI needed given current renderUI structure in server-side logic
# #' communityusage_metrics UI Function
# #'
# #' @description A shiny Module.
# #'
# #' @param id,input,output,session Internal parameters for {shiny}.
# #'
# #' @noRd 
# #'
# #' @importFrom shiny NS tagList 
# mod_communityusage_metrics_ui <- function(id){
#   ns <- NS(id)
#   tagList(
#  
#   )
# }
    
#' communityusage_metrics Server Functions
#' 
#' @import shiny
#'
#' @noRd 
mod_communityusage_metrics_server <- 
  function(input, output, session
  ){ # id removed & added: input, output, session!
    
  # moduleServer( id, function(input, output, session){
  #   ns <- session$ns
  # })
    # Render Output UI for Community Usage Metrics.
    
    output$community_usage_metrics <- renderUI({
      Sys.sleep(0.1)
      if (!is.null(values$packsDB$name) &&
          !identical(values$packsDB$name, character(0))) {
        if (input$select_pack != "Select") {
          shiny::tagList(
            br(),
            div(class = "row col-sm-12 u_p_heading_row",
                actionBttn("help_cum", "Need help?", color = "primary",
                           icon = icon("far fa-star"),
                           block = FALSE, style = "simple", size = "sm")),
            br(), br(),
            fluidRow(
              div(style = "height:25px;"),
              class = "c_u_m_row_main",
              fluidRow(
                id = "cum_infoboxes",
                class = "c_u_m_row_1",
                infoBoxOutput("time_since_first_release", width = 4),  # Info box to show the time since First release.
                infoBoxOutput("time_since_version_release", width = 4),  # Info box to show the time since version release.
                infoBoxOutput("dwnlds_last_yr", width = 4)  # Info box to show the total # of Downloads in the last year.
              ),
              fluidRow(
                id = "cum_plot",
                class = "c_u_m_row_graph",
                column(width = 1, ),
                column(width = 10,
                       class = "w-90",
                       plotly::plotlyOutput("no_of_downloads")
                ),
                column(width = 1, )
              ),
              
              fluidRow(
                id = "cum_add_comment",
                class = "c_u_m_row_comments_box",
                column(
                  
                  width = 8,
                  class = "mb-4 label-float-left",
                  # Text box to leave community usage metrics comments.
                  textAreaInput(
                    "cum_comment",
                    h3(tags$b("Leave Your Comment for Community Usage Metrics:")),
                    width = "100%",
                    rows = 4,
                    placeholder = paste("Commenting as", values$name, "(", values$role, ")")
                  ) %>%
                    shiny::tagAppendAttributes(style = 'width: 100%;'),
                  # Action button to submit the comments.
                  actionButton("submit_cum_comment", class = "submit_cum_comment_class btn-secondary", "Submit")
                )
              ),
              fluidRow(
                id = "cum_prev_comments",
                class = "c_u_m_row_comments",
                column(
                  width = 12,
                  align = "left",
                  h3(tags$b(paste0('Comments(',nrow(values$comment_cum2),'):'))),
                  htmlOutput("cum_commented")  # html output to show the comments on applicaiton.
                ))
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
    
    
    # Implement the intro logic. Sidebar steps are listed in global.r
    # this dataset is also static... perhaps it should be sourced from global.r?
    cum_steps <- reactive(
      data.frame(
        # Note that we access chooseCSVtext with '.' instead of '#', because we track its class and not its id.
        element = c("#cum_infoboxes", "#cum_plot", "#cum_add_comment", "#cum_prev_comments"),
        intro = c(
          "Several ways of measuring community usage assessed here. Please review!",
          "Digest downloads per month by selecting a pre-defined time periods or toggling the date slider at bottom of plot for custom date range",
          "Have something to share within your organization? Add a comment.",
          "Keep track of the on-going conversation for this package's community usage"
        ),
        position = c("bottom", rep("top", 3))
      )
    )
    
    
    # Start introjs when help button is pressed.
    observeEvent(input$help_cum,
                 introjs(session,
                         options = list(
                           steps = 
                             cum_steps() %>%
                             union(sidebar_steps),
                           "nextLabel" = "Next",
                           "prevLabel" = "Previous",
                           "skipLabel" = "Close"
                         )
                 )
    )
    
    
    # Start of the observe.
    
    # 1. Observe to load the columns from DB into reactive values.
    observe({
      req(input$select_pack)
      if (input$tabs == "cum_tab_value") {
        if (input$select_pack != "Select") {
          
          # Load the columns into values$riskmetrics.
          pkgs_in_db <- db_fun(paste0("SELECT cum_id FROM CommunityUsageMetrics"))
          
          if (input$select_pack %in% pkgs_in_db$cum_id &&
              !identical(pkgs_in_db$cum_id, character(0))) {
            values$riskmetrics_cum <-
              db_fun(
                paste0(
                  "SELECT * FROM CommunityUsageMetrics WHERE cum_id ='",
                  input$select_pack,
                  "'"
                )
              )
          } else{
            if (input$select_pack != "Select") {
              metric_cum_Info_upload_to_DB(input$select_pack)
              values$riskmetrics_cum <-
                db_fun(
                  paste0(
                    "SELECT * FROM CommunityUsageMetrics WHERE cum_id ='",
                    input$select_pack,
                    "'"
                  )
                )
            }
          }
          
          # Load the data table column into reactive variable for time since first release.
          values$time_since_first_release_info <-
            values$riskmetrics_cum$time_since_first_release[1]
          
          # Load the data table column into reactive variable for time since version release.
          values$time_since_version_release_info <-
            values$riskmetrics_cum$time_since_version_release[1]
          
          # Load the data table column into reactive variable for num dwnlds in year
          values$no_of_downloads_last_year_info <-
            values$riskmetrics_cum$no_of_downloads_last_year[1]
          
          runjs( "setTimeout(function(){ capturingSizeOfInfoBoxes(); }, 100);" )
          
          if (!is.null(input$cum_comment)) {
            if(values$time_since_version_release_info == "NA"){ runjs( "setTimeout(function(){ updateInfoBoxesColorWhenNA('time_since_version_release');}, 500);" ) }
            if(values$time_since_first_release_info == "NA"){ runjs( "setTimeout(function(){ updateInfoBoxesColorWhenNA('time_since_first_release');}, 500);" ) }
            if (values$riskmetrics_cum$no_of_downloads_last_year[1] == 0) { runjs("setTimeout(function(){ updateText('no_of_downloads');}, 500);") }
            req(values$selected_pkg$decision)
            if (values$selected_pkg$decision != "") {
              runjs("setTimeout(function(){ var ele = document.getElementById('cum_comment'); ele.disabled = true; }, 500);" )
              runjs("setTimeout(function(){ var ele = document.getElementById('submit_cum_comment'); ele.disabled = true; }, 500);")
            }
          }
        }
      }
    })  # End of the observe.
    
    # End of the observes.
    
    # Start of the render Output's'
    
    # 1. Render Output info box to show the content for time since first release.
    
    output$time_since_first_release <- renderInfoBox({
      req(values$time_since_first_release_info)
      shinydashboard::infoBox(
        title = "Package Maturity",
        values$time_since_first_release_info,
        subtitle = ifelse(values$time_since_first_release_info != "NA",
                          "Months since first release.",
                          "Metric is not applicable for this source of package."),
        icon = shiny::icon("calendar"),
        width = 3,
        fill = TRUE
      )
    })  # End of the time since first release render Output.
    
    # 2. Render Output info box to show the content for time since version release.
    
    output$time_since_version_release <- renderInfoBox({
      req(values$time_since_version_release_info)
      shinydashboard::infoBox(
        title = "Version Maturity",
        values$time_since_version_release_info,
        subtitle = ifelse(values$time_since_version_release_info != "NA", 
                          "Months since version release.",
                          "Metric is not applicable for this source of package."),
        icon = shiny::icon("calendar"),
        width = 3,
        fill = TRUE
      )
      
    })  # End of the time since version release render Output.
    
    
    
    # 2.5 Render Output info box to show the number of downloads last year
    
    output$dwnlds_last_yr <- renderInfoBox({
      req(values$no_of_downloads_last_year_info)
      shinydashboard::infoBox(
        title = "Download Count",
        formatC(values$no_of_downloads_last_year_info, format="f", big.mark=",", digits=0),
        subtitle = ifelse(values$no_of_downloads_last_year_info != "NA",
                          "Downloads in Last Year",
                          "Metric is not applicable for this source of package."),
        icon = shiny::icon("signal"),
        width = 3,
        fill = TRUE
      )
      
    })  # End 
    
    
    # 3. Render Output to show the plot for number of downloads on the application.
    output$no_of_downloads <- 
      plotly::renderPlotly({
        num_dwnlds_plot(data = values$riskmetrics_cum,
                        input_select_pack = input$select_pack)
      })
    
    
    
    # 4. Render output to show the comments.
    
    output$cum_commented <- renderText({
      if (values$cum_comment_submitted == "yes" ||
          values$cum_comment_submitted == "no") {
        values$comment_cum1 <-
          db_fun(
            paste0(
              "SELECT user_name, user_role, comment, added_on  FROM Comments WHERE comm_id = '",
              input$select_pack,
              "' AND comment_type = 'cum'"
            )
          )
        values$comment_cum2 <- data.frame(values$comment_cum1 %>% map(rev))
        req(values$comment_cum2$comment)
        values$cum_comment_submitted <- "no"
        paste(
          "<div class='col-sm-12 comment-border-bottom'><i class='fa fa-user-tie fa-4x'></i><h3 class='ml-3'><b class='user-name-color'>",
          values$comment_cum2$user_name,
          "(",
          values$comment_cum2$user_role,
          ")",
          "</b><sub>",
          values$comment_cum2$added_on,
          "</sub></h3><h4 class='ml-3 lh-4'>",
          values$comment_cum2$comment,
          "</h4></div>"
        )
      }
    })  # End of the render output for comments.
    
    # End of the Render Output's'.
    
    values$cum_comment_submitted <- "no"
    
    # Start of the Observe Events.
    
    # Observe event for cum comment submit button. 
    
    observeEvent(input$submit_cum_comment, {
      if (trimws(input$cum_comment) != "") {
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
            input$cum_comment,
            "',",
            "'cum',",
            "'",
            TimeStamp(),
            "'"  ,
            ")" 
          )
        )
        values$cum_comment_submitted <- "yes"
        updateTextAreaInput(session, "cum_comment", value = "")
        # After comment added to Comments table, update db dash
        values$db_pkg_overview <- update_db_dash()
      }
    })  # End of the submit button observe event.
    
  
}
    
## To be copied in the UI
# mod_communityusage_metrics_ui("communityusage_metrics_ui_1")
    
## To be copied in the server
# mod_communityusage_metrics_server("communityusage_metrics_ui_1")
