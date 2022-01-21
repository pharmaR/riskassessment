
# No UI needed given current renderUI structure in server-side logic
# #' reportpreview UI Function
# #'
# #' @description A shiny Module.
# #'
# #' @param id,input,output,session Internal parameters for {shiny}.
# #'
# #' @noRd 
# #'
# #' @importFrom shiny NS tagList 
# mod_reportpreview_ui <- function(id){
#   ns <- NS(id)
#   tagList(
#  
#   )
# }
    
#' reportpreview Server Functions
#' 
#' @import shiny
#'
#' @noRd 
mod_reportpreview_server <- 
  function(input, output, session = getDefaultReactiveDomain(),
           values
  ){ # id removed & added: input, output, session!
    
  # moduleServer( id, function(input, output, session){
  #   ns <- session$ns
  # })
    
    output$report_preview<-renderUI({
      Sys.sleep(0.1)
      if (!is.null(values$packsDB$name) &&
          !identical(values$packsDB$name, character(0))) {
        if (input$select_pack != "Select") {
          removeUI(selector = "#Upload")
          
          shiny::tagList(
            br(),
            div(class = "row col-sm-12 u_p_heading_row",
                shinyWidgets::actionBttn("help_rp", "Need help?", color = "primary",
                           icon = icon("far fa-star"),
                           block = FALSE, style = "simple", size = "sm")),
            br(), br(),
            fluidRow(
              class = "mt-4 r_p_main_row",
              
              fluidRow(
                id = "dwnld_rp",
                class="float-right r_p_format_row",
                tags$div(
                  class="col-sm W-40 text-left float-right",
                  selectInput("report_format", "Select Format", c("html", "docx")),  # Select input to select the format for report.
                ),
                tags$div(
                  class="col-sm float-right",
                  downloadButton("download_report_btn", "Download Report", class = "download_report_btn_class btn-secondary"),  # Download button to export the report.
                ),
              ),
              fluidRow(
                id = "rep_prev",
                column(
                  width = 12,
                  class = "text-left",
                  htmlOutput("gen_info"),  # Display General Information of the selected Package.
                  htmlOutput("decision_display"),  # Display the status of the Decision of a selected Package.
                  h3(tags$b(paste0('Overall Comments(',nrow(values$comment_o2),'):'))),
                  fluidRow(
                    class = "overall-comments-row",
                    column(
                      width = 12,
                      align = "left",
                      htmlOutput("overall_comments")  # Display the overall comment for selected Package. 
                    )
                  ),
                  source(file.path("UI", "mm_report.R"), local = TRUE)$value,
                  source(file.path("UI", "cum_report.R"), local = TRUE)$value,
                  # source(file.path("UI", "tm_report.R"), local = TRUE)$value
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
            id = "Upload",
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
    })  # End of the Render Output
    
    
    # Implement the intro logic. Sidebar steps are listed in global.r
    # this dataset is also static... perhaps it should be sourced from global.r?
    rp_steps <- reactive(
      data.frame(
        # Note that we access chooseCSVtext with '.' instead of '#', because we track its class and not its id.
        element = c( "#dwnld_rp", "#rep_prev"),
        intro = c(
          "Select file output type for report seen below and download for later use",
          "The current assessment of this package including your comments and overall decision have been collected from the other tabs to prepare the following report for convenience."
        ),
        position = c("left", "top")
      )
    )
    
    
    # Start introjs when help button is pressed.
    observeEvent(input$help_rp,
                 rintrojs::introjs(session,
                         options = list(
                           steps = 
                             rp_steps() %>%
                             union(sidebar_steps),
                           "nextLabel" = "Next",
                           "prevLabel" = "Previous")))
    
    
    # Display general information of the selected package.
    output$gen_info <- renderText({
      pkg_GenInfo <-
        db_fun(
          paste0(
            "SELECT * FROM package WHERE name ='",
            input$select_pack,
            "'"
          )
        )
      
      paste(
        "<h2><b>Package:</b> ",
        pkg_GenInfo$name,
        "</h2>",
        "<h4><b>Version: </b>",
        pkg_GenInfo$version,
        "</h4>",
        "<h4><b>Title: </b>",
        pkg_GenInfo$title,
        "</h4>",
        "<h4><b>Description:</b>",
        pkg_GenInfo$description,
        "</h4>",
        "<h4><b>Author:</b>",
        pkg_GenInfo$author,
        "</h4>",
        "<h4><b>Maintainer: </b>",
        pkg_GenInfo$maintainer,
        "<h4><b>License: </b>",
        pkg_GenInfo$license,
        "</h4>",
        "<h4><b>Published:</b>",
        pkg_GenInfo$published,
        "</h4>"
      )
    })
    
    
    # Display the decision status of the selected pacakge.
    output$decision_display <- renderText({
      if (!identical(values$selected_pkg$decision, character(0)) && values$selected_pkg$decision != "") {
        paste("<br>", "<h3>Overall risk: ", "<b>", values$selected_pkg$decision, "</b></h3>") 
      } else{
        paste("<br>", "<h3>Overall risk: Pending</h3>")
      }
    })    # End of the render Text Output.
    
    # Display the overall comment of the selected package. 
    output$overall_comments <- renderText({
      req(values$selected_pkg$name)
      if (values$o_comment_submitted == "yes" ||
          values$o_comment_submitted == "no") {
        values$comment_o1 <-
          db_fun(
            paste0(
              "SELECT * FROM Comments WHERE comm_id = '",
              values$selected_pkg$name,
              "' AND comment_type = 'o'"
            )
          )
        values$comment_o2 <- values$comment_o1 %>% arrange(desc(values$comment_o1$added_on))
        req(values$comment_o2$comment)
        values$o_comment_submitted <- "no"
        paste(
          "<div class='col-sm-12 comment-border-bottom single-comment-div'><i class='fa fa-user-tie fa-4x'></i><h3 class='ml-3'><b class='user-name-color'>",
          values$comment_o2$user_name,
          "(",
          values$comment_o2$user_role,
          ")",
          "</b><sub>",
          values$comment_o2$added_on,
          "</sub></h3><h4 class='ml-3 lh-4'>",
          values$comment_o2$comment,
          "</h4></div>"
        )
      }
    })
    
    # Create report.
    values$cwd <- getwd()
    output$download_report_btn <- downloadHandler(
      filename = function() {
        paste0(input$select_pack, "_", input$select_ver, "_Risk_Assessment.",
               switch(input$report_format, "docx" = "docx", "html" = "html"))
      },
      content = function(file) {
        shiny::withProgress(
          message = paste0("Downloading ", input$dataset, " Report"),
          value = 0,
          {
            shiny::incProgress(1 / 10)
            shiny::incProgress(5 / 10)
            if (input$report_format == "html") {
              Report <- file.path(tempdir(), "Report_html.Rmd")
              file.copy("inst/app/www/Reports/Report_html.Rmd", Report, overwrite = TRUE)
            } else {
              Report <- file.path(tempdir(), "Report_doc.Rmd")
              file.copy("inst/app/www/Reports/Report_doc.Rmd", Report, overwrite = TRUE)
            }
            
            rmarkdown::render(
              Report,
              output_file = file,
              params = list(package = values$selected_pkg$name,
                            riskmetric_version = packageVersion("riskmetric"),
                            cwd = values$cwd,
                            username = values$name,
                            user_role = values$role)
            )
          })
      }
    )
    
    # source(file.path("Server", "mm_report.R"), local = TRUE)$value
    # source(file.path("Server", "cum_report.R"), local = TRUE)$value
    
    return(values)
}
    
## To be copied in the UI
# mod_reportpreview_ui("reportpreview_ui_1")
    
## To be copied in the server
# mod_reportpreview_server("reportpreview_ui_1")
