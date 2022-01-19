
# No UI needed given current renderUI structure in server-side logic
# #' uploadpackage UI Function
# #'
# #' @description A shiny Module.
# #'
# #' @param id,input,output,session Internal parameters for {shiny}.
# #'
# #' @noRd 
# #'
# #' @importFrom shiny NS tagList 
# mod_uploadpackage_ui <- function(id){
#   ns <- NS(id)
#   tagList(
#  
#   )
# }
    
#' uploadpackage Server Functions
#' 
#' @import shiny
#'
#' @noRd 
mod_uploadpackage_server <- 
  function(input, output, session
    ){ # id removed & added: input, output, session!
  # moduleServer( function(input, output, session){ # id, 
  #   ns <- session$ns
  # })
  
  # Render Output UI for upload package.
  output$upload_package <- renderUI({
    fluidRow(
      style = "padding-left: 30px; padding-right: 30px; padding-bottom: 50px",
      class = "u_p_main_row",
      tags$div(class = "row col-sm-12 u_p_heading_row",
               tags$h2("Upload list of R Packages"),
               actionBttn("help", "Need help?", color = "primary",
                          icon = icon("far fa-star"),
                          block = FALSE, style = "simple", size = "sm")
      ),
      fluidRow(
        style = "text-align: left;",
        column(
          width = 4,
          tags$h4("Choose a CSV File", class = "chooseCSVtext"),
          # file input to choose the csv from local to application.
          fileInput(
            "uploaded_file",
            "",
            accept = ".csv",
            placeholder = "No file selected"
          ),
          # Click on Button to view the sample format dataset to upload.
          actionLink("upload_format", "View Sample Dataset",
                     class = "sample_dataset_link")
        )
      ),
      
      # Display the summary information of the uploaded csv.
      htmlOutput("upload_summary_text"),
      
      fluidRow(
        style = "padding-left: 50px; padding-bottom: 10px",
        column(width = 6,
               div(id = "upload_summary",
                   div(class = "row col-sm-12 mb-4 u_p_dropdown_row",
                       uiOutput("upload_summary_select")),
                   # Display the table with total rows in the DB.
                   dataTableOutput("total_new_undis_dup_table"))
        )
      )
    )
  })
  
  
  
  # Implement the intro logic. Sidebar steps are listed in global.r
  # this dataset is also static... perhaps it should be sourced from global.r?
  upload_pkg_initial_steps <- reactive(
    data.frame(
      # Note that we access chooseCSVtext with '.' instead of '#', because we track its class and not its id.
      element = c("#help", ".chooseCSVtext", ".sample_dataset_link"),
      intro = c(
        "Click here anytime you need help.",
        "Upload a CSV file with the package(s) you would like to assess.",
        "You can use this sample dataset to explore the app."
      ),
      position = c("right", rep("top", 2))
    )
  )
  upload_pkg_steps <- reactive(
    if(values$upload_complete == "upload_complete"){
      data.frame(
        element = c("#upload_summary_text", "#upload_summary"),
        intro = c(
          "Text description of packages uploaded. Counts by type: 'Total', 'New', 'Undiscovered', 'Duplicate'.",
          "Confirm uploaded packages list, filter by type"
        ),
        position = c("bottom", "top")
      )
    } else {
      data.frame(element = character(0) , intro = character(0), position = character(0))
    }
    
  )
  
  # Start introjs when help button is pressed.
  observeEvent(input$help,
               introjs(session,
                       options = list(
                         steps = 
                           upload_pkg_initial_steps() %>%
                           union(upload_pkg_steps()) %>%
                           union(sidebar_steps),
                         "nextLabel" = "Next",
                         "prevLabel" = "Previous"
                       )
               )
  )
  
  # Sample csv file content.
  data <- reactive({
    data.table(upload_format) # internal data
  })
  
  # Load the columns from DB into reactive values.
  observeEvent(list(input$total_new_undis_dup,input$uploaded_file), {
    req(values$upload_complete == "upload_complete")
    
    # After upload complete, update db dash screen with new package(s)
    values$db_pkg_overview <- update_db_dash()
    
    if (input$total_new_undis_dup == "All") {
      values$Total_New_Undis_Dup <- values$Total
    } else if (input$total_new_undis_dup == "New") {
      values$Total_New_Undis_Dup <- values$New
    } else if (input$total_new_undis_dup == "Undiscovered") {
      values$Total_New_Undis_Dup <- values$Undis
    } else if (input$total_new_undis_dup == "Duplicates") {
      values$Total_New_Undis_Dup <- values$Dup
    } 
  }, ignoreInit = TRUE)  # End of the observe.
  
  # 2. Observe to disable the input widgets while the packages uploading into DB.
  
  observeEvent(input$uploaded_file, {
    # req(input$uploaded_file)
    values$uploaded_file_status <- file_upload_error_handling(input$uploaded_file)
    if (values$uploaded_file_status != "no_error") {
      shinyjs::hide("upload_summary_text")
      shinyjs::hide("upload_summary_select")
      shinyjs::hide("total_new_undis_dup_table")
      reset("uploaded_file") 
      return()
    } else{
      shinyjs::show("upload_summary_text")
      shinyjs::show("upload_summary_select")
      shinyjs::show("total_new_undis_dup_table")
    }
    file_to_read <- input$uploaded_file
    pkgs_file <-
      read.csv(file_to_read$datapath,
               sep = ",",
               stringsAsFactors = FALSE)
    names(pkgs_file) <- tolower(names(pkgs_file))
    pkgs_file$package <- trimws(pkgs_file$package)
    pkgs_file$version <- trimws(pkgs_file$version)
    values$Total <- pkgs_file
    pkgs_db1 <- db_fun("SELECT name FROM package")
    values$Dup <- filter(values$Total, values$Total$package %in% pkgs_db1$name)
    values$New <- filter(values$Total, !(values$Total$package %in% pkgs_db1$name))
    withProgress(message = "Uploading Packages to DB:", value = 0, {
      if (nrow(values$New) != 0) {
        for (i in 1:nrow(values$New)) {
          incProgress(1 / (nrow(values$New) + 1), detail = values$New[i, 1])
          new_package<-values$New$package[i]
          get_packages_info_from_web(new_package)
          metric_mm_tm_Info_upload_to_DB(new_package)
          metric_cum_Info_upload_to_DB(new_package)
        }
      }
    })
    
    pkgs_db2 <- db_fun("SELECT name FROM package")
    
    values$Undis <-
      filter(values$New, !(values$New$package %in% pkgs_db2$name))
    values$packsDB <- pkgs_db2
    
    updateSelectizeInput(
      session,
      "select_pack",
      choices = c("Select", values$packsDB$name),
      selected = "Select"
    )
    
    showNotification(id = "show_notification_id", "Upload completed", type = "message")
    values$upload_complete <- "upload_complete"
    
    # Show the download reports buttons after all the packages have been loaded
    # and the information extracted.
    loggit("INFO", paste("Summary of the uploaded file:",input$uploaded_file$name, 
                         "Total Packages:", nrow(values$Total),
                         "New Packages:", nrow(values$New),
                         "Undiscovered Packages:", nrow(values$Undis),
                         "Duplicate Packages:", nrow(values$Dup)), echo = FALSE)
  }, ignoreInit = TRUE)  # End of the Observe.
  
  # 1. Render Output to download the sample format dataset.
  output$upload_format_download <- downloadHandler(
    filename = function() {
      paste("Upload_file_structure", ".csv", sep = "")
    },
    content = function(file) {
      # upload_format is internal data
      write.csv(upload_format, file, row.names = F)
    }
  )
  
  # 2. Render Output to show the summary of the uploaded csv into application.
  
  output$upload_summary_text <- renderText({
    if (values$upload_complete == "upload_complete") {
      paste(
        "<br><br><hr>",
        "<h3><b>Summary of uploaded package(s) </b></h3>",
        "<h4>Total Packages: ", nrow(values$Total), "</h4>",
        "<h4>New Packages:",  nrow(values$New), "</h4>",
        "<h4>Undiscovered Packages:", nrow(values$Undis), "</h4>",
        "<h4>Duplicate Packages:", nrow(values$Dup), "</h4>",
        "<h4><b>Note: The assessment will be performed on the latest version of each package, irrespective of the uploaded version."
      )
    }
  })  # End of the render Output.
  
  # 3. Render Output to show the select input to select the choices to display the table.
  
  output$upload_summary_select <- renderUI({
    if (values$upload_complete == "upload_complete") {
      removeUI(selector = "#Upload")
      selectInput(
        "total_new_undis_dup",
        "",
        choices = c("All", "New", "Undiscovered", "Duplicates")
      )
    } 
  })  # End of the render Output.
  
  # 4. Render Output to show the data table of uploaded csv.
  
  output$total_new_undis_dup_table <- DT::renderDataTable({
    if (values$upload_complete == "upload_complete") {
      datatable(
        values$Total_New_Undis_Dup,
        escape = FALSE,
        class = "cell-border",
        selection = 'none',
        extensions = 'Buttons',
        options = list(
          searching = FALSE,
          sScrollX = "100%",
          lengthChange = FALSE,
          aLengthMenu = list(c(5, 10, 20, 100,-1), list('5', '10', '20', '100', 'All')),
          iDisplayLength = 5
        )
      )
    }
  }) # End of the render Output 
  # End of the Render Output's'.
  
  # View sample dataset
  observeEvent(input$upload_format, {
    dataTableOutput("sampletable")
    showModal(modalDialog(
      output$sampletable <- DT::renderDataTable(
        datatable(
          data(),
          escape = FALSE,
          class = "cell-border",
          editable = FALSE,
          filter = "none",
          selection = 'none',
          extensions = 'Buttons',
          options = list(
            sScrollX = "100%",
            aLengthMenu = list(c(5, 10, 20, 100, -1), list('5', '10', '20', '100', 'All')),
            iDisplayLength = 5,
            dom = 't'
          )
        )
      ),
      downloadButton("upload_format_download", "Download", class = "btn-secondary")
    ))
  })  # End of the observe event for sample button.
  
}
    
## To be copied in the UI
# mod_uploadpackage_ui("uploadpackage_ui_1")
    
## To be copied in the server
# mod_uploadpackage_server("uploadpackage_ui_1")
