#' UI for 'About' Module
#' 
#' @param id a module id name
#' 
#' @importFrom DT dataTableOutput
#' 
#' @keywords internal
#' 
aboutInfoUI <- function(id) {
  fluidPage(
    fluidRow(
      column(
        width = 8, offset = 2,
        h2("About Risk Assessment Application", align = "center", `padding-bottom`="20px"),
        br(),
        
        # Assessment criteria sub-tab
        tabsetPanel(
          tabPanel(
            title = "Assessment Criteria",
            icon = icon("circle-info"),
            assessmentInfoUI("assessmentInfo")  # call assessment module UI
               ),
        # Contacts sub-tab  
          tabPanel(
            title = "Contact",
            h6("List of Important Contacts"),
            br(),
            uiOutput(NS(id,"contact_text")),  # ui output with contacts description.
          ),
        # Contributors sub-tab
          tabPanel(
            title = "Contributors and Companies",
            h6("List of Contributors and Comanies"),
            br(),
            uiOutput(NS(id,"contributor_text")), # ui output with contributors and companies description.
          )
        )
      )))
}

#' Server Logic for 'About' Module
#' 
#' @param id a module id name
#' 
#' 
#' @keywords internal
#' 
aboutInfoServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Display the contact description.

    output$contact_text <- renderUI({contact_text})

    # Display the contributor description.
    output$contributor_text <- renderUI({contributor_text})
    
    # Load server of the assessment criteria module.
    assessmentInfoServer("assessmentInfo", metric_weights = metric_weights) 
    
  })
}
