#' legend UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom bslib popover
legendUI <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      style = "position: absolute; top: 1.5rem; right: 4.5rem; margin-right: auto;",
      shinyWidgets::actionBttn(
           NS(id, "score_legend"),
           "",
           color = "success",
           icon = icon("square-check"),
           block = FALSE,
           style = "simple",
           size = "md") |>
        bslib::tooltip("About Score Cards")
        # bslib::popover(
        #   div(style = "display: flex; align-items: center;", metric_gauge("NA"), br(), "text about NA"), br(),
        #   div(style = "display: flex; align-items: center;", metric_gauge("0"), br(), "text about 1"), br(),
        #   div(style = "display: flex; align-items: center;", metric_gauge(".50"), br(), "text about .50"), br(),
        #   div(style = "display: flex; align-items: center;", metric_gauge("1"), br(),"text about 0"),
        #   title = "Risk Score Legend"
        # )
    )
  )
}
    
#' legend Server Functions
#'
#' @importFrom shiny showModal modalDialog
#' @noRd 
legendServer <- function(id){
  moduleServer( id, function(input, output, session){
 
    observeEvent(input$score_legend, {
      shiny::showModal(shiny::modalDialog(
        size = "l",
        easyClose = TRUE,
        title = h3("Risk Score Legend"),
        tagList(
          br(),
          div(style = "display: flex; align-items: center;",
            icon("triangle-exclamation", class = "text-warning", verify_fa = FALSE, style = "font-size:65px; padding-left: 10px;"),
            div(style = "position: absolute; left: 8rem; padding-right: 20px;", "A 'Not Found' indicates there was no assessment value derived from {riskmetric}'s fetch for this information. This is usually due to the reference sourced used when loading the package into the database. For example, {riskmetric} cannot fetch testing coverage when using the CRAN Remote reference source. Since there is no assessment value, there is also no score. As such, this score will not impact the package's overall risk score.")),
          br(),br(),br(),
          div(style = "display: flex; align-items: center;",
              metric_gauge("NA"),
              div(style = "position: absolute; left: 8rem; padding-right: 20px;", "An 'NA' indicates an assessment value exists for {riskmetric}, but there is no score recorded. This is usually an intention practice performed by {riskmetric}'s development team when scoring such a metric is ambiguous or strange, thus a score is not applicable")),
          br(),
          div(style = "display: flex; align-items: center;",
              metric_gauge("0"),
              div(style = "position: absolute; left: 8rem; padding-right: 20px;","A score of 1 indicates the highest risk possible based on the {riskmetric} assessment value.")),
          br(),
          div(style = "display: flex; align-items: center;",
              metric_gauge(".50"),
              div(style = "position: absolute; left: 8rem; padding-right: 20px;", "A Score close to 1 indicates the {riskmetric} assessment value is high risk, where scores closer to 0 inidicate lower risk")),
          br(),
          div(style = "display: flex; align-items: center;",
              metric_gauge("1"),
              div(style = "position: absolute; left: 8rem; padding-right: 20px;", "A score of 0 indicates the lowest possible risk based on the {riskmetric} assessment value."))
        )
      ))
    })
    
  })
}
    

