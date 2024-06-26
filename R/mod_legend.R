#' legend UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom bslib tooltip
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
    )
  )
}
    
#' legend Server Functions
#'
#' @importFrom shiny showModal modalDialog HTML
#' @noRd 
legendServer <- function(id){
  moduleServer( id, function(input, output, session){
 
    observeEvent(input$score_legend, {
      shiny::showModal(shiny::modalDialog(
        size = "l",
        easyClose = TRUE,
        title = h3("Risk Score Legend"),
        footer = tagList(
          # icon('circle-info'),
          HTML("<span><i class='fas fa-circle-info'></i> The displayed assessment-level scores are inverted from the {riskmetric} package to maintain a consistent interpretation with the overall package score, where 0 indidcates low risk and 1 indicates high risk.</span>"),
          modalButton("Dismiss")),
        tagList(
          br(),
          div(style = "display: flex; align-items: center;",
            icon("triangle-exclamation", class = "text-warning", verify_fa = FALSE, style = "font-size:65px; padding-left: 10px;"),
            div(style = "position: absolute; left: 8rem; padding-right: 20px;",
                HTML("A 'Not Found' indicates there was no assessment value derived from {riskmetric}'s fetch for this information. This is usually due to the reference sourced used when loading the package into the database. See <a href='https://pharmar.github.io/riskmetric/reference/pkg_ref.html#details' target='_blank'><code>riskmetric::pkg_ref</code></a> for more information. Since there is no assessment value, there is also no score. As such, this score will NOT impact the package's overall risk score."))),
          br(),br(),br(),
          div(style = "display: flex; align-items: center;",
              metric_gauge("NA"),
              div(style = "position: absolute; left: 8rem; padding-right: 20px;",
                  "An 'NA' indicates an assessment value exists in the {riskmetric} output, but there is no score recorded. This is usually an intentional practice performed by {riskmetric}'s development team when scoring a metric is ambiguous or strange, thus a score is not applicable. This score will NOT impact the package's overall risk score.")),
          br(),
          div(style = "display: flex; align-items: center;",
              metric_gauge("0"),
              div(style = "position: absolute; left: 8rem; padding-right: 20px;",
                  "A score of 1 indicates the highest risk possible, based on the {riskmetric}-derived assessment value.")),
          br(),
          div(style = "display: flex; align-items: center;",
              metric_gauge(".50"),
              div(style = "position: absolute; left: 8rem; padding-right: 20px;",
                  "A score close to 1 indicates that the {riskmetric} assessment value is high risk, where a score closer to 0 inidicates lower risk")),
          br(),
          div(style = "display: flex; align-items: center;",
              metric_gauge("1"),
              div(style = "position: absolute; left: 8rem; padding-right: 20px;",
                  "A score of 0 indicates the lowest possible risk, based on the {riskmetric}-derived assessment value."))
        )
      ))
    })
    
  })
}
    

