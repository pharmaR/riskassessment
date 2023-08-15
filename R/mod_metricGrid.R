
#' Metric Grid module's UI.
#' 
#' @param id a module id name
#' @keywords internal
#' 
metricGridUI <- function(id) {
  fluidPage(uiOutput(NS(id, 'grid')))
}


#' Metric Grid module's server logic
#' 
#' @param id a module id name
#' @param metrics placeholder
#' 
#' @keywords internal
#' 
#' @import dplyr
#' @importFrom stringr str_extract
metricGridServer <- function(id, metrics) {
  moduleServer(id, function(input, output, session) {
    
    metric <- dbSelect("select * from metric", db_name = golem::get_golem_options('assessment_db_name'))
    
    output$grid <- renderUI({
      req(nrow(metrics()) > 1) # need at least two cards to make a metric grid UI

      columns <- 3
      column_vector_grid_split <- split(seq_len(nrow(metrics())), rep(1:columns, length.out = nrow(metrics())))

      fluidRow(style = "padding-right: 10px", class = "card-group",
               map(column_vector_grid_split, 
                   ~ column(width= 4,map(.x,~ metricBoxUI(session$ns(metrics()$name[.x]))))),
      if(any(!(metrics()$title %in% metric$long_name)) & stringr::str_extract(session$ns(id), "\\w+") != "databaseView") {
        tags$em("* Provided for additional context. Not a {riskmetric} assessment, so this measure will not impact the risk score.")
      } 
      )
    })
    
    observeEvent(req(nrow(metrics()) > 0), {
      apply(metrics(), 1, function(metric)
        metricBoxServer(id = metric['name'],
            title = metric['title'],
            desc = metric['desc'],
            value = dplyr::case_when(metric['name'] != 'has_bug_reports_url' ~ metric['value'],
                                     metric['value'] == "1" ~ 'TRUE',
                                     TRUE ~ 'FALSE'),
            is_perc = metric['is_perc'] == 1,
            is_url = metric['is_url'] == 1,
            succ_icon = metric['succ_icon'],
            icon_class = metric['icon_class'],
            type = metric['type']
          )
        )
    })
  })
}
