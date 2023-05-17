
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
metricGridServer <- function(id, metrics) {
  moduleServer(id, function(input, output, session) {
    
    
    output$grid <- renderUI({
      req(nrow(metrics()) > 0)
      
      col_width <- 3 # This is how many columns are defined below.

      # cards are presented across in row-major order instead of down by column
      fluidRow(style = "padding-right: 10px", class = "card-group",
               column(width = 4, {
                 lapply(X = seq(1, nrow(metrics()), col_width), function(i){
                   metricBoxUI(session$ns(metrics()$name[i]))
                 })
               }),
               column(width = 4, {
                 lapply(X = seq(2, nrow(metrics()), col_width), function(i){
                   metricBoxUI(session$ns(metrics()$name[i]))
                 })
               }),
               column(width = 4, {
                 lapply(X = seq(3, nrow(metrics()), col_width), function(i){
                   metricBoxUI(session$ns(metrics()$name[i]))
                 })
               })
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
            icon_class = metric['icon_class'])
        )
    })
  })
}
