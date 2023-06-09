
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
      
      col_length <- floor((nrow(metrics()) + 1)/3)
      
      fluidRow(style = "padding-right: 10px", class = "card-group",
               column(width = 4, {
                 lapply(X = 1:col_length, function(i){
                   metricBoxUI(session$ns(metrics()$name[i]))
                 })
               }),
               column(width = 4, {
                 lapply(X = (col_length + 1):(2*col_length), function(i){
                   metricBoxUI(session$ns(metrics()$name[i]))
                 })
               }),
               column(width = 4, {
                 lapply(X = (2*col_length + 1):nrow(metrics()), function(i){
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
