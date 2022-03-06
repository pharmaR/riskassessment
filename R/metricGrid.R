metricGridUI <- function(id) {
  fluidPage(uiOutput(NS(id, 'grid')))
}

metricGridServer <- function(id, metrics) {
  moduleServer(id, function(input, output, session) {
    
    output$grid <- renderUI({
      req(metrics())
      
      col_length <- nrow(metrics())%/%3

      fluidRow(style = "padding-right: 10px", class = "card-group",
               column(width = 4, {
                 lapply(X = 1:col_length, function(i){
                   metricBoxUI(NS(id, metrics()$name[i]))
                 })
               }),
               column(width = 4, {
                 lapply(X = (col_length + 1):(2*col_length), function(i){
                   metricBoxUI(NS(id, metrics()$name[i]))
                 })
               }),
               column(width = 4, {
                 lapply(X = (2*col_length + 1):nrow(metrics()), function(i){
                   metricBoxUI(NS(id, metrics()$name[i]))
                 })
               })
      )
    })
    
    observeEvent(metrics(), {
      
      apply(metrics(), 1, function(metric)
        metricBoxServer(id = metric['name'],
                        title = metric['long_name'],
                        desc = metric['description'],
                        value = metric['value'],
                        is_perc = metric['is_perc'] == 1,
                        is_url = metric['is_url'] == 1))
    })
  })
}