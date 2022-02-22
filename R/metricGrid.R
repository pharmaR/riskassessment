cards_style <- "padding-right: 10px"

metricGridUI <- function(id) {
  fluidPage(
    fluidRow(style = cards_style, class = "card-group",
             column(width = 4,
                    uiOutput(NS(id, "col1"))),
             column(width = 4,
                    uiOutput(NS(id, "col2"))),
             column(width = 4,
                    uiOutput(NS(id, "col3")))
  ))
}

metricGridServer <- function(id, metrics) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    metric_lst <- reactive({
      metrics()[[1]] %>%
        split(ceiling((3*seq_along(.)-1)/length(.)))
    })
    
    output$col1 <- renderUI({
      lapply(metric_lst()[[1]], function(x) metricBoxUI(ns(x)))
    })

    output$col2 <- renderUI({
      lapply(metric_lst()[[2]], function(x) metricBoxUI(ns(x)))
    })

    output$col3 <- renderUI({
      lapply(metric_lst()[[3]], function(x) metricBoxUI(ns(x)))
    })
    
    # Create 
    observeEvent(metrics(), {
      if (all(c('name', 'long_name', 'description', 'value', 'is_perc', 'is_url') %in% names(metrics()))) {
        apply(metrics(), 1, function(metric)
          metricBoxServer(id = metric['name'],
                          title = metric['long_name'],
                          desc = metric['description'],
                          value = metric['value'],
                          is_perc = metric['is_perc'] == 1,
                          is_url = metric['is_url'] == 1))
      } else if (all(c('id', 'title', 'desc', 'value', 'succ_icon', 'icon_class') == names(metrics()))) {
        pmap(metrics(), metricBoxServer)
      }
    })
  })
}