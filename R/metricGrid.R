cards_style <- "padding-right: 10px"

metricGridUI <- function(id) {
  fluidPage(
    fluidRow(style = cards_style, class = "card-group",
             column(width = 4,
                    metricBoxUI(NS(id, "has_vignettes")),
                    metricBoxUI(NS(id, "news_current")),
                    metricBoxUI(NS(id, "has_source_control"))),
             column(width = 4,
                    metricBoxUI(NS(id, "has_maintainer")),
                    metricBoxUI(NS(id, "has_bug_reports_url")),
                    metricBoxUI(NS(id, "export_help"))),
             column(width = 4,
                    metricBoxUI(NS(id, "has_news")),
                    metricBoxUI(NS(id, "has_website")),
                    metricBoxUI(NS(id, "bugs_status")))
    ))
}

metricGridServer <- function(id, metrics) {
  moduleServer(id, function(input, output, session) {
    
    # Create 
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