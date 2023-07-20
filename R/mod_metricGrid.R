
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
      
      col_length <- (nrow(metrics()) + 1) %/% 3
      
      adjustment_for_custom_box_grid <- ifelse(nrow(metrics()) %% col_length == 0,
                                               0,
                                               ifelse(nrow(metrics()) %% col_length ==1,
                                                      1,
                                                      -1))
      column_vector_grid_split <- split(seq_len(nrow(metrics())), rep(1:3, c(col_length, 
                                              col_length + adjustment_for_custom_box_grid, 
                                              col_length)))
    
      fluidRow(style = "padding-right: 10px", class = "card-group",
               map(column_vector_grid_split, 
                   ~ column(width= 4,map(.x,~ metricBoxUI(session$ns(metrics()$name[.x])))))
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
