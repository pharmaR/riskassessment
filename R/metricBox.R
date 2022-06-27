metricBoxUI <- function(id) {
  uiOutput(NS(id, "metricBox_ui"))
}

#' `title`: title.
#' `desc`: description.
#' `value`: metric value.
#' `is_true`: whether the metric is TRUE. If true, then the succ_icon will be
#' used; if false, then the unsucc_icon will be used.
#' `is_perc`: whether the value is a percentage.
#' `succ_icon`: icon used if is_true.
#' `unsucc_icon`: icon used if not is_true.
metricBoxServer <- function(id, title, desc, value,
                            is_perc = FALSE, is_url = FALSE,
                            succ_icon = "check",  unsucc_icon = "times",
                            icon_class = "text-success") {
  moduleServer(id, function(input, output, session) {
    
    
    # Render metric.
    output$metricBox_ui <- renderUI({
      req(title, desc, value)
      
      is_true <- !(value %in% c(0, "pkg_metric_error", "NA", "", 'FALSE'))
      
      if(value %in% c("pkg_metric_error", "NA"))
        value <- "Not found"
      else if(is_perc)
        value <- glue::glue('{round(as.numeric(value), 1)}%')
      else if(is_url)
        value <- a(glue::glue('{str_sub(value, 1, 29)}...'), href = value)
      else if(value %in% c('TRUE', 'FALSE'))
        value <- ifelse(value == 'TRUE', 'Yes', 'No')
      
      icon_name <- succ_icon

      if(!is_true){
        icon_name <- unsucc_icon
        icon_class <- "text-warning"
      }
      
      if(is_perc){
        icon_name <- "percent"
        icon_class <- "text-info"
      }
      
      card_style = "max-width: 400px; max-height: 250px; overflow-y: scroll;"
      
      div(class="card mb-3 text-center border-info", style=card_style,
          div(class ="row no-gutters",
              div(class="col-md-4 text-center border-info",
                  icon(icon_name, class=icon_class,
                       style="padding-top: 40%; font-size:60px; padding-left: 20%;")),
              div(class="col-md-8",
                  h5(class="card-header bg-transparent", style="font-size: 1vw",
                     title),
                  div(class="card-body text-info",
                      p(class="card-title", style="font-size: 1.5vw", value))),
              div(class="card-footer bg-transparent", desc)
          )
      )
    })
  })
}