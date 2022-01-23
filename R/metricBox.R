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
metricBoxServer <- function(id, title, desc, value, is_true = reactive(TRUE),
                            is_perc = reactive(FALSE),
                            succ_icon = "check",  unsucc_icon = "times") {
  moduleServer(id, function(input, output, session) {
    
    # Render metric.
    output$metricBox_ui <- renderUI({
      req(title(), desc(), value())
      icon_name <- succ_icon
      icon_class <- "text-success"
      
      if(!is_true()){
        icon_name <- unsucc_icon
        icon_class <- "text-warning"
      }
      
      if(is_perc()){
        icon_name <- "percent"
        icon_class <- "text-info"
      }
      
      div(class="card mb-3 text-center border-info", style="max-width: 250px;",
          div(class ="row no-gutters",
              div(class="col-md-4 text-center border-info",
                  icon(icon_name, class=icon_class,
                       style="padding-top: 40%; font-size:60px; padding-left: 20%;")),
              div(class="col-md-8",
                  h5(class="card-header bg-transparent", title()),
                  div(class="card-body text-info",
                      h2(class="card-title", value()))),
              div(class="card-footer bg-transparent", desc())
          )
      )
    })
  })
}