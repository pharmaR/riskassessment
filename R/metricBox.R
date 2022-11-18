#' The UI for the 'Metric Box' module
#' 
#' @param id a module id name
#' 
#' 
metricBoxUI <- function(id) {
  uiOutput(NS(id, "metricBox_ui"))
}

#' Server logic for the 'Metric Box' module
#'
#' @param id a module id name
#' @param title title.
#' @param desc description.
#' @param value metric value.
#' @param is_perc logical is the value is a percentage?
#' @param is_url  logical is the value a url
#' @param succ_icon icon used if is_true.
#' @param unsucc_icon icon used if not is_true.
#' @param icon_class string type of icon
#'
#' 
#' @import dplyr
#' @importFrom stringr str_sub
#' @importFrom glue glue
#'   
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
        value <- a(glue::glue('{stringr::str_sub(value, 1, 29)}...'), href = value)
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
