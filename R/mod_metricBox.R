#' The UI for the 'Metric Box' module
#' 
#' @param id a module id name
#' @keywords internal
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
#' @keywords internal
#'   
metricBoxServer <- function(id, title, desc, value,
                            is_perc = FALSE, is_url = FALSE,
                            succ_icon = "check",  unsucc_icon = "times",
                            icon_class = "text-success") {
  moduleServer(id, function(input, output, session) {
    
    
    # Render metric.
    output$metricBox_ui <- renderUI({
      req(title, desc, value)
      
      # A str length of 41 chars tends to wrap to two rows and look quite nice
      val_max_nchar <- 31
      is_true <- !(value %in% c(0, "pkg_metric_error", "NA", "", 'FALSE'))
      
      if(value %in% c("pkg_metric_error", "NA"))
        value <- "Not found"
      else if(is_perc)
        value <- glue::glue('{round(as.numeric(value), 1)}%')
      else if(is_url)
        value <- a(ifelse(nchar(value) <= val_max_nchar, value,
                    glue::glue('{stringr::str_sub(value, 1, (val_max_nchar - 3))}...')
                 ), href = value)
        # unfortunately, adding the href can sometimes force the footer to fall
        # outside the card when val_max_nchar is too large.
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
      
      # define some styles prior to building card
      card_style = "max-width: 400px; max-height: 250px; padding-left: 5%; padding-right: 5%;" # overflow-y: scroll;
      auto_font_out <- auto_font(value, txt_max = val_max_nchar,
                                 size_min = .85, size_max = 1.5) #, num_bins = 3
      body_p_style = glue::glue('font-size: {auto_font_out}vw')
      
      div(class="card mb-3 text-center border-info", style=card_style,
          div(class ="row no-gutters;",
              div(class="col-md-4 text-center border-info",
                  icon(icon_name, class=icon_class, verify_fa = FALSE,
                       style="padding-top: 40%; font-size:60px; padding-left: 20%;")),
              div(class="col-md-8",
                  h5(class="card-header bg-transparent", style="font-size: 1vw",
                     title),
                  div(class="card-body text-info",
                      p(class="card-title", style= body_p_style, value))),
              div(class="card-footer bg-transparent", desc)
          )
      )
    })
  })
}

