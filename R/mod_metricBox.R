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
#' @param type string to color the icon ("information" or "danger")
#'
#'
#' @import dplyr
#' @importFrom stringr str_sub str_extract
#' @importFrom glue glue
#' @keywords internal
#'
metricBoxServer <- function(id, title, desc, value, score = NULL,
                            is_perc = FALSE, is_url = FALSE,
                            succ_icon = "check", unsucc_icon = "times",
                            icon_class = "text-success", type = "information"
                            ) {
  moduleServer(id, function(input, output, session) {

    metric <- dbSelect("select * from metric", db_name = golem::get_golem_options('assessment_db_name'))
    
    # Render metric.
    output$metricBox_ui <- renderUI({
      req(title, desc)

      # A str length of 41 chars tends to wrap to two rows and look quite nice
      val_max_nchar <- 31
      is_true <- !(value %in% c(0, "pkg_metric_error", "NA", "", "FALSE", NA))
      
      # logic for assessment values
      if (value %in% c("pkg_metric_error", "NA", NA)) {
        value <- "Not found"
        # if(!is.null(score)) {score <- "NA"} # Shouldn't be needed
      } else if (is_perc) {
        value <- glue::glue("{round(as.numeric(value), 1)}%")
      } else if (is_url) {
        value <- a(ifelse(nchar(value) <= val_max_nchar, value,
          glue::glue("{stringr::str_sub(value, 1, (val_max_nchar - 3))}...")
        ), href = value)
      } # unfortunately, adding the href can sometimes force the footer to fall
      # outside the card when val_max_nchar is too large.
      else if (value %in% c("TRUE", "FALSE")) {
        value <- ifelse(value == "TRUE", "Yes", "No")
      }

      
      # add asterisk to title if it is not in the metric table
      # skip databaseView cards
      title = if_else(stringr::str_extract(session$ns(id), "\\w+") != "databaseView" 
                      & !title %in% metric$long_name, paste0(title, "*"), title)

      # define some styles prior to building card
      card_style <- "max-width: 400px; max-height: 250px; padding-left: 5%; padding-right: 5%;" # overflow-y: scroll;
      auto_font_out <- auto_font(value,
        txt_max = val_max_nchar,
        size_min = .85, size_max = 1.5
      ) # , num_bins = 3
      
      body_p_style <- glue::glue("font-size: {auto_font_out}vw")

      # build the html card
      if(is.null(score) | !is_true) { # use icon version
        
        # maintain icon logic
        icon_name <- succ_icon # succ_icon should never show in practice
        if (!is_true) {
          icon_name <- unsucc_icon
          icon_class <- "text-warning"
        }
        # if (is_perc) {
        #   icon_name <- "percent"
        #   icon_class <- "text-info"
        # }
        
        html_component <- div(
          class = "card mb-3 text-center border-info", style = card_style,
          div(
            class = "row no-gutters;",
            div(
              class = "col-md-4 text-center border-info",
              icon(icon_name,
                   class = icon_class, verify_fa = FALSE,
                   style = "padding-top: 40%; font-size:60px; padding-left: 20%;"
              )
            ),
            div(
              class = "col-md-8",
              h5(
                class = "card-header bg-transparent", style = "font-size: 1vw",
                title
              ),
              div(
                class = "card-body text-info",
                p(class = "card-title", style = body_p_style, value)
              )
            ),
            div(class = "card-footer bg-transparent", desc)
          )
        )
      
      } else { # use image version (displaying riskmetric gauge)
        
        html_component <- div(
          class = "card mb-3 text-center border-info", style = card_style,
          div(
            class = "row no-gutters;",
            div(
              class = "col-md-4 text-center border-info",
              img(src=glue::glue("www/scores/rm-gauge-{score}.png"),
              style = "width: 128px; padding-top: 40%; font-size:60px; padding-left: 20%;")
              # div(style = "width: 128px; padding-top: 40%; font-size:60px; padding-left: 20%;", 
              #   flexdashboard::gauge(score, 
              #       min = 0, 
              #       max = 1, 
              #       sectors = flexdashboard::gaugeSectors(
              #         success = c(0, 33), 
              #         warning = c(34, 66),
              #         danger = c(67, 100)
              #       )
              #   )
              # )
            ),
            div(
              class = "col-md-8",
              h5(
                class = "card-header bg-transparent", style = "font-size: 1vw",
                title
              ),
              div(
                class = "card-body text-info",
                p(class = "card-title", style = body_p_style, value)
              )
            ),
            div(class = "card-footer bg-transparent", desc)
          )
        )
        
        
      }
      
      
      if (type == "danger" & !is.na(type)) {
        html_component %>% 
          shiny::tagAppendAttributes(class = "text-danger", .cssSelector = "i") %>% 
          shiny::tagAppendAttributes(class = "text-danger", .cssSelector = "p")
      } else {
        html_component
      }
    })
  })
}
