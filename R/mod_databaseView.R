# Global Risk color palettes.
# run locally and paste hex codes
# colorspace::darken(viridisLite::turbo(11, begin = 0.4, end = .8225), .25)
low_risk_color  <- "#06B756FF"  # 1st
med_risk_color  <- "#A99D04FF"  # 6th
high_risk_color <- "#A63E24FF"  # 11th
setColorPalette <- colorRampPalette(
  c("#06B756FF","#2FBC06FF","#67BA04FF","#81B50AFF","#96AB0AFF","#A99D04FF",
    "#B78D07FF","#BE7900FF","#BE6200FF","#B24F22FF","#A63E24FF"))


#' UI for 'Database View' module
#' 
#' @param id a module id name
#' 
#' 
#' @importFrom shinydashboard box
#' @importFrom DT dataTableOutput
#' 
#' @keywords internal
databaseViewUI <- function(id) {
  fluidPage(
    fluidRow(
      column(
        width = 8, offset = 2, align = "center",
        br(),
        h4("Database Overview"),
        hr(),
        tags$section(
          br(), br(),
          shinydashboard::box(width = 12,
              title = h5("Uploaded Packages", style = "margin-top: 5px"),
              DT::dataTableOutput(NS(id, "packages_table")),
              br(),
              fluidRow(
                column(
                  width = 6,
                  style = "margin: auto;",
                  mod_downloadHandler_button_ui(NS(id, "downloadHandler"), multiple = TRUE)),
                column(
                  width = 6,
                  mod_downloadHandler_filetype_ui(NS(id, "downloadHandler"))
                )
              )))
      ))
  )
}

#' Server logic for 'Database View' module
#'
#' @param id a module id name
#' @param user a user name
#' @param uploaded_pkgs a vector of uploaded package names
#' @param metric_weights a reactive data.frame holding metric weights
#' @param changes a reactive value integer count
#' @param parent the parent (calling module) session information
#'
#' 
#' @import dplyr
#' @importFrom lubridate as_datetime
#' @importFrom stringr str_replace_all str_replace
#' @importFrom shinyjs enable disable
#' @importFrom rmarkdown render
#' @importFrom glue glue
#' @importFrom DT renderDataTable formatStyle
#' @importFrom formattable formattable as.datatable formatter style csscolor
#'   icontext
#' @keywords internal
databaseViewServer <- function(id, user, uploaded_pkgs, metric_weights, changes, parent) {
  moduleServer(id, function(input, output, session) {
    
    ns = session$ns
    
    # used for adding action buttons to table_data
    shinyInput <- function(FUN, len, id, ...) {
      inputs <- character(len)
      for (i in seq_len(len)) {
        inputs[i] <- as.character(FUN(paste0(id, i), ...))
      }
      inputs
    }
    
    # Update table_data if a package has been uploaded
    table_data <- eventReactive({uploaded_pkgs(); changes()}, {
      
      db_pkg_overview <- dbSelect(
        'SELECT pi.name, pi.version, pi.score, pi.decision, c.last_comment
        FROM package as pi
        LEFT JOIN (
            SELECT id, max(added_on) as last_comment FROM comments GROUP BY id)
        AS c ON c.id = pi.name
        ORDER BY 1 DESC'
      )
      
      db_pkg_overview %>%
        dplyr::mutate(last_comment = as.character(lubridate::as_datetime(last_comment))) %>%
        dplyr::mutate(last_comment = ifelse(is.na(last_comment), "-", last_comment)) %>%
        dplyr::mutate(decision = ifelse(decision != "", paste(decision, "Risk"), "-")) %>%
        dplyr::mutate(was_decision_made = ifelse(decision != "-", TRUE, FALSE)) %>%
        dplyr::select(name, version, score, was_decision_made, decision, last_comment)
    })
    
    exportTestValues(
      table_data = {
        table_data()
      },
      pkgs = {
        pkgs()
      }
    )
    
    # Create table for the db dashboard.
    output$packages_table <- DT::renderDataTable({
      
      my_data_table <- reactive({
        cbind(table_data(), 
        tibble::tibble(
          Actions = shinyInput(actionButton, nrow(table_data()),
                               'button_',
                               label = icon("microscope", class="fa-regular", lib = "font-awesome"),
                               onclick = paste0('Shiny.onInputChange(\"' , ns("select_button"), '\", this.id)')
          )
        )
        )
      })
      
      formattable::as.datatable(
        formattable::formattable(
          my_data_table(),
          list(
            score = formattable::formatter(
              "span",
              style = x ~ formattable::style(display = "block",
                                "border-radius" = "4px",
                                "padding-right" = "4px",
                                "font-weight" = "bold",
                                "color" = "white",
                                "order" = x,
                                "background-color" = formattable::csscolor(
                                  setColorPalette(100)[round(as.numeric(x)*100)]))),
            decision = formattable::formatter(
              "span",
              style = x ~ formattable::style(display = "block",
                                "border-radius" = "4px",
                                "padding-right" = "4px",
                                "font-weight" = "bold",
                                "color" = "white",
                                "background-color" = 
                                  ifelse(x == "High Risk", high_risk_color,
                                         ifelse(x == "Medium Risk", med_risk_color,
                                                ifelse(x == "Low Risk", low_risk_color, "transparent"))))),
            was_decision_made = formattable::formatter("span",
                                          style = x ~ formattable::style(color = ifelse(x, "#0668A3", "gray")),
                                          x ~ formattable::icontext(ifelse(x, "ok", "remove"), ifelse(x, "Yes", "No")))
          )),
        selection = list(mode = 'multiple'),
        colnames = c("Package", "Version", "Score", "Decision Made?", "Decision", "Last Comment", "Explore Metric"),
        rownames = FALSE,
        options = list(
          searching = FALSE,
          lengthChange = FALSE,
          #dom = 'Blftpr',
          pageLength = 15,
          lengthMenu = list(c(15, 60, 120, -1), c('15', '60', '120', "All")),
          columnDefs = list(list(className = 'dt-center', targets = "_all"))
        )
      ) %>%
        DT::formatStyle(names(table_data()), textAlign = 'center')
    })
    
    observeEvent(input$select_button, {
      req(table_data())
      
      selectedRow <- as.numeric(strsplit(input$select_button, "_")[[1]][2])
      
      # grab the package name
      pkg_name <- table_data()[selectedRow, 1]

      parent$userData$flag <- "DB"
      
      # update sidebar-select_pkg
      updateSelectizeInput(
        session = parent,
        inputId = "sidebar-select_pkg",
        choices = c("-", dbSelect('SELECT name FROM package')$name),
        selected = pkg_name
      )
      
      # select maintenance metrics panel
      updateTabsetPanel(session = parent, 
                        inputId = 'tabs', 
                        selected = "Maintenance Metrics"
      )
      
      # jump over to risk-assessment-tab so we can see the maintenance metrics
      updateTabsetPanel(session = parent, 
                        inputId = 'apptabs', 
                        selected = "risk-assessment-tab"
      )
    })
    
    pkgs <- reactive({
      if (is.null(input$packages_table_rows_selected)) {
        character(0)
      } else {
        table_data() %>%
          dplyr::slice(input$packages_table_rows_selected) %>%
          dplyr::pull(name)
      }
    })
    
    mod_downloadHandler_server("downloadHandler", pkgs, user, metric_weights)
  })
}
