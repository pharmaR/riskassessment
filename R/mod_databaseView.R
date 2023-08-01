# Global Risk color palettes.
# run locally and paste hex codes
# colorspace::darken(viridisLite::turbo(11, begin = 0.4, end = .8225), .25)
setColorPalette <- colorRampPalette(c("#06B756FF","#2FBC06FF","#67BA04FF","#81B50AFF","#96AB0AFF","#A99D04FF","#B78D07FF","#BE7900FF","#BE6200FF","#B24F22FF","#A63E24FF"))


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
  tagList(
    h2("Database Overview", align = "center", `padding-bottom`="20px"),
    br(),
    tabsetPanel(
      tabPanel(
        "Uploaded Packages",
        column(
          width = 8, offset = 2, align = "center",
          tags$section(
            shinydashboard::box(width = 12,
                                title = h3("Uploaded Packages", style = "margin-top: 5px"),
                                br(),
                                metricGridUI(NS(id, 'metricGrid')),
                                DT::dataTableOutput(NS(id, "packages_table")),
                                br(),
                                h5("Report Configurations"),
                                br(),
                                fluidRow(
                                  column(5,
                                         mod_downloadHandler_filetype_ui(NS(id, "downloadHandler")),
                                         mod_downloadHandler_button_ui(NS(id, "downloadHandler"), multiple = FALSE)
                                  ),
                                  column(7, 
                                         mod_downloadHandler_include_ui(NS(id, "downloadHandler"))
                                  )
                                )))
        )),
      tabPanel(
        "Decision Categories",
        column(
          width = 8, offset = 2, align = "center",
          tags$section(
            shinydashboard::box(width = 12,
                                title = h3("Decision Categories", style = "margin-top: 5px"),
                                mod_decision_automation_ui_2("automate")
                                ))
      ))
    )
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
    
    decision_lst <- if (!is.null(golem::get_golem_options("decision_categories"))) golem::get_golem_options("decision_categories") else c("Low Risk", "Medium Risk", "High Risk")
    color_lst <- get_colors(golem::get_golem_options("assessment_db_name"))
    
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
        'SELECT pi.name, pi.version, pi.date_added, pi.score, dc.decision, pi.decision_by, pi.decision_date, c.last_comment
        FROM package as pi
        LEFT JOIN (
            SELECT id, max(added_on) as last_comment FROM comments GROUP BY id)
        AS c ON c.id = pi.name
        LEFT JOIN decision_categories as dc
          ON pi.decision_id = dc.id
        ORDER BY 1 DESC'
      )
      
      db_pkg_overview %>%
        dplyr::mutate(last_comment = as.character(lubridate::as_datetime(last_comment))) %>%
        dplyr::mutate(last_comment = ifelse(is.na(last_comment), "-", last_comment)) %>%
        dplyr::mutate(decision    = if_else(is.na(decision)    | decision    == "", "-", decision)) %>%
        dplyr::mutate(decision_by = if_else(is.na(decision_by) | decision_by == "", "-", decision_by)) %>% 
        dplyr::mutate(decision_date = ifelse(is.na(decision_date) | decision_date == "NA", "-", decision_date)) %>% 
        dplyr::select(name, version, date_added, score, decision, decision_by, decision_date, last_comment)
    })
    
    exportTestValues(
      table_data = {
        table_data()
      },
      pkgs = {
        pkgs()
      }
    )
    
    # Database cards (saved to share with db report): Package Count,
    #   Count (%) by Decision made, Count (%) by Decision
    cards <- eventReactive(table_data(), {
      build_db_cards(data = table_data() %>% mutate(decision = factor(decision, levels = decision_lst)))
    })
    
    # Create metric grid cards, containing database stats.
    metricGridServer(id = 'metricGrid', metrics = cards)
    
    # Create table for the db dashboard.
    output$packages_table <- DT::renderDataTable(server = FALSE, {  # This allows for downloading entire data set
      
      my_data_table <- reactive({
        cbind(table_data(), 
        data.frame(
          Actions = shinyInput(actionButton, nrow(table_data()),
                               'button_',
                               size = "xs",
                               style='height:24px; padding-top:1px;',
                               label = icon("arrow-right", class="fa-regular", lib = "font-awesome"),
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
                                "color" = ifelse(x %in% decision_lst, "white", "inherit"),
                                "background-color" = 
                                  ifelse(x %in% decision_lst, 
                                         glue::glue("var(--{risk_lbl(x, input = FALSE)}-color)"), 
                                         "transparent")))
          )),
        selection = list(mode = 'multiple'),
        colnames = c("Package", "Version", "Date Uploaded", "Score", "Decision", "Decision by", "Decision Date", "Last Comment", "Explore Metrics"),
        rownames = FALSE,
        extensions = "Buttons",
        options = list(
          searching = TRUE,
          lengthChange = FALSE,
          dom = 'Blftpr',
          pageLength = 15,
          lengthMenu = list(c(15, 60, 120, -1), c('15', '60', '120', "All")),
          columnDefs = list(list(className = 'dt-center', targets = "_all")),
          buttons = list(
            list(extend = "excel", text = shiny::HTML('<i class="fas fa-download"></i> Excel'),
                 exportOptions = list(columns = c(0:6)), # which columns to download
                 filename = paste("{riskassessment} pkgs " ,stringr::str_replace_all(paste(Sys.time()),":", "."))),
            list(extend = "csv", text = shiny::HTML('<i class="fas fa-download"></i> CSV'),
                 exportOptions = list(columns = c(0:6)), # which columns to download
                 filename = paste("{riskassessment} pkgs " ,stringr::str_replace_all(paste(Sys.time()),":", "."))))
        )
        , style="default"
      ) %>%
        DT::formatStyle(names(table_data()), textAlign = 'center')
    })
    
    observeEvent(input$select_button, {
      req(table_data())
      
      selectedRow <- as.numeric(strsplit(input$select_button, "_")[[1]][2])
      
      # grab the package name
      pkg_name <- table_data()[selectedRow, 1]

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
                        selected = "Package Metrics"
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
