# Global Risk color palettes.
# R won't let you build the package if you use the internal data here.
setColorPalette <- colorRampPalette(c('#9CFF94FF', '#B3FF87FF', '#BCFF43FF', '#D8F244FF', '#F2E24BFF', '#FFD070FF', '#FFBE82FF', '#FFA87CFF', '#FF8F6CFF', '#FF765BFF')) # internal data object
# defaults
# setColorPalette(3)[1] # low risk
# setColorPalette(3)[2] # med risk
# setColorPalette(3)[3] # high risk


#' UI for 'Database View' module
#' 
#' @param id a module id name
#' 
#' 
#' @importFrom DT dataTableOutput
#' @importFrom shinyWidgets prettyToggle
#' 
#' @keywords internal
databaseViewUI <- function(id) {
  tagList(
    h2("Database Overview", align = "center", `padding-bottom`="20px"),
    br(),
    fluidRow(
      column(
        width = 10, offset = 1,
    tabsetPanel(
      tabPanel(
        "Uploaded Packages",
        column(
          width = 10, offset = 1, align = "center",
          tags$section(
            div(class = "box",
              div(class = "box-header",
                h3(class = "box-title",
                h3("Uploaded Packages", style = "margin-top: 5px")
                )
              ),
              div(class = "box-body",
                br(),
                metricGridUI(NS(id, 'metricGrid')),
                br(),
                DT::dataTableOutput(NS(id, "packages_table")),
                div(style = "font-size: 25px;", align = "left",
                    shinyWidgets::prettyToggle(NS(id, "dt_sel"), 
                                               label_on  = "All Rows Selected",
                                               label_off = "Select All Rows",
                                               icon_on = icon("check"),
                                               width = "100%",
                                               status_off = "primary",
                                               status_on = "primary",
                                               outline = TRUE,
                                               inline = TRUE,
                                               bigger = TRUE)),
                br(), br(),
                div(id = "dwnld_rp",
                    fluidRow(
                      column(4, h5("Report Configurations"),),
                      column(3, mod_downloadHandler_button_ui(NS(id, "downloadHandler"), multiple = FALSE)),
                      column(3, shiny::actionButton(NS(id, "downloadHandler-store_prefs"), "Store Preferences", 
                                                    icon = icon("fas fa-floppy-disk", class = "fa-reqular", lib = "font-awesome")))
                    ),
                    br(),
                    fluidRow(
                      column(4,
                             mod_downloadHandler_filetype_ui(NS(id, "downloadHandler"))
                      ),
                      column(8, 
                             mod_downloadHandler_include_ui(NS(id, "downloadHandler"))
                      )
                    ),
                ),
              )
            ) %>%
              column(width = 12)
          )
        )),
      tabPanel(
        "Decision Categories",
        column(
          width = 8, offset = 2, align = "center",
          tags$section(
            div(class = "box",
              div(class = "box-header",
                h3(class = "box-title",
                h3("Decision Categories", style = "margin-top: 5px")
                )
              ),
              mod_decision_automation_ui_2("automate") %>%
                div(class = "box-body")
            ) %>%
              column(width = 12)
            )
        ))
    )
  )
  ))
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
#' @importFrom DT dataTableProxy renderDataTable formatStyle selectRows dataTableOutput
#' @importFrom formattable formattable as.datatable formatter style csscolor
#'   icontext
#' @keywords internal
databaseViewServer <- function(id, user, uploaded_pkgs, metric_weights, changes, parent) {
  moduleServer(id, function(input, output, session) {
    
    ns = session$ns
    
    decision_lst <- if (!is.null(golem::get_golem_options("decision_categories"))) golem::get_golem_options("decision_categories") else c("Low Risk", "Medium Risk", "High Risk")
    color_lst <- get_colors(golem::get_golem_options("assessment_db_name"))
    
    # Update table_data if a package has been uploaded
    table_data <- eventReactive({uploaded_pkgs(); changes()}, {
     
      db_pkg_overview <- dbSelect(
        'SELECT pi.name, pi.date_added, pi.version, pi.score, dc.decision, pi.decision_by, pi.decision_date, c.last_comment
        FROM package as pi
        LEFT JOIN (
            SELECT id, max(added_on) as last_comment FROM comments GROUP BY id)
        AS c ON c.id = pi.name
        LEFT JOIN decision_categories as dc
          ON pi.decision_id = dc.id
        ORDER BY 1 DESC'
      )
      
      db_pkg_overview %>%
        dplyr::mutate(date_added = as.Date(date_added)) %>% # new
        dplyr::mutate(score = as.numeric(score)) %>% # new
        
        dplyr::mutate(decision    = if_else(is.na(decision)    | decision    == "", "-", decision)) %>% # keep
        dplyr::mutate(decision = factor(decision)) %>% # new
        
        dplyr::mutate(decision_by = if_else(is.na(decision_by) | decision_by == "", "-", decision_by)) %>% # keep
        dplyr::mutate(decision_by = factor(decision_by)) %>% # new
        
        # dplyr::mutate(decision_date = ifelse(is.na(decision_date) | decision_date == "NA", "-", decision_date)) %>% # old
        dplyr::mutate(decision_date = as.Date(decision_date)) %>% # new
        
        dplyr::mutate(last_comment = lubridate::as_datetime(last_comment)) %>% # new
        # dplyr::mutate(last_comment = as.character(lubridate::as_datetime(last_comment))) %>% # old
        # dplyr::mutate(last_comment = ifelse(is.na(last_comment), "-", last_comment)) %>% # old
        
        dplyr::select(name, date_added, version, score, decision, decision_by, decision_date, last_comment)
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
      build_db_cards(data = table_data())
    })
    
    # Create metric grid cards, containing database stats.
    metricGridServer(id = 'metricGrid', metrics = cards)
    
    tableProxy <- DT::dataTableProxy('packages_table')
    
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
                               onclick = paste0('Shiny.setInputValue(\"' , ns("select_button"), '\", this.id, {priority: \"event\"})')
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
                                "color" = "black",
                                "order" = x,
                                "background-color" = formattable::csscolor(
                                  setColorPalette(100)[round(as.numeric(x)*100)]))),
            decision = formattable::formatter(
              "span",
              style = x ~ formattable::style(display = "block",
                                "border-radius" = "4px",
                                "padding-right" = "4px",
                                "color" = ifelse(x %in% decision_lst, get_text_color(get_colors(golem::get_golem_options("assessment_db_name"))[x]), "inherit"),
                                "background-color" = 
                                  ifelse(x %in% decision_lst,
                                         glue::glue("var(--{risk_lbl(x, type = 'attribute')}-color)"),
                                         "transparent")))
          )),
        selection = list(mode = 'multiple'),
        colnames = c("Package", "Date Uploaded", "Version", "Score", "Decision", "Decision by", "Decision Date", "Last Comment", "Explore Metrics"),
        rownames = FALSE,
        filter = list(
          position = "top",
          plain = TRUE
        ),
        extensions = "Buttons",
        options = list(
          searching = TRUE,
          lengthChange = FALSE,
          dom = 'Blftpr',
          pageLength = 15,
          lengthMenu = list(c(15, 60, 120, -1), c('15', '60', '120', "All")),
          columnDefs = list(
            list(className = 'dt-center', targets = "_all"),
            list(targets = 8, searchable = FALSE) # make sure 'Explore Metrics' column filter is disabled
            ),
          buttons = list(
            list(extend = "excel", text = shiny::HTML('<i class="fas fa-download"></i> Excel'),
                 exportOptions = list(columns = c(0:6)), # which columns to download
                 filename = paste("{riskassessment} pkgs " ,stringr::str_replace_all(paste(get_time()),":", "."))),
            list(extend = "csv", text = shiny::HTML('<i class="fas fa-download"></i> CSV'),
                 exportOptions = list(columns = c(0:6)), # which columns to download
                 filename = paste("{riskassessment} pkgs " ,stringr::str_replace_all(paste(get_time()),":", "."))))
        )
        , style="default"
      ) %>%
        DT::formatStyle(names(table_data()), textAlign = 'center')
    })
    
    
    observeEvent(input$dt_sel, {
      if (isTRUE(input$dt_sel)) {
        DT::selectRows(tableProxy, input$packages_table_rows_all)
      } else {
        DT::selectRows(tableProxy, NULL)
      }
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
      
      updateSelectInput(session = parent, 
                        inputId = 'metric_type', 
                        selected = "mm"
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
    
    # return vector of elements to include in the report
    report_includes <- mod_downloadHandler_include_server("downloadHandler")

    mod_downloadHandler_server("downloadHandler", pkgs, user, metric_weights)
    
  })
}
