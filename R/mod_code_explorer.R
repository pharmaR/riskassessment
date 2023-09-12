#' code_explorer UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_code_explorer_ui <- function(id){
  ns <- NS(id)
  uiOutput(ns("func_explorer_ui"))
}

#' code_explorer Server Functions
#'
#' @noRd 
#' 
#' @importFrom tools Rd2HTML
#' @importFrom purrr map_dfr
mod_code_explorer_server <- function(id, selected_pkg, pkgdir = reactiveVal(), creating_dir = reactiveVal(TRUE), user, credentials){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$func_explorer_ui <- renderUI({
      # Lets the user know that a package needs to be selected.
      if(identical(selected_pkg$name(), character(0))) {
        showHelperMessage()
      } else if (!file.exists(file.path("tarballs", glue::glue("{selected_pkg$name()}_{selected_pkg$version()}.tar.gz")))) {
        showHelperMessage(message = glue::glue("Source code not available for {{{selected_pkg$name()}}}"))
      } else {
        div(
          br(),
          fluidRow(
            column(3,
                   wellPanel(
                     selectInput(ns("exported_function"), "Exported Function", choices = exported_functions()) %>%
                       tagAppendAttributes(class = "exported_function"),
                     selectInput(ns("file_type"), "File Type", choices = c("Testing Files" = "test", "R Source Code" = "source", "Help Documentation" = "man")),
                     conditionalPanel(
                       condition = "input.file_type == 'test'",
                       selectInput(ns("test_files"), NULL,
                                   choices = NULL, selectize = FALSE, size = 12
                       ),
                       ns = ns
                     ),
                     conditionalPanel(
                       condition = "input.file_type == 'source'",
                       selectInput(ns("source_files"), NULL,
                                   choices = NULL, selectize = FALSE, size = 12
                       ),
                       ns = ns
                     ),
                     conditionalPanel(
                       condition = "input.file_type == 'man'",
                       selectInput(ns("man_files"), NULL,
                                   choices = NULL, selectize = FALSE, size = 12
                       ),
                       ns = ns
                     )
                   )
            ),
            column(9,
                   div(
                     uiOutput(ns("file_output"), class = "file_browser"),
                     style = "height: 62vh; overflow: auto; border: 1px solid var(--bs-border-color-translucent);"
                   )
            )
          )
        )
      }
    })
    
    exported_functions <- eventReactive(pkgdir(), {
      get_exported_functions(pkgdir())
    })
    
    parse_data <- eventReactive(exported_functions(), {
      purrr::map_dfr(
        c("test", "source"),
        ~ get_parse_data(.x, pkgdir(), exported_functions())
      )
    })
    
    test_files <- reactiveVal()
    source_files <- reactiveVal()
    man_files <- reactiveVal()
    observeEvent(input$exported_function, {
      req(input$exported_function)
      test_files(get_files(input$exported_function, "test", parse_data()))
      updateSelectInput(session, "test_files", choices = test_files(),
                        selected = if (!rlang::is_empty(test_files())) test_files()[1] else NULL)
      
      source_files(get_files(input$exported_function, "source", parse_data()))
      updateSelectInput(session, "source_files", choices = source_files(),
                        selected = if (!rlang::is_empty(source_files())) source_files()[1] else NULL)
      
      man_files(get_files(input$exported_function, "man", pkgdir()))
      updateSelectInput(session, "man_files", choices = man_files(),
                        selected = if (!rlang::is_empty(man_files())) man_files()[1] else NULL)
    })
    
    test_code <- reactive({
      if (rlang::is_empty(test_files())) return(HTML("No files to display"))
      req(input$test_files)
      lines <- readLines(file.path(pkgdir(), "tests", "testthat", input$test_files))
      highlight_index <- parse_data() %>% 
        filter(file == input$test_files & func == input$exported_function) %>% 
        pull(line)
      renderCode(lines, highlight_index)
    }) %>%
      bindEvent(input$test_files, input$exported_function, ignoreNULL = FALSE)
    
    source_code <- reactive({
      if (rlang::is_empty(source_files())) return(HTML("No files to display"))
      req(input$source_files)
      lines <- readLines(file.path(pkgdir(), "R", input$source_files))
      highlight_index <- parse_data() %>% 
        filter(file == input$source_files & func %in% c(input$exported_function, gsub("^(\\%.*\\%)$", "`\\1`", input$exported_function))) %>% 
        pull(line)
      renderCode(lines, highlight_index)
    }) %>%
      bindEvent(input$source_files, input$exported_function, ignoreNULL = FALSE)
    
    man_page <- reactive({
      if (rlang::is_empty(man_files())) return(HTML("No files to display"))
      req(input$man_files)
      out_dir <- tempdir()
      tools::Rd2HTML(file.path(pkgdir(), "man", input$man_files), package = selected_pkg$name(), out = file.path(out_dir, "man.html"))
      includeHTML(file.path(out_dir, "man.html"))
    }) %>%
      bindEvent(input$man_files, input$exported_function, ignoreNULL = FALSE)
    
    output$file_output <- renderUI({
      switch (input$file_type,
              test = test_code(),
              source = source_code(),
              man = man_page()
      )
    })
    
  })
}

## To be copied in the UI
# mod_code_explorer_ui("code_explorer_1")

## To be copied in the server
# mod_code_explorer_server("code_explorer_1")
