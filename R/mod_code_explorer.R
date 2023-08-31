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
  fluidRow(
    column(3,
           wellPanel(
             selectInput(ns("exported_function"), "exported function", choices = NULL),
             selectInput(ns("file_type"), "File Type", choices = c("Test Code" = "test", "Source Code" = "source", "Man Page" = "man")),
             checkboxInput(ns("always_show_files"), "always show files", value = FALSE),
             conditionalPanel(
               condition = "input.file_type == 'test'",
               selectInput(ns("test_files"), "test files",
                           choices = NULL, selectize = FALSE, size = 15
               ),
               ns = ns
             ),
             conditionalPanel(
               condition = "input.file_type == 'source' & (output.has_several_source_files | input.always_show_files)",
               selectInput(ns("source_files"), "source files",
                           choices = NULL, selectize = FALSE, size = 3
               ),
               ns = ns
             ),
             conditionalPanel(
               condition = "input.file_type == 'man' & (output.has_several_man_files | input.always_show_files)",
               selectInput(ns("man_files"), "man files",
                           choices = NULL, selectize = FALSE, size = 3
               ),
               ns = ns
             )
           )
    ),
    column(9,
           div(
             conditionalPanel(
               condition = "input.file_type == 'test'",
               htmlOutput(ns("test_code")),
               ns = ns
             ),
             conditionalPanel(
               condition = "input.file_type == 'source'",
               htmlOutput(ns("source_code")),
               ns = ns
             ),
             conditionalPanel(
               condition = "input.file_type == 'man'",
               htmlOutput(ns("man_page")),
               ns = ns
             ),
             style = "height: 62vh"
           )
    )
  )
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
    
    exported_functions <- eventReactive(pkgdir(), {
      get_exported_functions(pkgdir())
    })
    
    observeEvent(exported_functions(), {
      updateSelectInput(session, "exported_function", choices = exported_functions())
    })
    
    parse_data <- eventReactive(exported_functions(), {
      purrr::map_dfr(
        c("test", "source"),
        ~ get_parse_data(.x, pkgdir(), exported_functions())
      )
    })
    
    source_files <- reactiveVal()
    man_files <- reactiveVal()
    observeEvent(input$exported_function, {
      req(input$exported_function)
      test_files <- get_files(input$exported_function, "test", parse_data())
      updateSelectInput(session, "test_files", choices = test_files,
                        selected = if (!rlang::is_empty(test_files)) test_files[1] else NULL)
      
      source_files(get_files(input$exported_function, "source", parse_data()))
      updateSelectInput(session, "source_files", choices = source_files(),
                        selected = if (!rlang::is_empty(source_files())) source_files()[1] else NULL)
      
      man_files(get_files(input$exported_function, "man", pkgdir()))
      updateSelectInput(session, "man_files", choices = man_files(),
                        selected = if (!rlang::is_empty(man_files())) man_files()[1] else NULL)
    })
    
    output$has_several_source_files <- reactive({
      return(length(source_files()) > 1)
    })
    outputOptions(output, "has_several_source_files", suspendWhenHidden = FALSE)
    
    output$has_several_man_files <- reactive({
      return(length(man_files()) > 1)
    })
    outputOptions(output, "has_several_man_files", suspendWhenHidden = FALSE)
    
    renderCode <- function(lines, hlindex) {
      tags$table(class = "code-table",
                 tags$tbody(
                   lapply(seq_along(lines), function(i) {
                     tags$tr(class = if (i %in% hlindex) "highlight" else "plain",
                             tags$td(class = "number", i),
                             tags$td(class = "code", tags$pre(class = "language-r", lines[i]))
                     )
                   })
                 ),
                 tags$script(HTML("
        document.querySelectorAll('.code pre').forEach(bl => {
          hljs.highlightBlock(bl);
        }); 
      "))
      )
    }
    
    output$test_code <- renderUI({
      req(input$test_files)
      lines <- readLines(file.path(pkgdir(), "tests", "testthat", input$test_files))
      highlight_index <- parse_data() %>% 
        filter(file == input$test_files & func == input$exported_function) %>% 
        pull(line)
      renderCode(lines, highlight_index)
    })
    
    output$source_code <- renderUI({
      req(input$source_files)
      lines <- readLines(file.path(pkgdir(), "R", input$source_files))
      highlight_index <- parse_data() %>% 
        filter(file == input$source_files & func == input$exported_function) %>% 
        pull(line)
      renderCode(lines, highlight_index)
    })
    
    output$man_page <- renderUI({
      req(input$man_files)
      out_dir <- tempdir()
      tools::Rd2HTML(file.path(pkgdir(), "man", input$man_files), out = file.path(out_dir, "man.html"))
      HTML(paste(readLines(file.path(out_dir, "man.html")), collapse = "\n"))
    })
    
  })
}

## To be copied in the UI
# mod_code_explorer_ui("code_explorer_1")

## To be copied in the server
# mod_code_explorer_server("code_explorer_1")
