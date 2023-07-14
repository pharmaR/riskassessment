#' pkg_explorer UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom jsTreeR jstreeOutput
#' @importFrom shinyAce aceEditor
mod_pkg_explorer_ui <- function(id){
  ns <- NS(id)
  tagList(
    h4("file browser"),
    fluidRow(
      column(4,
             wellPanel(
               jsTreeR::jstreeOutput(ns("dirtree")),
             )
      ),
      column(8,
             conditionalPanel(
               condition = "output.is_child",
               shinyAce::aceEditor(ns("editor"), value = "", height = "600px",
                                   mode = "txt", readOnly = TRUE, theme = "tomorrow",
                                   fontSize = 14, wordWrap = FALSE, showLineNumbers = FALSE,
                                   highlightActiveLine = TRUE, tabSize = 2, showInvisibles = FALSE
               ),
               htmlOutput(ns("filepath")),
               ns = ns
             )
      )
    )
  )
}

#' pkg_explorer Server Functions
#' 
#' @importFrom jsTreeR renderJstree jstree
#' @importFrom shinyAce updateAceEditor
#'
#' @noRd 
mod_pkg_explorer_server <- function(id, pkgdir, accepted_extensions = c("r", "rmd", "rd", "txt", "md","csv", "tsv", "json", "xml", "yaml", "yml", "dcf", "html", "js", "css", "c", "cpp", "h", "java", "scala", "py", "perl", "sh", "sql"), accepted_filenames = c("DESCRIPTION", "NAMESPACE", "LICENSE", "LICENSE.note", "NEWS", "README", "CHANGES", "MD5")) {
  if (!is.reactive(pkgdir))
    pkgdir <- reactiveVal(pkgdir)
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # Necessary addition for {jsTree} to work without attaching the package.
    # I anticipate that the package developer will update the package to work
    # on load. When that occurs, I will update the package and remove these
    # lines of code.
    shiny::registerInputHandler("jsTreeR.list", function(data, ...){
      data
    }, force = TRUE)
    shiny::registerInputHandler("jsTreeR.move", function(data, ...){
      lapply(data, unlist)
    }, force = TRUE)
    shiny::registerInputHandler("jsTreeR.copied", function(data, ...){
      data[["from"]][["path"]] <- unlist(data[["from"]][["path"]])
      data[["to"]][["path"]] <- unlist(data[["to"]][["path"]])
      data
    }, force = TRUE)
    shiny::registerInputHandler("jsTreeR.path", function(data, ...){
      data[["path"]] <- unlist(data[["path"]])
      data
    }, force = TRUE)
    
    nodes <- reactive(makeNodes(list.files(pkgdir(), recursive = TRUE, include.dirs = TRUE)))
    
    types <- list(
      root = list(icon = "fa fa-folder"),
      child = list(icon = "fa fa-file")
    )
    
    output$dirtree <- jsTreeR::renderJstree({
      jsTreeR::jstree(nodes(), types = types, multiple = FALSE, theme = "proton")
    })
    
    output$is_child <- reactive({
      return(length(input$dirtree_selected) > 0 && input$dirtree_selected[[1]]$type == "child")
    })
    outputOptions(output, "is_child", suspendWhenHidden = FALSE)
    
    observeEvent(input$dirtree_selected, {
      s <- ""
      e <- "txt"
      if (length(input$dirtree_selected) > 0 && input$dirtree_selected[[1]]$type == "child") {
        filename <- input$dirtree_selected[[1]]$text
        filepath <- input$dirtree_selected[[1]]$data
        e <- tolower(tools::file_ext(filepath))
        if (e %in% accepted_extensions || filename %in% accepted_filenames) {
          s <- readLines(file.path(pkgdir(), filepath))
          s <- paste(s, collapse = "\n")
        } else {
          s <- "file format not supported"
          e <- "txt"
        }
      }
      shinyAce::updateAceEditor(session, "editor", value = s, mode = e)
    })
    
    output$filepath <- renderUI({
      s <- if (length(input$dirtree_selected) > 0 && input$dirtree_selected[[1]]$type == "child")
        input$dirtree_selected[[1]]$data else ""
      HTML(sprintf('<h5>%s</h5>', s))
    }) %>%
      bindEvent(input$dirtree_selected)
    
  })
}

## To be copied in the UI
# mod_pkg_explorer_ui("pkg_explorer")

## To be copied in the server
# mod_pkg_explorer_server("pkg_explorer")
