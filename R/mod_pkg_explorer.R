#' pkg_explorer UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom shinyAce aceEditor
mod_pkg_explorer_ui <- function(id){
  ns <- NS(id)
  uiOutput(ns("pkg_explorer_ui"))
}

#' pkg_explorer Server Functions
#' 
#' @importFrom shinyAce updateAceEditor
#' @importFrom utils untar
#' @importFrom shinyTree shinyTree renderTree updateTree get_selected
#'
#' @noRd 
mod_pkg_explorer_server <- function(id, selected_pkg,
                                    accepted_extensions = c("r", "rmd", "rd", "txt", "md","csv", "tsv", "json", "xml", "yaml", "yml", "dcf", "html", "js", "css", "c", "cpp", "h", "java", "scala", "py", "perl", "sh", "sql"),
                                    accepted_filenames = c("DESCRIPTION", "NAMESPACE", "LICENSE", "LICENSE.note", "NEWS", "README", "CHANGES", "MD5"),
                                    create_dir = reactiveVal(TRUE),
                                    user, credentials) {
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    pkgdir <- reactiveVal()
    
    output$pkg_explorer_ui <- renderUI({
      
      
      # Lets the user know that a package needs to be selected.
      if(identical(selected_pkg$name(), character(0))) {
        showHelperMessage()
      } else if (!file.exists(file.path("tarballs", glue::glue("{selected_pkg$name()}_{selected_pkg$version()}.tar.gz")))) {
        showHelperMessage(message = glue::glue("Source code not available for {{{selected_pkg$name()}}}"))
      } else {
        div(
            br(),
            h4("File Browser", style = "text-align: center;"),
            br(), br(),
            fluidRow(
              column(4,
                     wellPanel(
                       {
                         treeTag <- 
                           shinyTree::shinyTree(ns("dirtree"), theme = "proton", types = '{"default":{"icon":"fa fa-folder"},"file":{"icon":"fa fa-file"}}')
                         treeTag[[1]]$children[[3]] <- NULL
                         treeTag
                       }
                     )
              ),
              column(8,
                     conditionalPanel(
                       condition = "output.is_file",
                       shinyAce::aceEditor(ns("editor"), value = "", height = "62vh",
                                           mode = "txt", readOnly = TRUE, theme = "tomorrow",
                                           fontSize = 14, wordWrap = FALSE, showLineNumbers = FALSE,
                                           highlightActiveLine = TRUE, tabSize = 2, showInvisibles = FALSE
                       ),
                       htmlOutput(ns("filepath")),
                       ns = ns
                     )
              )
            ),
            br(), br(),
            div(id = ns("comments_for_se"), fluidRow(
              if ("general_comment" %in% credentials$privileges[[user$role]]) addCommentUI(id = ns("add_comment")),
              viewCommentsUI(id = ns("view_comments")))),
          id = id
        )
      }
    })
    
    observe({
      req(selected_pkg$name() != "-")
      req(create_dir())
      req(file.exists(file.path("tarballs", glue::glue("{selected_pkg$name()}_{selected_pkg$version()}.tar.gz"))))
      
      shinyjs::addClass(id, class = "jstree-disable", asis = TRUE)
      session$onFlushed(function() {
        shinyjs::removeClass(id, class = "jstree-disable", asis = TRUE)
      })
      src_dir <- file.path("source", selected_pkg$name())
      if (dir.exists(src_dir)) {
        pkgdir(src_dir)
      } else {
        withProgress(
          utils::untar(file.path("tarballs", glue::glue("{selected_pkg$name()}_{selected_pkg$version()}.tar.gz")), exdir = "source"),
          message = glue::glue("Unpacking {selected_pkg$name()}_{selected_pkg$version()}.tar.gz"),
          value = 1
        )
        pkgdir(src_dir)
      }
    }) %>% 
      bindEvent(selected_pkg$name(), create_dir())
    
    nodes <- reactive({
      req(pkgdir())
      make_nodes(list.files(pkgdir(), recursive = TRUE))
    })
    
    output$dirtree <- shinyTree::renderTree(isolate(nodes()))
    observeEvent(nodes(), {
      shinyTree::updateTree(session, "dirtree", nodes())
    })
    
    is_file <- reactive({
      length(input$dirtree) > 0 && isTRUE(attr(get_list_element(shinyTree::get_selected(input$dirtree, "slices")[[1]], isolate(nodes())), "sttype") == "file")
    })
    output$is_file <- is_file
    outputOptions(output, "is_file", suspendWhenHidden = FALSE)
    
    observeEvent(input$dirtree, {
      s <- ""
      e <- "txt"
      if (is_file()) {
        filepath <- get_selected_path(shinyTree::get_selected(input$dirtree, "slices")[[1]])
        filename <- basename(filepath)
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
      s <- if (is_file())
        get_selected_path(shinyTree::get_selected(input$dirtree, "slices")[[1]]) else ""
      HTML(sprintf('<h5>%s</h5>', s))
    }) %>%
      bindEvent(input$dirtree)
    
    
    # Call module to create comments and save the output.
    comment_added <- addCommentServer(id = "add_comment",
                                      metric_abrv = 'se',
                                      user = user,
                                      credentials = credentials,
                                      pkg_name = selected_pkg$name)
    
    comments <- eventReactive(list(comment_added(), selected_pkg$name()), {
      get_se_comments(selected_pkg$name()) # see utils
    })
    
    # View comments.
    viewCommentsServer(id = "view_comments",
                       comments = comments,
                       pkg_name = selected_pkg$name)
    
    
  })
}

## To be copied in the UI
# mod_pkg_explorer_ui("pkg_explorer")

## To be copied in the server
# mod_pkg_explorer_server("pkg_explorer")
