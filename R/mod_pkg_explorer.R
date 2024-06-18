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
#' @importFrom archive archive_read archive 
#'
#' @noRd 
mod_pkg_explorer_server <- function(id, selected_pkg,
                                    accepted_extensions = c("r", "rmd", "rd", "txt", "md","csv", "tsv", "json", "xml", "yaml", "yml", "dcf", "html", "js", "css", "c", "cpp", "h", "java", "scala", "py", "perl", "sh", "sql"),
                                    accepted_filenames = c("DESCRIPTION", "NAMESPACE", "LICENSE", "LICENSE.note", "NEWS", "README", "CHANGES", "MD5"),
                                    pkgarchive = reactiveVal(),
                                    creating_dir = reactiveVal(TRUE),
                                    user, credentials) {
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$pkg_explorer_ui <- renderUI({
      
      # Lets the user know that a package needs to be selected.
      if(identical(selected_pkg$name(), character(0))) {
        showHelperMessage()
      } else if (!file.exists(file.path("tarballs", glue::glue("{selected_pkg$name()}_{selected_pkg$version()}.tar.gz")))) {
        showHelperMessage(message = glue::glue("Source code not available for {{{selected_pkg$name()}}}"))
      } else {
        div(introJSUI(NS(id, "introJS")),
            br(),
            fluidRow(
              column(4,
                     div(id = ns("file_tree"),
                     wellPanel(
                       {
                         treeTag <- 
                           shinyTree::shinyTree(ns("dirtree"), theme = "proton", types = '{"default":{"icon":"fa fa-folder"},"file":{"icon":"fa fa-file"}}')
                         treeTag[[1]]$children[[3]] <- NULL
                         treeTag
                       }
                     ))
              ),
              column(8,
                     div(id = ns("file_editor"),
                     conditionalPanel(
                       condition = "output.is_file",
                       shinyAce::aceEditor(ns("editor"), value = "", height = "62vh",
                                           mode = "txt", readOnly = TRUE, theme = "tomorrow",
                                           fontSize = 14, wordWrap = FALSE, showLineNumbers = FALSE,
                                           highlightActiveLine = TRUE, tabSize = 2, showInvisibles = FALSE
                       ),
                       htmlOutput(ns("filepath")),
                       ns = ns
                     ))
              )
            ),
            br(), br(),
            div(id = ns("comments_for_se"), fluidRow(
              if ("general_comment" %in% unlist(credentials$privileges[user$role], use.names = FALSE)) addCommentUI(id = ns("add_comment")),
              viewCommentsUI(id = ns("view_comments")))),
          id = id
        )
      }
    })
    
    observe({
      shinyjs::addClass(id, class = "jstree-disable", asis = TRUE)
      session$onFlushed(function() {
        shinyjs::removeClass(id, class = "jstree-disable", asis = TRUE)
      })
    }, priority = 100) %>%
      bindEvent(selected_pkg$name(), creating_dir())

    nodes <- reactive({
      req(pkgarchive())
      s <-  pkgarchive() %>%
        filter(size > 0) %>%
        filter(grepl("/", path))  %>%
       dplyr::pull(path) %>%
      make_nodes() %>%
        .[[1]]
      if(!is.null(s[["DESCRIPTION"]])){
      attr(s[["DESCRIPTION"]],"stselected") = TRUE
      }
      else {
        f <- names(head(purrr::keep(s, \(x) !is.null(attr(x, "sttype"))), 1))
        attr(s[[f]],"stselected") = TRUE
      }
      
      s
    }) %>%
      bindEvent(pkgarchive (), selected_pkg$name())
    
    output$dirtree <- shinyTree::renderTree(nodes())
    
    observeEvent(nodes(), {
      shinyjs::runjs(paste0('Shiny.setInputValue(\"', ns("dirtree"), '\", null)'))
    })
    
    is_file <- reactive({
      length(input$dirtree) > 0 && length(shinyTree::get_selected(input$dirtree, "slices")) > 0 && isTRUE(attr(get_list_element(shinyTree::get_selected(input$dirtree, "slices")[[1]], isolate(nodes())), "sttype") == "file")
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
          con <- archive::archive_read(file.path("tarballs",
                                                 glue::glue("{selected_pkg$name()}_{selected_pkg$version()}.tar.gz")),
                                       file = glue::glue("{selected_pkg$name()}/{filepath}"))
          s <- readLines(con)
          close(con)
          s <- paste(s, collapse = "\n")
        } else {
          s <- "file format not supported"
          e <- "txt"
        }
      }
      shinyAce::updateAceEditor(session, "editor", value = s, mode = e)
    })
    
    introJSServer("introJS", text = reactive(pe_steps), user, credentials)
    
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
