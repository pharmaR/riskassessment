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
  div(
    conditionalPanel(
        condition = "output.show_tree",
            br(),
        introJSUI(NS(id, "introJS")),
            h4("File Browser", style = "text-align: center;"),
            br(), br(),
            fluidRow(
                column(4,
                       div(
                         id = "file_explorer_jstree",
                   wellPanel(
                     jsTreeR::jstreeOutput(ns("dirtree"))
                   ))
                ),
                column(8,
                 conditionalPanel(
                   condition = "output.is_child",
                   div(id = "file_explorer_viewer",
                   shinyAce::aceEditor(ns("editor"), value = "", height = "62vh",
                                       mode = "txt", readOnly = TRUE, theme = "tomorrow",
                                       fontSize = 14, wordWrap = FALSE, showLineNumbers = FALSE,
                                       highlightActiveLine = TRUE, tabSize = 2, showInvisibles = FALSE
                                                           )),
                   htmlOutput(ns("filepath")),
                   ns = ns
                 )
               )
              ),
            br(), br(),
        div(id = "file_explorer_comments",
            uiOutput(ns("comments_for_se"))),
        ns = ns
      ),
    uiOutput(ns("pkg_explorer_ui")),
    id = id
  )
}

#' pkg_explorer Server Functions
#' 
#' @importFrom jsTreeR renderJstree jstree jstreeUpdate
#' @importFrom shinyAce updateAceEditor
#' @importFrom utils untar
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
      }
    })
    
    output$comments_for_se <- renderUI({
      fluidRow( 
        if ("general_comment" %in% credentials$privileges[[user$role]]) addCommentUI(id = ns("add_comment")),
        viewCommentsUI(id = ns("view_comments")))
    })
    
    output$show_tree <- reactive({
      !identical(selected_pkg$name(), character(0)) && file.exists(file.path("tarballs", glue::glue("{selected_pkg$name()}_{selected_pkg$version()}.tar.gz")))
    })
    outputOptions(output, "show_tree", suspendWhenHidden = FALSE)
    
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
        utils::untar(file.path("tarballs", glue::glue("{selected_pkg$name()}_{selected_pkg$version()}.tar.gz")), exdir = "source")
        pkgdir(src_dir)
      }
    }) %>% 
      bindEvent(selected_pkg$name(), create_dir())
    
    nodes <- reactive({
      req(pkgdir())
      makeNodes(list.files(pkgdir(), recursive = TRUE, include.dirs = TRUE))
    })
    
    types <- list(
      root = list(icon = "fa fa-folder"),
      child = list(icon = "fa fa-file")
    )
    
    output$dirtree <- jsTreeR::renderJstree({
      jsTreeR::jstree(isolate(nodes()), types = types, multiple = FALSE, theme = "proton")
    })
    
    observeEvent(nodes(), {
      jsTreeR::jstreeUpdate(session, ns("dirtree"), nodes())
    }, ignoreInit = TRUE)
    
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
    
    introJSServer("introJS", text = reactive(package_explorer_steps), user, credentials)
    
    output$filepath <- renderUI({
      s <- if (length(input$dirtree_selected) > 0 && input$dirtree_selected[[1]]$type == "child")
        input$dirtree_selected[[1]]$data else ""
      HTML(sprintf('<h5>%s</h5>', s))
    }) %>%
      bindEvent(input$dirtree_selected)
    
    
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
