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
#' @importFrom archive archive_read archive
#' @importFrom utils capture.output
mod_code_explorer_server <- function(id, selected_pkg, pkgarchive = reactiveVal(), creating_dir = reactiveVal(TRUE), user, credentials){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$func_explorer_ui <- renderUI({
      # Lets the user know that a package needs to be selected.
      if(identical(selected_pkg$name(), character(0))) {
        showHelperMessage()
      } else if (!file.exists(file.path("tarballs", glue::glue("{selected_pkg$name()}_{selected_pkg$version()}.tar.gz")))) {
        showHelperMessage(message = glue::glue("Source code not available for {{{selected_pkg$name()}}}"))
      } else {
        div(introJSUI(NS(id, "introJS")),
 
          fluidRow(style = "height:35px !important;",
                   column(2,offset = 10,
                          conditionalPanel(
                            condition = "typeof(window.$highlights_list) != 'undefined' && window.$highlights_list.length > 1",
                          actionButton(ns("prev_button"),label = "",icon = icon("chevron-left"),
                                       style ="width: 32px !important; 
                            height: 32px !important;
                            font-size: 16px !important;
                            line-height: 5px !important;
                            padding: 0px !important;") |>bslib::tooltip("Previous occurence"), style = "display: inline-block;"),
                          conditionalPanel(
                            condition = "typeof(window.$highlights_list) != 'undefined'  && window.$highlights_list.length > 1",
                          actionButton(ns("next_button"),label = "",icon = icon("chevron-right"),
                                       style = "width: 32px !important; 
                            height: 32px !important;
                            font-size: 16px !important;
                            line-height: 5px !important;
                            padding: 0px !important;")|>bslib::tooltip("Next occurence",placement ="right"), style = "display: inline-block;"))),

 
 
          fluidRow(
            column(3,
                   wellPanel(
                     div(id = ns("function_list"),
                     selectInput(ns("exported_function"), "Exported Function", choices = exported_functions()) %>%
                       tagAppendAttributes(class = "exported_function")),
                     div(id = ns("file_type_div"),
                         selectInput(ns("file_type"), "File Type", choices = c("Testing Files" = "test", "R Source Code" = "source", "Help Documentation" = "man"))),
                     div(id = ns("file_list"),
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
                   )
            ),
            column(9,
                   div(id = ns("file_viewer"),
                     uiOutput(ns("file_output"), class = "file_browser"),
                     style = "height: 62vh; overflow: auto; border: 1px solid var(--bs-border-color-translucent);"
                   ),
                   br(),
                   fluidRow(style = "height:35px !important;",
                            column(4,offset = 8,
                                   conditionalPanel(
                                     condition = "typeof(window.$highlights_list) != 'undefined' && window.$highlights_list.length > 1",
                                     actionButton(ns("prev_button"),label = "",icon = icon("chevron-left"),
                                                  style ="width: 32px !important; 
                            height: 32px !important;
                            font-size: 16px !important;
                            line-height: 5px !important;
                            padding: 0px !important;") |>bslib::tooltip("Previous occurence"), style = "display: inline-block;",
                            
                            div(id = "search_index","",style ="display:inline"),
                            actionButton(ns("next_button"),label = "",icon = icon("chevron-right"),
                                           style = "width: 32px !important; 
                            height: 32px !important;
                            font-size: 16px !important;
                            line-height: 5px !important;
                            padding: 0px !important;
                            display:inline;
                            ")|>bslib::tooltip("Next occurence",placement ="right"), style = "display: inline-block;")))
            )
          ),
          br(), br(),
          div(id = ns("comments_for_fe"), fluidRow(
            if ("general_comment" %in% unlist(credentials$privileges[user$role], use.names = FALSE)) addCommentUI(id = ns("add_comment")),
            viewCommentsUI(id = ns("view_comments"))))
        )
      }
    })
    
    exported_functions <- eventReactive(pkgarchive(), {
      get_exported_functions(pkg_name = selected_pkg$name(), pkg_version = selected_pkg$version())
    })
    
    parse_data <- eventReactive(exported_functions(), {
      
      purrr::map_dfr(
        c("test", "source"),
        ~ get_parse_data(.x, pkgarchive(), pkg_name = selected_pkg$name(), pkg_version = selected_pkg$version(), exported_functions())
      )
    })
    
    test_files <- reactiveVal()
    source_files <- reactiveVal()
    man_files <- reactiveVal()
    observeEvent(input$exported_function, {
      req(input$exported_function)
      test_files(get_files(input$exported_function, "test", parse_data()))
      updateSelectInput(session, "test_files", choices = basename(test_files()),
                        selected = if (!rlang::is_empty(test_files())) basename(test_files())[1] else NULL)
      
      source_files(get_files(input$exported_function, "source", parse_data()))
      updateSelectInput(session, "source_files", choices = basename(source_files()),
                        selected = if (!rlang::is_empty(source_files())) basename(source_files())[1] else NULL)
      
      man_files(get_files(input$exported_function, "man", pkgarchive(), pkg_name = selected_pkg$name(), pkg_version = selected_pkg$version()))
      updateSelectInput(session, "man_files", choices = basename(man_files()),
                        selected = if (!rlang::is_empty(man_files())) basename(man_files())[1] else NULL)
    })
    
    test_code <- reactive({
      if (rlang::is_empty(test_files())) return(HTML("No files to display"))
      req(input$test_files)
      if (file.path(glue::glue("{selected_pkg$name()}"),"tests", "testthat.R") %in% pkgarchive()$path )
        {
        fp <-   file.path(glue::glue("{selected_pkg$name()}"), "tests", "testthat", input$test_files)}
      else 
      { 
        fp <- file.path(glue::glue("{selected_pkg$name()}"), "tests", input$test_files) 
        }
      con <- archive::archive_read(file.path("tarballs",
                                             glue::glue("{selected_pkg$name()}_{selected_pkg$version()}.tar.gz")),
                                   file = fp)
      lines <- readLines(con)
      close(con)
      func_list <- c(input$exported_function, paste0("`", input$exported_function, "`"))
      highlight_index <- parse_data() %>% 
        filter(basename(file) == input$test_files & func %in% func_list) %>% 
        pull(line)
      renderCode(lines, highlight_index)
    }) %>%
      bindEvent(input$test_files, input$exported_function, ignoreNULL = FALSE)
    
    source_code <- reactive({
      if (rlang::is_empty(source_files())) return(HTML("No files to display"))
      req(input$source_files)
      fp <- file.path(glue::glue("{selected_pkg$name()}"), "R", input$source_files) 
      con <- archive::archive_read(file.path("tarballs",
                                             glue::glue("{selected_pkg$name()}_{selected_pkg$version()}.tar.gz")),
                                   file = fp)
      lines <- readLines(con)
      close(con)
      func_list <- c(input$exported_function, paste0("`", input$exported_function, "`"))
      highlight_index <- parse_data() %>% 
        filter(basename(file) ==  input$source_files & func %in% func_list) %>% 
        pull(line)
      renderCode(lines, highlight_index)
    }) %>%
      bindEvent(input$source_files, input$exported_function, ignoreNULL = FALSE)
    
    man_page <- reactive({
      if (rlang::is_empty(man_files())) return(HTML("No files to display"))
      req(input$man_files)
      fp <- file.path(glue::glue("{selected_pkg$name()}"), "man", input$man_files) 
      con <- archive::archive_read(file.path("tarballs",
                                             glue::glue("{selected_pkg$name()}_{selected_pkg$version()}.tar.gz")),
                                   file = fp)
      Rdfile <-tools::parse_Rd(con)
      close(con)
      HTML(paste0(utils::capture.output(tools::Rd2HTML(Rdfile,
                                               package = c(selected_pkg$name(),
                                                           selected_pkg$version()), out = "")), collapse = "\n"))
    }) %>%
      bindEvent(input$man_files, input$exported_function, ignoreNULL = FALSE)
    
    introJSServer("introJS", text = reactive(fe_steps), user, credentials)
    search_index_value <- reactiveVal(1)
    highlight_list <- reactiveVal(1)
    
    observeEvent(input$next_button,{
      if (input$next_button > 0){
      shinyjs::runjs('
       
                    var $index =Array.from($highlights_list).findIndex(node => node.isEqualNode($curr_sel));
                    if( $index == $highlights_list.length -1) 
                    {
              
                          $curr_sel = $highlights_list[0]
                          search_index.innerHTML = 1 + " of " + $highlights_list.length;
              
                    }
                else 
                    {
                          $curr_sel = $highlights_list[$index +1]
                          search_index.innerHTML =  ( $index+2) + " of " + $highlights_list.length;
                    }  
                
                    var $target = document.querySelector("#code_explorer-file_viewer")
                    $target.scrollTop = 0;
 
                    $target.scrollTop =$curr_sel.offsetTop -40; 
                      
                     var $index =Array.from(window.$highlights_list).findIndex(node => node.isEqualNode(window.$gh));
              if( $index == window.$highlights_list.length -1) {
              
              var $gh = window.$highlights_list[0]
              
              }
              else 
              {
              var $gh = window.$highlights_list[$index +1]
              }  
              var $target = document.querySelector("#code_explorer-file_viewer")
        $target.scrollTop = 0;
        $target.scrollTop = $gh.offsetTop  - $target.offsetTop + $target.scrollTop; 
 
                    $target.scrollTop =$curr_sel.offsetTop -40;
                   
 
              ')
      }
      
    })
    
    observeEvent(input$prev_button,{
 
      if (input$prev_button > 0){
        
        shinyjs::runjs('var $index =Array.from($highlights_list).findIndex(node => node.isEqualNode($curr_sel));
                      if( $index ==0) 
                      {
                            $curr_sel = $highlights_list[$highlights_list.length -1]
                            search_index.innerHTML =   $highlights_list.length + " of " + $highlights_list.length;
                      }
                        else 
                      {
                            $curr_sel = $highlights_list[$index -1]
                            search_index.innerHTML =   ($index) + " of " + $highlights_list.length;
                      }  
                      var $target = document.querySelector("#code_explorer-file_viewer")
 
                      $target.scrollTop = 0; # scroll to the top 
                      $target.scrollTop = $curr_sel.offsetTop  - 40; 

              ')
 
                      $target.scrollTop = 0; // scroll to the top 
                      $target.scrollTop = $curr_sel.offsetTop  - 40; 
                     
                      ')
 
      }
      
    })
    output$file_output <- renderUI({
      switch (input$file_type,
              test = test_code(),
              source = source_code(),
              man = man_page()
      )
    })
    
    
    # Call module to create comments and save the output.
    comment_added <- addCommentServer(id = "add_comment",
                                      metric_abrv = 'fe',
                                      user = user,
                                      credentials = credentials,
                                      pkg_name = selected_pkg$name)
    
    comments <- eventReactive(list(comment_added(), selected_pkg$name()), {
      get_fe_comments(selected_pkg$name()) # see utils
    })
    
    # View comments.
    viewCommentsServer(id = "view_comments",
                       comments = comments,
                       pkg_name = selected_pkg$name)
    
  })
}

## To be copied in the UI
# mod_code_explorer_ui("code_explorer_1")

## To be copied in the server
# mod_code_explorer_server("code_explorer_1")
