
#' Community Usage Metrics UI function
#' 
#' @param id a module id name
#' 
#' @keywords internal
#' 
communityMetricsUI <- function(id) {
  uiOutput(NS(id, 'communityMetrics_ui'))
}

#' Community Usage Metrics server logic
#' 
#' @param id a module id name
#' @param selected_pkg placeholder
#' @param community_metrics placeholder
#' @param user placeholder
#' 
#' 
#' @import dplyr
#' @importFrom glue glue
#' @importFrom plotly plotlyOutput renderPlotly
#' 
#' @keywords internal
#' 
communityMetricsServer <- function(id, selected_pkg, community_metrics, user, credentials) {
  if (missing(credentials))
    credentials <- get_credential_config()
  
  moduleServer(id, function(input, output, session) {
    
    # Render Output UI for Community Usage Metrics.
    output$communityMetrics_ui <- renderUI({
      
      vect <- dbSelect("select distinct id from community_usage_metrics") %>% dplyr::pull()
      # Lets the user know that a package needs to be selected.
      if(identical(selected_pkg$name(), character(0)))
        showHelperMessage()
      
      else if(!selected_pkg$name() %in% vect) {
          showHelperMessage(message = glue::glue("Community Usage Metrics not avaiable for {{{selected_pkg$name()}}}"))
      }
      else {
          tagList(
            introJSUI(NS(id, 'introJS')),
            br(), br(),
            div(id = "cum_infoboxes", metricGridUI(NS(id, 'metricGrid'))),
            br(), br(),
            div(id = "cum_plot", fluidRow(
              column(width = 12, style = 'padding-left: 20px; padding-right: 20px;',
                     plotly::plotlyOutput(NS(id, "downloads_plot"), height = "500px")))),
            br(), br(),
            div(id = "comments_for_cum", fluidRow( 
              if ("general_comment" %in% unlist(credentials$privileges[user$role], use.names = FALSE)) addCommentUI(id = session$ns("add_comment")),
              viewCommentsUI(id = session$ns("view_comments"))))
          )
      }
    })
    
    # IntroJS.
    introJSServer(id = "introJS", text = reactive(cum_steps), user, credentials)

    # Community cards (saved to share with report preview): the 
    # time since first release, the time since latest release, 
    # and the number of downloads since last year.
    cards <- eventReactive(community_metrics(), {
      build_comm_cards(community_metrics())
    })
    
    # Create metric grid card.
    metricGridServer(id = 'metricGrid', metrics = cards)
    
    # Call module to create comments and save the output.
    comment_added <- addCommentServer(id = "add_comment",
                                      metric_abrv = 'cum',
                                      user = user,
                                      credentials = credentials,
                                      pkg_name = selected_pkg$name)
    
    comments <- eventReactive(list(comment_added(), selected_pkg$name()), {
      get_cm_comments(selected_pkg$name()) # see utils
    })
    
    # View comments.
    viewCommentsServer(id = "view_comments",
                       comments = comments,
                       pkg_name = selected_pkg$name)
    
    # Data to create downloads plot.
    downloads_plot_data <- reactive({
      build_comm_plotly(community_metrics()) # see utils
    })
    
    output$downloads_plot <- plotly::renderPlotly({
      downloads_plot_data()
    })

    # Return the a reactive element triggered when a comment is added.
    list(
      comment_added = comment_added,
      comments = comments,
      cards = cards,
      downloads_plot_data = downloads_plot_data
    )
  })
}
