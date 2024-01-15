#' downloadHandler UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
mod_downloadHandler_button_ui <- function(id, multiple = TRUE){
  ns <- NS(id)
  tagList(
    downloadButton(ns("download_reports"), if (multiple) "Download Report(s)" else "Download Report")
  )
}

#' downloadHandler UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
mod_downloadHandler_filetype_ui <- function(id){ 
  # will want to change this to input_UI so we can include additional items
  # such as "Include comments" checkboxes for summary, maintmetrics, comm usage, and overall comments
  ns <- NS(id)
  tagList(
    div(style = 'width:230px;',
      selectInput(ns("report_format"), "Select Format", c("html", "docx", "pdf"))
    )
  )
}

#' downloadHandler Inlcude UI Function
#'
#' @description A shiny Module.
#'
#' @param id Internal parameters for {shiny}.
#'
#' @noRd 
mod_downloadHandler_include_ui <- function(id){
  # will want to change this to input_UI so we can include additional items
  # such as "Include comments" check boxes for summary, maint-metrics, comm usage, and overall comments
  uiOutput(NS(id, "mod_downloadHandler_incl_output"))
}
#' downloadHandler Include Server Function
#'
#' @description A shiny Module.
#'
#' @param id Internal parameters for {shiny}.
#' 
#' @importFrom shiny showModal modalDialog
#' @importFrom glue glue
#' @importFrom shinyWidgets prettyCheckboxGroup updatePrettyCheckboxGroup
#'
#' @noRd 
mod_downloadHandler_include_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$mod_downloadHandler_incl_output <- renderUI({
      div(
        strong(p("Elements to include:")),
        div(align = 'left', class = 'twocol', style = 'margin-top: 0px;',
            shinyWidgets::prettyCheckboxGroup(
              ns("report_includes"), label = NULL, inline = FALSE,
              choices = rpt_choices, selected = isolate(session$userData$user_report$report_includes) %||% rpt_choices
            )
        )
      )
    })
    
    # save user selections and notify user
    observeEvent(input$store_prefs, {
      writeLines(
        session$userData$user_report$report_includes, 
        session$userData$user_report$user_file
      )

      shiny::showModal(shiny::modalDialog(title = "User preferences saved",
                                          "Report preferences stored for user", 
                                          footer = modalButton("Dismiss"), 
                                          easyClose = TRUE))
    }, ignoreInit = TRUE)
    
    observeEvent(session$userData$trigger_events$update_report_pref_inclusions, {
      shinyWidgets::updatePrettyCheckboxGroup(
        session,
        "report_includes",
        selected = session$userData$user_report$report_includes
      )
    })
    
    observeEvent(input$report_includes, {
      session$userData$user_report$report_includes <- input$report_includes %||% ""
    }, ignoreNULL = FALSE, ignoreInit = TRUE)
    
    return(reactive(input$report_includes))
  })
}
  
#' downloadHandler Server Functions
#' 
#' @importFrom flextable flextable set_table_properties colformat_char
#'
#' @noRd 
mod_downloadHandler_server <- function(id, pkgs, user, metric_weights){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    observe({
      if(isTruthy(pkgs())) {
        shinyjs::enable("download_reports")
      } else {
        shinyjs::disable("download_reports")
      }
    })
    
    output$download_reports <- downloadHandler(
      filename = function() {
        n_pkgs <- length(pkgs())
        
        if (n_pkgs > 1) {
          report_datetime <- stringr::str_replace_all(stringr::str_replace(get_time(), " ", "_"), ":", "-")
          glue::glue('RiskAssessment-Report-{report_datetime}.zip')
        } else {
          pkg_ver <- dbSelect("SELECT version FROM package WHERE name = {pkgs()}")
          glue::glue('{pkgs()}_{pkg_ver}_Risk_Assessment.{input$report_format}')
        }
      },
      content = function(file) {
        n_pkgs <- length(pkgs())
        
        req(n_pkgs > 0)
        
        if (!isTruthy(session$userData$repo_pkgs())) {
          if (isTRUE(getOption("shiny.testmode"))) {
            session$userData$repo_pkgs(purrr::map_dfr(test_pkg_refs, ~ as.data.frame(.x, col.names = c("Package", "Version", "Source"))))
          } else {
            session$userData$repo_pkgs(as.data.frame(utils::available.packages()[,1:2]))
          }
        }
        
        shiny::withProgress(
          message = glue::glue('Downloading {ifelse(n_pkgs > 1, paste0(n_pkgs, " "), "")}Report{ifelse(n_pkgs > 1, "s", paste0(": ", pkgs()))}'),
          value = 0,
          max = n_pkgs + 2, # Tell the progress bar the total number of events.
          {
            shiny::incProgress(1)
            
            my_tempdir <- tempdir()
            if (input$report_format == "html") {
              
              # https://github.com/rstudio/fontawesome/issues/99
              # Here, we make sure user has a functional version of fontawesome
              fa_v <- packageVersion("fontawesome")
              if(fa_v == '0.4.0') {
                msg1 <- "HTML reports will not render with {fontawesome} v0.4.0."
                msg2 <- glue::glue("You currently have v{fa_v} installed. If the report download failed, please install a stable version. We recommend v0.5.0 or higher.")
                warning(paste(msg1, msg2))
                showModal(modalDialog(
                  size = "l",
                  title = h3(msg1, class = "mb-0 mt-0 txt-color"),
                  h5(msg2)
                ))
              }
              
              Report <- file.path(my_tempdir, "reportHtml.Rmd")
              file.copy(app_sys('report_downloads', 'reportHtml.Rmd'), Report, overwrite = TRUE)
              file.copy(app_sys('report_downloads', 'raa-image.png'),
                        file.path(my_tempdir, 'raa-image.png'), overwrite = TRUE)
              file.copy(app_sys('report_downloads', 'header.html'),
                        file.path(my_tempdir, 'header.html'), overwrite = TRUE)
            } 
            else if (input$report_format == "docx") { 
              Report <- file.path(my_tempdir, "reportDocx.Rmd")
              if (!dir.exists(file.path(my_tempdir, "images")))
                dir.create(file.path(my_tempdir, "images"))
              file.copy(app_sys('report_downloads', 'reportDocx.Rmd'),
                        Report,
                        overwrite = TRUE)
              file.copy(app_sys('report_downloads', 'header.docx'),
                        file.path(my_tempdir, 'header.docx'),
                        overwrite = TRUE)
              file.copy(app_sys('report_downloads', 'read_html.lua'),
                        file.path(my_tempdir, "read_html.lua"), overwrite = TRUE)
              file.copy(app_sys('report_downloads', 'images', 'user-tie.png'),
                        file.path(my_tempdir, "images", "user-tie.png"),
                        overwrite = TRUE)
              file.copy(app_sys('report_downloads', 'images', 'user-shield.png'),
                        file.path(my_tempdir, "images", "user-shield.png"),
                        overwrite = TRUE)
              file.copy(app_sys('report_downloads', 'images', 'calendar-alt.png'),
                        file.path(my_tempdir, "images", "calendar-alt.png"),
                        overwrite = TRUE)
              file.copy(app_sys('report_downloads', 'raa-image.png'),
                        file.path(my_tempdir, 'raa-image.png'), overwrite = TRUE)
            } 
            else { 
              Report <- file.path(my_tempdir, "reportPdf.Rmd")
              if (!dir.exists(file.path(my_tempdir, "images")))
                dir.create(file.path(my_tempdir, "images"))
              file.copy(app_sys('report_downloads', 'reportPdf.Rmd'),
                        Report,
                        overwrite = TRUE)
              file.copy(app_sys('report_downloads', 'header.tex'),
                        file.path(my_tempdir, 'header.tex'),
                        overwrite = TRUE)
              file.copy(app_sys('report_downloads', 'fancyhdr.sty'),
                        file.path(my_tempdir, 'fancyhdr.sty'),
                        overwrite = TRUE)              
              file.copy(app_sys('report_downloads', 'read_html.lua'),
                        file.path(my_tempdir, "read_html.lua"), overwrite = TRUE)
              file.copy(app_sys('report_downloads', 'images', 'user-tie.png'),
                        file.path(my_tempdir, "images", "user-tie.png"),
                        overwrite = TRUE)
              file.copy(app_sys('report_downloads', 'images', 'user-shield.png'),
                        file.path(my_tempdir, "images", "user-shield.png"),
                        overwrite = TRUE)
              file.copy(app_sys('report_downloads', 'images', 'calendar-alt.png'),
                        file.path(my_tempdir, "images", "calendar-alt.png"),
                        overwrite = TRUE)
              file.copy(app_sys('report_downloads', 'raa-image.png'),
                        file.path(my_tempdir, 'raa-image.png'), overwrite = TRUE)
            }
            
            fs <- c()
            for (i in 1:n_pkgs) {
              # Grab package name and version, then create filename and path.
              # this_pkg <- "stringr" # for testing
              selected_pkg <- get_pkg_info(pkgs()[i])
              this_pkg <- selected_pkg$name
              this_ver <- selected_pkg$version
              file_named <- glue::glue('{this_pkg}_{this_ver}_Risk_Assessment.{input$report_format}')
              path <- if (n_pkgs > 1) {
                file.path(my_tempdir, file_named)
              } else {
                file
              }
              
              pkg_list <- list(
                id = selected_pkg$id,
                name = selected_pkg$name,
                version = selected_pkg$version,
                date_added = selected_pkg$date_added,
                title = selected_pkg$title,
                decision = selected_pkg$decision,
                description = selected_pkg$description,
                author = selected_pkg$author,
                maintainer = selected_pkg$maintainer,
                license = selected_pkg$license,
                published = selected_pkg$published_on,
                score = selected_pkg$score
              )
              
              # gather comments data
              overall_comments <- get_overall_comments(this_pkg)
              pkg_summary <- get_pkg_summary(this_pkg)
              mm_comments <- get_mm_comments(this_pkg)
              cm_comments <- get_cm_comments(this_pkg)
              se_comments <- get_se_comments(this_pkg)
              fe_comments <- get_fe_comments(this_pkg)
              
              # gather maint metrics & community metric data
              mm_data <- get_metric_data(this_pkg, metric_class = "maintenance")
              comm_data <- get_comm_data(this_pkg)
              comm_cards <- build_comm_cards(comm_data)
              downloads_plot <- build_comm_plotly(comm_data)
              metric_tbl <- dbSelect("select * from metric", db_name = golem::get_golem_options('assessment_db_name'))
              
              dep_metrics <- eventReactive(this_pkg, {
                get_depends_data(this_pkg, db_name = golem::get_golem_options("assessment_db_name"))
              })

              dep_cards <- build_dep_cards(data = dep_metrics(), loaded = session$userData$loaded2_db()$name, toggled = 0L)

              dep_table <- purrr::map_df(dep_metrics()$name, ~get_versnScore(.x, session$userData$loaded2_db(), session$userData$repo_pkgs())) %>%
                  right_join(dep_metrics(), by = "name") %>%
                  select(package, type, version, score) %>%
                  arrange(package, type) %>%
                  distinct()

              # Render the report, passing parameters to the rmd file.
              rmarkdown::render(
                input = Report,
                output_file = path,
                clean = FALSE,
                params = list(pkg = pkg_list,
                              report_includes = input$report_includes,
                              riskmetric_version = paste0(packageVersion("riskmetric")),
                              app_version = golem::get_golem_options('app_version'),
                              metric_weights = metric_weights(),
                              user_name = user$name,
                              user_role = paste(user$role, collapse = ', '),
                              overall_comments = overall_comments,
                              pkg_summary = pkg_summary,
                              mm_comments = mm_comments,
                              cm_comments = cm_comments,
                              se_comments = se_comments,
                              fe_comments = fe_comments,
                              maint_metrics = mm_data,
                              com_metrics = comm_cards,
                              com_metrics_raw = comm_data,
                              downloads_plot_data = downloads_plot,
                              dep_cards = dep_cards,
                              dep_table = dep_table,
                              metric_tbl = metric_tbl
                )
              )
              fs <- c(fs, path)  # Save all the reports/
              shiny::incProgress(1) # Increment progress bar.
            }
            # Zip all the files up. -j retains just the files in zip file.
            if (n_pkgs > 1) zip(zipfile = file, files = fs, extras = "-j")
            shiny::incProgress(1) # Increment progress bar.
          })
      }
    )
  })
}
    
## To be copied in the UI
# mod_downloadHandler_button_ui("downloadHandler_1")

## To be copied in the UI
# mod_downloadHandler_filetype_ui("downloadHandler_1")
    
## To be copied in the server
# mod_downloadHandler_server("downloadHandler_1")
