#' 'Upload Package' UI
#' 
#' @param id a module id
#' 
#' 
#' @importFrom DT dataTableOutput
#' @keywords internal
#' 
uploadPackageUI <- function(id) {
  fluidPage(
    br(), br(),
    
    introJSUI(NS(id, "introJS")),
    
    tags$head(tags$style(".shiny-notification {font-size:30px; color:darkblue; position: fixed; width:415px; height: 150px; top: 75% ;right: 10%;")),
    
    fluidRow(
      
      column(
        width = 4,
        div(
          id = "type-package-group",
          style = "display: flex;",
          shinyjs::disabled(
            selectizeInput(NS(id, "pkg_lst"), "Type Package Name(s)", choices = NULL, multiple = TRUE, 
                           options = list(selectOnTab = TRUE, showAddOptionOnCreate = FALSE, 
                                          onFocus = I(paste0('function() {Shiny.setInputValue("', NS(id, "load_cran"), '", "load", {priority: "event"})}')))),
            actionButton(NS(id, "add_pkgs"), shiny::icon("angle-right"),
                         style = 'height: calc(1.5em + 1.5rem + 2px)')),
          tags$script(I(glue::glue('$(window).on("load resize", function() {{
                                             $("#{NS(id, "add_pkgs")}").css("margin-top", $("#{NS(id, "pkg_lst")}-label")[0].scrollHeight + .5*parseFloat(getComputedStyle(document.documentElement).fontSize))
                                             }})
                                             $("a[data-toggle=\'tab\']").on("shown.bs.tab", function(e) {{
                                             $("#{NS(id, "add_pkgs")}").css("margin-top", $("#{NS(id, "pkg_lst")}-label")[0].scrollHeight + .5*parseFloat(getComputedStyle(document.documentElement).fontSize))
                                             }})')))
        ),
        uiOutput(NS(id, "rem_pkg_div"))
      ),
      column(width = 1),
      
      column(
        width = 4,
        div(id = "upload-file-grp",
            shinyjs::disabled(
              fileInput(
                inputId = NS(id, "uploaded_file"),
                label = "Or Upload a CSV file",
                accept = ".csv",
                placeholder = "No file selected"
              )),
            tags$script(glue::glue('$("#{NS(id, \'uploaded_file\')}").parents("span").addClass("disabled")'))
        ),
        uiOutput(NS(id, "upload_format_lnk"))
      ),
    ),
    fluidRow(mod_decision_automation_ui("automate")),
    
    # Display the summary information of the uploaded csv.
    fluidRow(column(width = 12, htmlOutput(NS(id, "upload_summary_text")))),
    
    # Summary of packages uploaded.
    fluidRow(column(width = 12, DT::dataTableOutput(NS(id, "upload_pkgs_table"))))
  )
}


#' Server logic for the 'Upload Package' module
#'
#' @param id a module id
#' @param user a username
#' @param auto_list a list of decision automation rules
#' @param parent the parent (calling module) session information
#' 
#' @importFrom DT datatable dataTableOutput formatStyle renderDataTable
#' @importFrom formattable as.datatable csscolor formattable formatter style
#' @importFrom glue glue
#' @importFrom golem get_golem_options
#' @importFrom loggit loggit
#' @importFrom purrr map map_chr map_lgl 
#' @importFrom riskmetric pkg_ref
#' @importFrom rlang inform is_empty
#' @importFrom shiny icon
#' @importFrom shinyjs runjs
#' @importFrom utils adist available.packages download.file read.csv
#' 
#' @keywords internal
#' 
uploadPackageServer <- function(id, user, auto_list, credentials, parent) {
  if (missing(credentials))
    credentials <- get_db_config("credentials")
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    observe({
      req(user$role)
      req(credentials$privileges)
      
      if ("add_package" %in% credentials$privileges[[user$role]]) {
        shinyjs::enable("pkg_lst")
        shinyjs::enable("add_pkgs")
        shinyjs::enable("uploaded_file")
        shinyjs::runjs(glue::glue('$("#{NS(id, \'uploaded_file\')}").parents("span").removeClass("disabled")'))
      } else {
        shinyjs::disable("pkg_lst")
        shinyjs::disable("add_pkgs")
        shinyjs::disable("uploaded_file")
        shinyjs::runjs(glue::glue('$("#{NS(id, \'uploaded_file\')}").parents("span").addClass("disabled")'))
      }
    })

    output$upload_format_lnk <- renderUI({
      req("add_package" %in% credentials$privileges[[user$role]])
      
      actionLink(NS(id, "upload_format"), "View Sample Dataset")
    })
    
    # Determine which guide to use for IntroJS.
    upload_pkg_txt <- reactive({
      req(uploaded_pkgs())
      
      dplyr::bind_rows(
        upload_pkg,
        if ("add_package" %in% credentials$privileges[[user$role]]) upload_pkg_add,
        if ("delete_package" %in% credentials$privileges[[user$role]]) upload_pkg_delete,
        if ("auto_decision_adjust" %in% credentials$privileges[[user$role]]) upload_pkg_dec_adj,
        if (nrow(uploaded_pkgs()) > 0) upload_pkg_comp
      )
    })

    cran_pkgs <- reactiveVal()
    
    observeEvent(input$load_cran, {
      if (!isTruthy(cran_pkgs())) {
        if (isTRUE(getOption("shiny.testmode"))) {
          cran_pkgs(test_pkg_lst)
        } else {
          cran_pkgs(utils::available.packages("https://cran.rstudio.com/src/contrib")[,1])
        }
      }
    },
    once = TRUE)
    
    pkgs_have <- reactiveVal()
    
    observeEvent(input$curr_pkgs, {
      pkgs_have(dbSelect("select name from package")[,1])
    })
    
    observeEvent(cran_pkgs(), {
      req(cran_pkgs())
      updateSelectizeInput(session, "pkg_lst", choices = cran_pkgs(), server = TRUE)
    })
    
    observeEvent(pkgs_have(), {
      updateSelectizeInput(session, "rem_pkg_lst", choices = pkgs_have(), server = TRUE)
    })
    
    # Start introjs when help button is pressed. Had to do this outside of
    # a module in order to take a reactive data frame of steps
    introJSServer("introJS", text = upload_pkg_txt, user, credentials)

    uploaded_pkgs00 <- reactiveVal()
    
    observeEvent(session$userData$trigger_events$reset_pkg_upload, {
      uploaded_pkgs(data.frame())
    })

    output$rem_pkg_div <- renderUI({
      req(user$role)
      req(credentials$privileges)
      req("delete_package" %in% credentials$privileges[[user$role]])
      
      session$onFlushed(function() {
        shinyjs::runjs(glue::glue('$("#{NS(id, "rem_pkg_btn")}").css("margin-top", $("#{NS(id, "rem_pkg_lst")}-label")[0].scrollHeight + .5*parseFloat(getComputedStyle(document.documentElement).fontSize))'))
      })
      
      div(
        id = "rem-package-group",
        style = "display: flex;",
        selectizeInput(NS(id, "rem_pkg_lst"), "Remove Package(s)", choices = NULL, multiple = TRUE,
                       options = list(selectOnTab = TRUE, showAddOptionOnCreate = FALSE, 
                                      onFocus = I(paste0('function() {Shiny.setInputValue("', NS(id, "curr_pkgs"), '", "load", {priority: "event"})}')))),
        # note the action button moved out of alignment with 'selectizeInput' under 'renderUI'
        actionButton(NS(id, "rem_pkg_btn"), shiny::icon("trash-can")),
        tags$script(I(glue::glue('$(window).on("load resize", function() {{
                                             $("#{NS(id, "rem_pkg_btn")}").css("margin-top", $("#{NS(id, "rem_pkg_lst")}-label")[0].scrollHeight + .5*parseFloat(getComputedStyle(document.documentElement).fontSize))
                                             }})
                                             $("a[data-toggle=\'tab\']").on("shown.bs.tab", function(e) {{
                                             $("#{NS(id, "rem_pkg_btn")}").css("margin-top", $("#{NS(id, "rem_pkg_lst")}-label")[0].scrollHeight + .5*parseFloat(getComputedStyle(document.documentElement).fontSize))
                                             }})')))
      )
    })
    
    observeEvent(input$uploaded_file, {
      req(input$uploaded_file)
      
      if(is.null(input$uploaded_file$datapath))
        uploaded_pkgs00(validate('Please upload a nonempty CSV file.'))
      
      uploaded_packages <- utils::read.csv(input$uploaded_file$datapath, stringsAsFactors = FALSE)
      np <- nrow(uploaded_packages)
      if(np == 0)
        uploaded_pkgs00(validate('Please upload a nonempty CSV file.'))
      
      if(!all(colnames(uploaded_packages) == colnames(template)))
        uploaded_pkgs00(validate("Please upload a CSV with a valid format."))
      
      # Add status column and remove white space around package names.
      uploaded_packages <- uploaded_packages %>%
        dplyr::mutate(
          status = rep('', np),
          package = trimws(package),
          version = trimws(version)
        )
      
      uploaded_pkgs00(uploaded_packages)
    })
    
    
    
    observeEvent(input$add_pkgs, {
      req(input$pkg_lst)
      
      np <- length(input$pkg_lst)
      uploaded_packages <-
        dplyr::tibble(
          package = input$pkg_lst,
          version = rep('0.0.0', np),
          status = rep('', np)
        )
      
      updateSelectizeInput(session, "pkg_lst", selected = "")
      
      uploaded_pkgs00(uploaded_packages)
    })
    
    observeEvent(session$userData$trigger_events$upload_pkgs, {
      req(session$userData$trigger_events$upload_pkgs)
      
      np <- length(session$userData$trigger_events$upload_pkgs)
      uploaded_packages <-
        dplyr::tibble(
          package = session$userData$trigger_events$upload_pkgs,
          version = rep('0.0.0', np),
          status = rep('', np)
        )
      
      uploaded_pkgs00(uploaded_packages)
    })
    
    observeEvent(input$rem_pkg_btn, {
      req("delete_package" %in% credentials$privileges[[user$role]]) 
      
      np <- length(input$rem_pkg_lst)
      uploaded_packages <-
        dplyr::tibble(
          package = input$rem_pkg_lst,
          version = rep('0.0.0', np),
          status = rep("removed", np)
        )

      for (i in 1:np) {
      pkg_name <- input$rem_pkg_lst[i]
      # update version with what is in the package table
      ver_num <- dbSelect("select version from package where name = {pkg_name}", db_name = golem::get_golem_options('assessment_db_name'))
      uploaded_packages$version[i] <- ver_num
      dbUpdate("DELETE FROM package WHERE name = {pkg_name}", db_name = golem::get_golem_options('assessment_db_name'))
      unlink(glue::glue("tarballs/{pkg_name}_{ver_num}.tar.gz"))
      }
      
      # clean up other db tables
      db_trash_collection(db_name = golem::get_golem_options('assessment_db_name'))
      
      # update the list of packages we have
      pkgs_have(dbSelect("select name from package")[,1])

      updateSelectizeInput(session, "rem_pkg_lst", choices=pkgs_have(), selected = "")
      
      uploaded_pkgs(uploaded_packages)

    })
    
    checking_urls <- reactiveValues()
    
    observeEvent(input$check_urls, {
      checking_urls$finished <- FALSE
      removeModal()
    })
    
    observe({
      req(input$check_urls, !isTRUE(checking_urls$finished))
      invalidateLater(60*1000)
      
      withProgress({
        good_urls <- purrr::map_lgl(checking_urls$url_lst, 
                                    ~ try(curlGetHeaders(.x, verify = FALSE), silent = TRUE) %>%
                                      {class(.) != "try-error" && attr(., "status") != 404})
        Sys.sleep(.5)
      }, message = "Checking URLs")
      
      checking_urls$finished <- all(good_urls)
    })
    
    observeEvent(checking_urls$finished, {
      req(checking_urls$finished)
      showModal(modalDialog(
        title = h2("Data Connection Issues"),
        h5("The needed URLs are now reachable. Please try to upload the desired packages now."),
      ))
    })
    
    uploaded_pkgs <- reactiveVal(data.frame())
    # Save all the uploaded packages, marking them as 'new', 'not found', 
    # 'duplicate' or 'removed'
    observeEvent(uploaded_pkgs00(), {

      uploaded_packages <- uploaded_pkgs00()
      uploaded_pkgs00(NULL)
      uploaded_packages$score <- NA_real_
      if (!rlang::is_empty(auto_list())) {
        uploaded_packages$decision <- ""
        uploaded_packages$decision_rule <- ""
      }
      np <- nrow(uploaded_packages)
      
      if (!isTRUE(getOption("shiny.testmode"))) {
        url_lst <- list(
          "https://cran.rstudio.com",
          "https://cran.r-project.org",
          "https://cranlogs.r-pkg.org"
        )
        
        good_urls <- purrr::map_lgl(url_lst, 
                                    ~ try(curlGetHeaders(.x, verify = FALSE), silent = TRUE) %>%
                                      {class(.) != "try-error" && attr(., "status") != 404})

        if (!all(good_urls)) {
          checking_urls$url_lst <- url_lst[!good_urls]
          showModal(modalDialog(
            title = h2("Data Connection Issues"),
            h5("The process has been cancelled because at least one of the URLs used to populate the metrics is unreachable at this time."),
            br(),
            h5("Notify when  URLs are reachable?"),
            footer = tagList(
              actionButton(ns("check_urls"), "Yes"),
              modalButton("No")
            )
          ))
        }
        
      req(all(good_urls))
      }
      
      if (!isTruthy(cran_pkgs())) {
        if (isTRUE(getOption("shiny.testmode"))) {
          cran_pkgs(test_pkg_lst)
        } else {
          cran_pkgs(utils::available.packages("https://cran.rstudio.com/src/contrib")[,1])
        }
      }
      
      # Start progress bar. Need to establish a maximum increment
      # value based on the number of packages, np, and the number of
      # incProgress() function calls in the loop, plus one to show
      # the incProgress() that the process is completed.
      withProgress(
        max = (np * 5) + 1, value = 0,
        message = "Uploading Packages to DB:", {
          
          for (i in 1:np) {

            user_ver <- uploaded_packages$version[i]
            incProgress(1, detail = glue::glue("{uploaded_packages$package[i]} {user_ver}"))
            

            if (grepl("^[[:alpha:]][[:alnum:].]*[[:alnum:]]$", uploaded_packages$package[i])) {
              # run pkg_ref() to get pkg version and source info
              if (!isTRUE(getOption("shiny.testmode")))
                ref <- riskmetric::pkg_ref(uploaded_packages$package[i],
                                           source = "pkg_cran_remote")
              else
                ref <- test_pkg_refs[[uploaded_packages$package[i]]]
            } else {
              ref <- list(name = uploaded_packages$package[i],
                          source = "name_bad")
            }

            if (ref$source %in% c("pkg_missing", "name_bad")) {
              incProgress(1, detail = 'Package {uploaded_packages$package[i]} not found')

              # Suggest alternative spellings using utils::adist() function
              v <- utils::adist(uploaded_packages$package[i], cran_pkgs(), ignore.case = FALSE)
              rlang::inform(paste("Package name",uploaded_packages$package[i],"was not found."))

              suggested_nms <- paste("Suggested package name(s):",paste(head(cran_pkgs()[which(v == min(v))], 10),collapse = ", "))
              rlang::inform(suggested_nms)

              uploaded_packages$status[i] <- HTML(paste0('<a href="#" title="', suggested_nms, '">not found</a>'))

              if (ref$source == "pkg_missing")
                loggit::loggit('WARN',
                               glue::glue('Package {ref$name} was flagged by riskmetric as {ref$source}.'))
              else
                loggit::loggit('WARN',
                               glue::glue("Riskmetric can't interpret '{ref$name}' as a package reference."))

              next
            }

            ref_ver <- as.character(ref$version)
            
            deets <- glue::glue("{uploaded_packages$package[i]} {ref_ver}")
            uploaded_packages$version[i] <- ref_ver
            
            # Save version.
            incProgress(1, detail = deets)
            uploaded_packages$version[i] <- as.character(ref$version)
            
            found <- nrow(dbSelect(
              "SELECT name
              FROM package
              WHERE name = {uploaded_packages$package[i]}"))
            
            uploaded_packages$status[i] <- ifelse(found == 0, 'new', 'duplicate')

            # Add package and metrics to the db if package is not in the db.
            if(!found) {
              # Get and upload pkg general info to db.
              incProgress(1, detail = deets)

              if (!isTRUE(getOption("shiny.testmode"))) {
                dwn_ld <- utils::download.file(ref$tarball_url, file.path("tarballs", basename(ref$tarball_url)), 
                                        quiet = TRUE, mode = "wb")
                if (dwn_ld != 0) {
                  loggit::loggit("INFO", glue::glue("Unable to download the source files for {uploaded_packages$package[i]} from '{ref$tarball_url}'."))
                }
              }
              insert_pkg_info_to_db(uploaded_packages$package[i], ref_ver)
              # Get and upload maintenance metrics to db.
              incProgress(1, detail = deets)
              
              insert_riskmetric_to_db(uploaded_packages$package[i])
              # Get and upload community metrics to db.
              incProgress(1, detail = deets)
              
              insert_community_metrics_to_db(uploaded_packages$package[i])
              uploaded_packages$score[i] <- get_pkg_info(uploaded_packages$package[i])$score
              if (!rlang::is_empty(auto_list())) {
                assigned_decision <- assign_decisions(auto_list(), uploaded_packages$package[i])
                uploaded_packages$decision[i] <- assigned_decision$decision
                uploaded_packages$decision_rule[i] <- assigned_decision$decision_rule
              }
            }
          }
          
          incProgress(1, detail = "   **Completed Pkg Uploads**")
          Sys.sleep(0.25)
          
      }) #withProgress
      
      uploaded_pkgs(uploaded_packages)
      
    })
    
    # Download the sample dataset.
    output$download_sample <- downloadHandler(
      filename = function() {
        paste("template", ".csv", sep = "")
      },
      content = function(file) {
        write.csv(template, file, row.names = F)
      }
    )
    
    # Removed/Uploaded packages summary.
    output$upload_summary_text <- renderText({
      req(uploaded_pkgs)
      req(nrow(uploaded_pkgs()) > 0)

      # modify the message if we are removing packages
      if(isTruthy(sum(uploaded_pkgs()$status == 'removed') >0)) {
        loggit::loggit("INFO",
                       paste("Uploaded file:", input$uploaded_file$name, 
                             "Removed Packages", sum(uploaded_pkgs()$status == 'removed')),
                       echo = FALSE)
        
        as.character(tagList(
          br(), br(),
          hr(),
          div(id = "upload_summary_div",
              h5("Summary of Removed package(s)"),
              br(),
              p(tags$b("Removed Packages: "), sum(uploaded_pkgs()$status == 'removed')),
              p(tags$b("Remaining Packages: "), nrow(dbSelect("SELECT name FROM package"))),
              p("Note: The assessment will be performed on the latest version of each
          package, irrespective of the uploaded version.")
          )
        ))
      } else {
      loggit::loggit("INFO",
                     paste("Uploaded file:", input$uploaded_file$name, 
                           "Total Packages:", nrow(uploaded_pkgs()),
                           "New Packages:", sum(uploaded_pkgs()$status == 'new'),
                           "Undiscovered Packages:", sum(grepl('not found', uploaded_pkgs()$status)),
                           "Duplicate Packages:", sum(uploaded_pkgs()$status == 'duplicate')),
                     echo = FALSE)
      if (!is.null(uploaded_pkgs()$decision)) {
        dec_lst <- uploaded_pkgs()$decision %>% 
          unique() %>% 
          `[`(. != "") %>%
          purrr::map_chr(~ glue::glue("{.x}: {sum(uploaded_pkgs()$decision == .x)}")) %>%
          purrr::map(~ list(tags$code(.x), HTML("&emsp;")))
      }
      as.character(tagList(
        br(), br(),
        hr(),
        div(id = "upload_summary_div",
            h5("Summary of uploaded package(s)"),
            br(),
            p(tags$b("Total Packages: "), nrow(uploaded_pkgs())),
            p(tags$b("New Packages: "), sum(uploaded_pkgs()$status == 'new')),
            if (!is.null(uploaded_pkgs()$decision)) list(p(tags$b("Decisions Made: "), sum(uploaded_pkgs()$decision != "")), p(style = "margin-left: 25px", dec_lst)),
            p(tags$b("Undiscovered Packages: "), sum(grepl('not found', uploaded_pkgs()$status))),
            p(tags$b("Duplicate Packages: "), sum(uploaded_pkgs()$status == 'duplicate')),
            p("Note: The assessment will be performed on the latest version of each
          package, irrespective of the uploaded version.")
        )
      ))
      }
    })
    
    # Uploaded packages table.
    output$upload_pkgs_table <- DT::renderDataTable({
      req(nrow(uploaded_pkgs()) > 0)
      
      uploaded_pkgs_ext <- 
        if(!isTruthy(sum(uploaded_pkgs()$status == 'removed') > 0)) {
          cbind(uploaded_pkgs(), 
                data.frame(
                  explore_metrics = shinyInput(actionButton, nrow(uploaded_pkgs()),
                                               'button_',
                                               size = "xs",
                                               style='height:24px; padding-top:1px;',
                                               label = icon("arrow-right", class="fa-regular", lib = "font-awesome"),
                                               onclick = paste0('Shiny.setInputValue(\"' , ns("select_button"), '\", this.id, {priority: \"event\"})')
                  )
                )
          ) %>% # keep action button for 'new' or 'duplicate' only
            mutate(explore_metrics = if_else(!status %in% c('new', 'duplicate'), "", explore_metrics))
        } else {
          uploaded_pkgs()
        }
      
      formattable::as.datatable(
        formattable::formattable(
          uploaded_pkgs_ext,
          list(
            score = formattable::formatter(
              "span",
              style = x ~ formattable::style(display = "block",
                                             "border-radius" = "4px",
                                             "padding-right" = "4px",
                                             "color" = "#000000",
                                             "order" = x,
                                             "background-color" = formattable::csscolor(
                                               setColorPalette(100)[round(as.numeric(x)*100)]))),
            decision = formattable::formatter(
              "span",
              style = x ~ formattable::style(display = "block",
                                             "border-radius" = "4px",
                                             "padding-right" = "4px",
                                             "color" = "#000000",
                                             "background-color" = glue::glue("var(--{risk_lbl(x, type = 'attribute')}-color)")))
          )
        ),
        escape = FALSE,
        class = "cell-border",
        selection = 'none',
        rownames = FALSE,
        colnames = gsub("_", " ", names(uploaded_pkgs_ext)),
        options = list(
          searching = FALSE,
          columnDefs = list(list(className = 'dt-center', targets = "_all")),
          sScrollX = "100%",
          lengthChange = TRUE,
          aLengthMenu = list(c(5, 10, 20, 100, -1), list('5', '10', '20', '100', 'All')),
          iDisplayLength = 10
        )
      ) %>%
        DT::formatStyle(names(uploaded_pkgs_ext), textAlign = 'center')
    })
    
    # View sample dataset.
    observeEvent(input$upload_format, {
      DT::dataTableOutput(NS(id, "sampletable"))
      
      showModal(modalDialog(
        size = "l",
        easyClose = TRUE,
        footer = "",
        h5("Sample Dataset", style = 'text-align: center !important'),
        hr(),
        br(),
        fluidRow(
          column(
            width = 12,
            output$sampletable <- DT::renderDataTable(
              DT::datatable(
                template,
                escape = FALSE,
                editable = FALSE,
                filter = 'none',
                selection = 'none',
                extensions = 'Buttons',
                options = list(
                  sScrollX = "100%",
                  aLengthMenu = list(c(5, 10, 20, 100, -1), list('5', '10', '20', '100', 'All')),
                  iDisplayLength = 5,
                  dom = 't'
                )
              )))
        ),
        br(),
        fluidRow(column(align = 'center', width = 12,
                        downloadButton(NS(id, "download_sample"), "Download")))
      ))
    })
    
    observeEvent(input$select_button, {
      req(uploaded_pkgs())
      
      selectedRow <- as.numeric(strsplit(input$select_button, "_")[[1]][2])
      
      # grab the package name
      pkg_name <- uploaded_pkgs()[selectedRow, 1]
      
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
      updateSelectInput(session = parent, 
                        inputId = 'metric_type', 
                        selected = "mm"
      )
    })
    
    uploaded_pkgs
  })
}
