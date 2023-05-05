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
          selectizeInput(NS(id, "pkg_lst"), "Type Package Name(s)", choices = NULL, multiple = TRUE, 
                         options = list(showAddOptionOnCreate = FALSE, 
                                        onFocus = I(paste0('function() {Shiny.setInputValue("', NS(id, "load_cran"), '", "load", {priority: "event"})}')))),
          actionButton(NS(id, "add_pkgs"), shiny::icon("angle-right"),
                       style = 'height: calc(1.5em + 1.5rem + 2px)'),
          tags$head(tags$script(I(paste0('$(window).on("load resize", function() {$("#', NS(id, "add_pkgs"), '").css("margin-top", $("#', NS(id, "pkg_lst"), '-label")[0].scrollHeight + .5*parseFloat(getComputedStyle(document.documentElement).fontSize));});'))))
        ),
        
        uiOutput(NS(id, "rem_pkg_div"))
      ),
      column(width = 1),
      
      column(
        width = 4,
        div(id = "upload-file-grp",
            fileInput(
              inputId = NS(id, "uploaded_file"),
              label = "Or Upload a CSV file",
              accept = ".csv",
              placeholder = "No file selected"
            )
        ),
        actionLink(NS(id, "upload_format"), "View Sample Dataset")
      ),
     ),
    fluidRow(mod_decision_automation_ui(NS(id, "automate"))),

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
#' 
#' @importFrom riskmetric pkg_ref
#' @importFrom rintrojs introjs
#' @importFrom utils read.csv available.packages
#' @importFrom rvest read_html html_nodes html_text
#' @keywords internal
#' 
uploadPackageServer <- function(id, user) {
  moduleServer(id, function(input, output, session) {
    
    assessment_lib <- get_golem_config("assessment_lib", file = app_sys("db-config.yml"))
    assessment_lib$packages <- 
        list.files(assessment_lib$location) %>%
        purrr::map_dfr(~ data.frame(name = .x, version = .x %>% packageDescription(assessment_lib$location, "Version") %>% as.character()))

    # Determine which guide to use for IntroJS.
    upload_pkg_txt <- reactive({
      req(uploaded_pkgs())
      
      if(user$role == "admin") {
        upload_pkg <- bind_rows(upload_pkg, upload_adm)
      }
      if(nrow(uploaded_pkgs()) > 0) 
        upload_pkg_complete <- bind_rows(upload_pkg, upload_pkg_comp)
      else 
        upload_pkg
    })
    
    auto_list <- mod_decision_automation_server("automate", user)
    pkg_lst <- reactiveValues(Library = purrr::pmap_chr(assessment_lib$packages, function(name, version) {paste(name, "-", version)}))
    

    cran_pkgs <- reactiveVal()
    
    observeEvent(input$load_cran, {
      if (!isTruthy(cran_pkgs())) {
        if (isTRUE(getOption("shiny.testmode"))) {
          cran_pkgs(test_pkg_lst)
        } else {
          cran_pkgs(as.data.frame(available.packages("https://cran.rstudio.com/src/contrib")[,c("Package", "Version")]))
        }
      }
    },
    once = TRUE)
    
    observeEvent(cran_pkgs(), {
      req(isTRUE(assessment_lib$editable))
      
      pkg_lst[["CRAN"]] <- 
        purrr::pmap_chr(cran_pkgs(), function(Package, Version) {paste(Package, "-", Version)})
    })
    
    pkgs_have <- reactiveVal()
    
    observeEvent(input$curr_pkgs, {
      pkgs_have(dbSelect("select name from package")[,1])
    })
    
    observeEvent(reactiveValuesToList(pkg_lst), {
      choice_lst <- reactiveValuesToList(pkg_lst) %>%
        purrr::imap(~ setNames(paste(.y, .x, sep = " - "), .x))
      choice_lst <- choice_lst[sort(names(choice_lst), decreasing = TRUE)]
      updateSelectizeInput(session, "pkg_lst", choices = choice_lst, server = TRUE)
    })
    
    observeEvent(pkgs_have(), {
      req(pkgs_have())
      req(user$role == "admin")
      updateSelectizeInput(session, "rem_pkg_lst", choices = pkgs_have(), server = TRUE)
    })
    
    # Start introjs when help button is pressed. Had to do this outside of
    # a module in order to take a reactive data frame of steps
    introJSServer("introJS", text = upload_pkg_txt, user)

    uploaded_pkgs00 <- reactiveVal()

    observeEvent(user$role, {
    req(user$role == "admin")  
    output$rem_pkg_div <- renderUI({
      div(
        id = "rem-package-group",
        style = "display: flex;",
        selectizeInput(NS(id, "rem_pkg_lst"), "Remove Package(s)", choices = NULL, multiple = TRUE,
                       options = list(create = FALSE, showAddOptionOnCreate = FALSE, 
                                      onFocus = I(paste0('function() {Shiny.setInputValue("', NS(id, "curr_pkgs"), '", "load", {priority: "event"})}')))),
        # note the action button moved out of alignment with 'selectizeInput' under 'renderUI'
        actionButton(NS(id, "rem_pkg_btn"), shiny::icon("trash-can")),
                     tags$head(tags$script(I(paste0('$(window).on("load resize", function() {$("#', NS(id, "rem_pkg_btn"), '").css("margin-top", $("#', NS(id, "rem_pkg_lst"), '-label")[0].scrollHeight + .5*parseFloat(getComputedStyle(document.documentElement).fontSize));});'))))
      )
     })
    })
    
    observeEvent(input$uploaded_file, {
      req(input$uploaded_file)
      
      if(is.null(input$uploaded_file$datapath))
        uploaded_pkgs00(validate('Please upload a nonempty CSV file.'))
      
      uploaded_packages <- read.csv(input$uploaded_file$datapath, stringsAsFactors = FALSE)
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
      
      uploaded_packages <-
        purrr::map_dfr(
          input$pkg_lst,
          function(.x) {
            pkg_info <- stringr::str_split(.x, " - ", simplify = TRUE)
            dplyr::tibble(
              package = pkg_info[2],
              version = pkg_info[3],
              status = ''
            )
          }
        )
      
      updateSelectizeInput(session, "pkg_lst", selected = "")
      
      uploaded_pkgs00(uploaded_packages)
    })
    
    observeEvent(input$rem_pkg_btn, {
      req(input$rem_pkg_lst)
      req(user$role == "admin")

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
      uploaded_packages$version[i] <- dbSelect("select version from package where name = {pkg_name}", db_name = golem::get_golem_options('assessment_db_name')) 
      dbUpdate("delete from package where name = {pkg_name}", db_name = golem::get_golem_options('assessment_db_name'))
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
    
    uploaded_pkgs01 <- reactiveVal()
    observeEvent(uploaded_pkgs00(), {
      uploaded_packages <- uploaded_pkgs00()
      uploaded_pkgs00(NULL)
      uploaded_packages$score <- NA_real_
      if (!rlang::is_empty(auto_list()))
        uploaded_packages$decision <- ""
      
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
              actionButton(session$ns("check_urls"), "Yes"),
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
          cran_pkgs(as.data.frame(available.packages("https://cran.rstudio.com/src/contrib")[,c("Package", "Version")]))
        }
      }
      
      uploaded_packages <-
        uploaded_packages %>%
        tidyr::unite("label", package, version, sep = " - ", remove = FALSE) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(
          action = dplyr::case_when(
            isTRUE(getOption("shiny.testmode")) ~ "test",
            assessment_lib$editable && length(find.package(package, lib.loc = assessment_lib$location, quiet = TRUE)) == 0 ~ "install",
            assessment_lib$editable && version != as.character(packageDescription(package, lib.loc = assessment_lib$location, "Version")) ~ "update",
            TRUE ~ "assess"
          )
        )
      
      showModal(modalDialog(
        size = "l",
        if (any(uploaded_packages$action == "install")) 
          tagList(
            h1("Packages to Install", style = "font-weight: bold; font-size: x-large"),
          checkboxGroupInput(session$ns("install_pkg_lst"), "",
                                             choices = uploaded_packages %>% dplyr::filter(action == "install") %>% dplyr::pull(label),
                                             selected = uploaded_packages %>% dplyr::filter(action == "install") %>% dplyr::pull(label))
          ),
        if (any(uploaded_packages$action == "update")) 
          tagList(
            h1("Packages to Update", style = "font-weight: bold; font-size: x-large"),
          checkboxGroupInput(session$ns("update_pkg_lst"), "",
                                             choices = uploaded_packages %>% dplyr::filter(action == "update") %>% dplyr::pull(label),
                                             selected = uploaded_packages %>% dplyr::filter(action == "update") %>% dplyr::pull(label))
          ),
        if (any(uploaded_packages$action == "assess")) 
          tagList(
            h1("Packages to Assess", style = "font-weight: bold; font-size: x-large"),
          checkboxGroupInput(session$ns("assess_pkg_lst"), "",
                                             choices = uploaded_packages %>% dplyr::filter(action == "assess") %>% dplyr::pull(label),
                                             selected = uploaded_packages %>% dplyr::filter(action == "assess") %>% dplyr::pull(label))
          ),
        footer = tagList(
          actionButton(session$ns("submit"), "Submit"),
          # TODO: Implement a background upload process
          # actionButton(session$ns("background"), "Background"),
          actionButton(session$ns('cancel'), 'Cancel')
        )
      ))
      
      uploaded_pkgs01(uploaded_packages)
    })
    
    observeEvent(input$cancel, {
      uploaded_pkgs01(NULL)
      removeModal()
      })
    
    uploaded_pkgs02 <- reactiveVal()
    observeEvent(input$submit, {
      uploaded_pkgs02({
        uploaded_pkgs01() %>%
          dplyr::filter(label %in% c(input$install_pkg_lst, input$update_pkg_lst, input$assess_pkg_lst)) %>%
          dplyr::select(-action, -label)
      })
      uploaded_pkgs01(NULL)
      removeModal()
    })
    uploaded_pkgs <- reactiveVal(data.frame())
    # Save all the uploaded packages, marking them as 'new', 'not found', 
    # 'duplicate' or 'removed'
    observeEvent(uploaded_pkgs02(), {
      uploaded_packages <- uploaded_pkgs02()
      uploaded_pkgs02(NULL)
      np <- nrow(uploaded_packages)
      
      # Start progress bar. Need to establish a maximum increment
      # value based on the number of packages, np, and the number of
      # incProgress() function calls in the loop, plus one to show
      # the incProgress() that the process is completed.
      withProgress(
        max = (np * 6) + 1, value = 0,
        message = "Processing Packages:", {
          
          for (i in 1:np) {
            
            user_ver <- uploaded_packages$version[i]
            setProgress((i-1)*6+1, detail = glue::glue("Querying {uploaded_packages$package[i]} V{user_ver}"))
            if (assessment_lib$editable && length(find.package(uploaded_packages$package[i], lib.loc = assessment_lib$location, quiet = TRUE)) == 0) {
              incProgress(1, detail = glue::glue("Installing {uploaded_packages$package[i]} V{user_ver}"))
              install.packages(uploaded_packages$package[i], lib = assessment_lib$location, repos = "https://cran.rstudio.com")
            } else if (assessment_lib$editable && uploaded_packages$version[i] != as.character(packageDescription(uploaded_packages$package[i], lib.loc = assessment_lib$location, "Version"))) {
              incProgress(1, detail = glue::glue("Installing {uploaded_packages$package[i]} V{user_ver}"))
              install.packages(uploaded_packages$package[i], lib = assessment_lib$location, repos = "https://cran.rstudio.com")
            }
            
            incProgress(1, detail = glue::glue("Creating {{riskmetric}} Reference {uploaded_packages$package[i]} V{user_ver}"))
            if (grepl("^[[:alpha:]][[:alnum:].]*[[:alnum:]]$", uploaded_packages$package[i])) {
              # run pkg_ref() to get pkg version and source info
              if (!isTRUE(getOption("shiny.testmode")))
                ref <- riskmetric::pkg_ref(uploaded_packages$package[i],
                                           lib.loc = assessment_lib$location)
              else
                ref <- test_pkg_refs[[uploaded_packages$package[i]]]
            } else {
              ref <- list(name = uploaded_packages$package[i],
                          source = "name_bad")
            }
            
            if (ref$source %in% c("pkg_missing", "name_bad")) {
              incProgress(1, detail = 'Reference Error {uploaded_packages$package[i]} V{user_ver}')
              
              # Suggest alternative spellings using utils::adist() function
              v <- utils::adist(uploaded_packages$package[i], cran_pkgs(), ignore.case = FALSE)
              rlang::inform(paste("Package name",uploaded_packages$package[i],"was not found."))
              
              suggested_nms <- paste("Suggested package name(s):",paste(head(cran_pkgs()[which(v == min(v))], 10),collapse = ", "))
              rlang::inform(suggested_nms)
              
              uploaded_packages$status[i] <- HTML(paste0('<a href="#" title="', suggested_nms, '">not found</a>'))
              
              if (ref$source == "pkg_missing")
                loggit::loggit('WARN',
                               glue::glue('Package {ref$name} was flagged by {{riskmetric}} as {ref$source}.'))
              else
                loggit::loggit('WARN',
                               glue::glue("Riskmetric can't interpret '{ref$name}' as a package reference."))
              
              next
            }
            
            ref_ver <- as.character(ref$version)
            
            if(user_ver == ref_ver) ver_msg <- glue::glue('V{ref_ver}')
            else ver_msg <- glue::glue("V{ref_ver}, not V{user_ver}")
            
            as.character(ref$version)
            deets <- glue::glue("{uploaded_packages$package[i]} {ver_msg}")
            
            # Save version.
            incProgress(1, detail = glue::glue('Uploading {deets}'))
            uploaded_packages$version[i] <- as.character(ref$version)
            
            found <- nrow(dbSelect(
              "SELECT name
              FROM package
              WHERE name = {uploaded_packages$package[i]}"))
            
            uploaded_packages$status[i] <- ifelse(found == 0, 'new', 'duplicate')

            # Add package and metrics to the db if package is not in the db.
            if(!found) {
              # Get and upload pkg general info to db.
              incProgress(1, detail = glue::glue('Uploading Package Information {deets}'))
              insert_pkg_info_to_db(uploaded_packages$package[i], assessment_lib$location)
              # Get and upload maintenance metrics to db.
              incProgress(1, detail = glue::glue('Uploading {{riskmetric}} Metrics {deets}'))
              insert_riskmetric_to_db(pkg_ref = ref)
              # Get and upload community metrics to db.
              incProgress(1, detail = glue::glue('Uploading Community Metrics {deets}'))
              insert_community_metrics_to_db(uploaded_packages$package[i])
              uploaded_packages$score[i] <- get_pkg_info(uploaded_packages$package[i])$score
              if (!rlang::is_empty(auto_list())) {
                uploaded_packages$decision[i] <-
                  assign_decisions(auto_list(), uploaded_packages$package[i])
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
      
      DT::datatable(
        uploaded_pkgs(),
        escape = FALSE,
        class = "cell-border",
        selection = 'none',
        extensions = 'Buttons',
        options = list(
          searching = FALSE,
          sScrollX = "100%",
          lengthChange = TRUE,
          aLengthMenu = list(c(5, 10, 20, 100, -1), list('5', '10', '20', '100', 'All')),
          iDisplayLength = 10
        )
      )
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
    
    list(
      names = uploaded_pkgs,
      auto_decision = auto_list
    )
  })
}
