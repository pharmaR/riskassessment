#' asynch 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
report_creation <- function(pkg_lst, metric_weights, report_format, report_includes, user, 
                            db_name = golem::get_golem_options('assessment_db_name'), 
                            my_tempdir = tempdir(), updateProgress = NULL) {
  n_pkgs <- length(pkg_lst)
  
  if (report_format == "html") {
    
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
  else if (report_format == "docx") { 
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
    if (is.function(updateProgress))
      updateProgress(1, paste("Processing", pkg_lst[i]))
    # Grab package name and version, then create filename and path.
    # this_pkg <- "stringr" # for testing
    selected_pkg <- get_pkg_info(pkg_lst[i], db_name)
    this_pkg <- selected_pkg$name
    this_ver <- selected_pkg$version
    file_named <- glue::glue('{this_pkg}_{this_ver}_Risk_Assessment.{report_format}')
    path <- if (n_pkgs > 1) {
      file.path(my_tempdir, file_named)
    } else {
      NULL
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
    overall_comments <- get_overall_comments(this_pkg, db_name)
    pkg_summary <- get_pkg_summary(this_pkg, db_name)
    mm_comments <- get_mm_comments(this_pkg, db_name)
    cm_comments <- get_cm_comments(this_pkg, db_name)
    se_comments <- get_se_comments(this_pkg, db_name)
    fe_comments <- get_fe_comments(this_pkg, db_name)
    
    # gather maint metrics & community metric data
    mm_data <- get_metric_data(this_pkg, metric_class = "maintenance", db_name)
    comm_data <- get_comm_data(this_pkg, db_name)
    comm_cards <- build_comm_cards(comm_data, db_name)
    downloads_plot <- build_comm_plotly(comm_data)
    metric_tbl <- dbSelect("select * from metric", db_name = db_name)
    
    
    # Render the report, passing parameters to the rmd file.
    out <-
      rmarkdown::render(
        input = Report,
        output_file = path,
        clean = FALSE,
        params = list(pkg = pkg_list,
                      report_includes = report_includes,
                      riskmetric_version = paste0(packageVersion("riskmetric")),
                      app_version = golem::get_golem_options('app_version'),
                      metric_weights = metric_weights,
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
                      metric_tbl = metric_tbl
        )
      )
    fs <- c(fs, path)  # Save all the reports/
  }
  # Zip all the files up. -j retains just the files in zip file.
  if (n_pkgs > 1) zip(zipfile = my_tempdir, files = fs, extras = "-j")
  if (is.function(updateProgress))
    updateProgress(1, "**Finished**")
  
  if (n_pkgs > 1) paste0(my_tempdir, ".zip") else out
}

