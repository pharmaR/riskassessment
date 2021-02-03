# Default infobox settings.
width <- 4
fill <- TRUE

# Infobox for has_vignettes.
has_vignettes_infobox <- function(values) {
  req(values$has_vignettes)
  
  has_metric <- !(values$has_vignettes %in% c("NA", "pkg_metric_error"))
  
  # Total number of vignettes.
  value <- as.numeric(values$has_vignettes)
  
  infoBox(
    width = width,
    fill = fill,
    title = "Presence of vignettes?",
    icon = icon(
      ifelse(has_metric && value >= 1, "thumbs-up", "thumbs-down"),
      lib = "glyphicon"
    ),
    color = ifelse(has_metric && value >= 1, "green", "red"),

    # Output
    #   YES (if metric has value),
    #   NO (if metric doesnt have any value),
    #   or NA (if metric equals NA or pkg_metric_error).
    if(!has_metric){"NA"}
    else if(value == 0){"NO"}
    else{"YES"},
    
    # Output metric value if it exists or
    #   a generic message if NA or error occurred.
    if(!has_metric) "Metric is not applicable for this source of package"
    else
      paste("The package has", value, if(value == 1) "vignette" else "vignettes")
  )
}

# Infobox for has_website.
has_website_infobox <- function(values) {
  req(values$has_website)
  
  has_metric <- !(values$has_website == "pkg_metric_error")
  
  # URL(s) of the package website(s).
  value <- values$has_website
  
  infoBox(
    width = width,
    fill = fill,
    title = "Associated website URL?",
    icon = icon(
      ifelse(has_metric && value != "NA", "thumbs-up", "thumbs-down"),
      lib = "glyphicon"
    ),
    color = ifelse(has_metric && value != "NA", "green", "red"),
    
    # Output
    #   YES (if metric has value),
    #   NO (if metric doesnt have any value),
    #   or NA (if metric equals NA or pkg_metric_error).
    if(!has_metric){"NA"}
    else if(value == "NA"){"NO"}
    else{"YES"},
    
    # Output metric value if it exists or
    #   a generic message if NA or error occurred.
    if(!has_metric) "Metric is not applicable for this source of package"
    else
      ifelse(value != "NA", paste("Website:", value),
             "The package does not have an associated website URL")
  )
}

# Infobox for has_news.
has_news_infobox <- function(values){
  req(values$has_news)
  
  has_metric <- !(values$has_news %in% c("NA", "pkg_metric_error"))
  
  # "1": if it has news, "0": otherwise.
  value <- values$has_news
  
  infoBox(
    width = width,
    fill = fill,
    title = "NEWS?",
    icon = icon(
      ifelse(has_metric && value == "1", "thumbs-up", "thumbs-down"),
      lib = "glyphicon"
    ),
    color = ifelse(has_metric && value == "1", "green", "red"),
    
    # Output
    #   YES (if metric has value),
    #   NO (if metric has value "0"),
    #   or NA (if metric equals NA or pkg_metric_error).
    if(!has_metric){"NA"}
    else if(value == "0"){"NO"}
    else{"YES"},
    
    # Output an affirmative/nevative message if metric's valus is "1"/"0" or
    #   a generic message if NA or error occurred.
    if(!has_metric) "Metric is not applicable for this source of package"
    else
      paste("The package", ifelse(value == "1", "has", "does not have"),
               "a NEWS file")
  )
}

# Infobox for news_current.
news_current_infobox <- function(values){
  req(values$news_current)
  
  has_metric <- !(values$news_current %in% c("NA", "pkg_metric_error"))
  
  # "TRUE": if news file is current, "FALSE": otherwise.
  value <- values$news_current
  
  infoBox(
    width = width,
    fill = fill,
    title = "News is current?",
    icon = icon(
      ifelse(has_metric && value == "TRUE", "thumbs-up", "thumbs-down"),
      lib = "glyphicon"
    ),
    color = ifelse(!has_metric, "black", ifelse(value == "TRUE", "green", "red")),
    
    # Output
    #   YES (if metric has value "TRUE"),
    #   NO (if metric has value "FALSE"),
    #   or NA (if metric equals NA or pkg_metric_error).
    if(!has_metric){"NA"}
    else if(value == "FALSE"){"NO"}
    else{"YES"},
    
    # Output an affirmative/nevative message if metric's valus is "TRUE"/"FALSE" or
    #   a generic message if NA or error occurred.
    if(!has_metric) "Metric is not applicable for this source of package"
    else
      paste("NEWS file",
            ifelse(value == "TRUE", "contains", "does not contain"),
            "an entry for the current version number")
  )
}

# Infobox for has_bug_reports_url.
has_bug_reports_url_infobox <- function(values){
  req(values$has_bug_reports_url)
  
  has_metric <- !(values$has_bug_reports_url == "pkg_metric_error")
  
  # URL(s) of the the bug reports.
  value <- values$has_bug_reports_url
  
  infoBox(
    width = width,
    fill = fill,
    title = "Bugs publicly documented?",
    icon = icon(
      ifelse(has_metric && value != "NA", "thumbs-up", "thumbs-down"),
      lib = "glyphicon"
    ),
    color = ifelse(has_metric && value != "NA", "green", "red"),
    
    # Output
    #   YES (if metric has value),
    #   NO (if metric doesnt have any value),
    #   or NA (if metric equals NA or pkg_metric_error).
    if(!has_metric){"NA"}
    else if(value == "NA"){"NO"}
    else{"YES"},
    
    # Output an affirmative/nevative message if metric has a value.
    if(!has_metric) "Metric is not applicable for this source of package"
    else
      ifelse(value != "NA", paste("Bug reports URL:", value),
                "The bugs are not publicly documented")
  )
}

# Infobox for bugs_status.
bugs_status_infobox <- function(values){
  req(values$bugs_status)
  
  has_metric <- !(values$bugs_status %in% c("NA", "pkg_metric_error"))
  
  # Percentage of last 30 bugs closed.
  value <- values$bugs_status
  
  infoBox(
    width = width,
    fill = fill,
    title = "Bug closure",
    color = ifelse(has_metric && value != "", "blue", "black"),
    
    # Output
    #   value% (if metric has a value),
    #   or NA (if metric equals doesnt have a value
    #   or euqls NA or pkg_metric_error).
    if(!has_metric || value == ""){"NA"}
    else if(value != ""){paste0(value, "%")},
    
    # Output metric percentage value or generic message.
    if(!has_metric) "Metric is not applicable for this source of package"
    else "Percentage of last 30 bugs closed"
  )
}

# Infobox for export_help.
export_help_infobox <- function(values){
  req(values$export_help)
  
  has_metric <- !(values$export_help %in% c("NA", "pkg_metric_error"))
  
  # Percentage of exported objects.
  value <- values$export_help
  
  infoBox(
    width = width,
    fill = fill,
    title = "Documentation",
    color = ifelse(has_metric && value != "", "blue", "black"),
    
    # Output
    #   value% (if metric has a value),
    #   or NA (if metric equals doesnt have a value
    #   or euqls NA or pkg_metric_error).
    if(!has_metric || value == ""){"NA"}
    else if(value != ""){paste0(value, "%")},
    
    # Output metric percentage value or generic message.
    if(!has_metric) "Metric is not applicable for this source of package"
    else "Percentage of exported objects documented"
  )
}

# Infobox for has_source_control.
has_source_control_infobox <- function(values){
  req(values$has_source_control)
  
  has_metric <- !(values$has_source_control == "pkg_metric_error")
  
  # URL of public source code.
  value <- values$has_source_control
  
  infoBox(
    width = width,
    fill = fill,
    title = "Source code public?",
    icon = icon(
      ifelse(has_metric && value != "NA", "thumbs-up", "thumbs-down"),
      lib = "glyphicon"
    ),
    color = ifelse(has_metric && value != "NA", "green", "red"),
    
    # Output
    #   YES (if metric has value),
    #   NO (if metric doesnt have any value),
    #   or NA (if metric equals NA or pkg_metric_error).
    if(!has_metric){"NA"}
    else if(value == "NA"){"NO"}
    else{"YES"},
    
    # Output an affirmative/nevative message if metric has a value.
    if(!has_metric) "Metric is not applicable for this source of package"
    else{
      ifelse(value != "NA", paste("Source code URL:", value),
                "Package does not have a source code URL")}
  )
}

# Infobox for has_maintainer.
has_maintainer_infobox <- function(values){
  req(values$has_maintainer)
  
  has_metric <- !(values$has_maintainer %in% c("NA", "pkg_metric_error"))
  
  # Maintainer name(s).e
  value <- values$has_maintainer
  
  infoBox(
    width = width,
    fill = fill,
    title = "Has a maintainer?",
    icon = icon(
      ifelse(
        has_metric && value != "", "thumbs-up", "thumbs-down"),
      lib = "glyphicon"
    ),
    color = ifelse(has_metric && value != "", "green", "red"),
    
    # Output
    #   YES (if metric has value),
    #   NO (if metric doesnt have any value),
    #   or NA (if metric equals NA or pkg_metric_error).
    if(!has_metric){"NA"}
    else if(value == ""){"NO"}
    else{"YES"},
    
    # Output an affirmative/nevative message if metric has a value.
    if(!has_metric) "Metric is not applicable for this source of package"
    else ifelse(value != "", value, "Package does not have a maintainer")
  )
}