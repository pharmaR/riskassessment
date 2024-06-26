# -----------------------------------------------------------------------------
# ------------ INTROJS TEXT
# -----------------------------------------------------------------------------

# Sidebar metrics.
sidebar_steps <-
  data.frame(
    element = c(
      "#sidebar-select_pkg_ui",
      "#sidebar-status-wp", "#sidebar-score-wp",
      "#sidebar-decision-grp",
      "#sidebar-overall-comment-grp"),
    intro = c(
      "Expand this dropdown list to select a specific package to assess that was previously uploaded.",
      "The status can be either 'Under Review' or 'Reviewed'.",
      "The score can take any value between 0 (no risk) and 1 (highest risk).",
      "Provide your input on the overall risk of the selected package (PRIVILEGES REQUIRED).",
      "After reviewing your package, you can leave an overall comment (PRIVILEGES REQUIRED)."
    ),
    position = c(rep("bottom", 5))
  )

# appui tab widgets
apptab_steps <- data.frame(
  element = c(
    "[data-value=about-tab]", "[data-value=database-tab]"),
  intro = c("Discover more about the project, including the package assessment process & how to contact the maintainers.",
            "Review the R packages that already exist in the database"),
  position = c(rep("bottom", 2))
)

apptab_admn <- data.frame(
  element = c(
    "[data-value=admin-mode-tab]"),
  intro = c("Manage user credentials and apply assessment reweighting here (PRIVILEGES REQUIRED)."),
  position = "bottom"
)

# upload package tab.
upload_pkg <- data.frame(
  element = c("#upload_package-introJS-help", "#type-package-group", "#upload-file-grp"),
  intro = c(
    "Click here anytime you need help.",
    "Type in the name of the package(s) you would like to assess (PRIVILEGES REQUIRED).",
    "Or you can Upload a CSV file with the package(s) if you have a lot (PRIVILEGES REQUIRED)."
  ),
  position = c("right", rep("top", 2))
)
upload_pkg_add <- data.frame(
  element = "#upload_package-upload_format",
  intro = "Follow format of this sample data when creating your csv. Or you can even download it to use as a template. Note: the `decision` column is optional and will only work for users with the correct privileges.",
  position = "top"
)

upload_pkg_delete <- data.frame(
  element = "#rem-package-group",
  intro = "You can remove packages from the database here (PRIVILEGES REQUIRED).",
  position = "top"
)

upload_pkg_dec_adj <- data.frame(
  element = "#automate-auto_dropdown",
  intro = "Decision automation: Automatically assign package risk decisions based on {riskmetric}-derived risk scores when uploading packages to the database. (PRIVILEGES REQUIRED)",
  position = "left"
)
upload_pkg_comp <-  data.frame(
  element = c("#upload_summary_div", "#upload_package-upload_pkgs_table"),
  intro = c(
    "An overall summary of packages uploaded, giving you a synopsis of your most recent upload request.",
    "A detailed upload summary, by package, including a quick look at scores and any automated decisions made. Click the 'Explore Metrics' button to dive deeper into any one assessment!"
  ),
  position = c("bottom", "top")
)

# Maintenance metrics.
mm_steps <- 
  data.frame(
    element = c(".card-group", "#comments_for_mm"),
    intro = c(
      "Several ways to measuring package maintenance are assessed here. Please review each!",
      "Have something to share within your organization? Add a comment and keep track of the ongoing conversation for this package's maintainence metrics"
    ),
    position = c(rep("left", 2))
  )

# Package Explorer
pe_steps <- 
  data.frame(
    element = c("#pkg_explorer-file_tree", "#pkg_explorer-file_editor","#pkg_explorer-comments_for_se"),
    intro = c(
      "This 'file tree' displays all files the author included in the R package build. Select one to preview the contents of the file or click the '+' to expand directories to explore more files. Note: not all file types can be previewed.",
      "Review the contents of a file. Use this viewer to explore code, documentation, or tests. Note: not all file types can be previewed.",
      "Have something to share within your organization? Add a comment and keep track of the ongoing conversation for this package's file browser."
    ),
    position = c("left","left","top")
  )

# Function Explorer
fe_steps <- 
  data.frame(
    element = c("#code_explorer-function_list", "#code_explorer-file_type_div","#code_explorer-file_list","#code_explorer-file_viewer","#code_explorer-comments_for_fe"),
    intro = c(
      "A list of exported functions. Click to view and select.",
      "After selecting a function, choose which kind of files you want to preview: tests, source code, or help docs.",
      "If multiple files are found, they'll be displayed here. Click through each to see how the function is defined & used throughout the package.",
      "Observe function usage with this handy file viewer. The selected function will be highlighted for your convenience.",
      "Have something to share within your organization? Add a comment and keep track of the ongoing conversation for this package's exported functions."
    ),
    position = c(rep("right", 3), rep("top", 2))
  )


# Report Preview.
rp_steps <- data.frame(
  element = c( "#dwnld_rp", "#rep_prev"),
  intro = c(
    "Select file output type for report seen below and download for later use",
    "The current assessment of this package including your comments and overall decision have been collected from the other tabs to prepare the following report for convenience."
  ),
  position = c("left", "left")
)


# Community Usage Metrics.
cum_steps <- data.frame(
  element = c("#cum_infoboxes", "#cum_plot", "#comments_for_cum"),
  intro = c(
    "Several ways of measuring community usage are assessed here. Please review each!",
    "Digest the 'downloads per month' metric by selecting a pre-defined time periods or toggling the date slider at bottom of plot for custom date range",
    "Have something to share within your organization? Add a comment and keep track of the ongoing conversation for this package's community usage metrics"
  ),
  position = c(rep("left", 3))
  
)


