# -----------------------------------------------------------------------------
# ------------ INTROJS TEXT
# -----------------------------------------------------------------------------

# Sidebar metrics.
sidebar_steps <-
  data.frame(
    element = c(
      "#sidebar-select_pkg_ui", # "#sidebar-select_ver", # not working
      "#sidebar-status-wp", "#sidebar-score-wp",
      "#sidebar-decision-grp",
      "#sidebar-overall-comment-grp"),
    intro = c(
      "Expand this dropdown list to select a specific package to assess that was previously uploaded.",
      # "The latest version will autopopulate here.", # can't peg ui object name, even with a div!
      "The status can be either 'Under Review' or 'Reviewed'.",
      "The score can take any value between 0 (no risk) and 1 (highest risk).",
      "Provide your input on the overall risk of the selected package.",
      "After reviewing your package, you can leave an overall comment."
    ),
    position = c(rep("bottom", 5))
  )

# appui tab widgets
apptab_steps <- data.frame(
  element = c(
  "[data-value=assessment-criteria-tab]", "[data-value=database-tab]"),
  intro = c("Discover the package assessment process & criteria",
            "Review the R packages that already exist in the database"),
  position = c(rep("bottom", 2))
)

apptab_admn <- data.frame(
  element = c(
    "[data-value=admin-mode-tab]"),
  intro = c("Manage user credentials and apply assessment reweighting here"),
  position = "bottom"
)

# upload package tab.
upload_pkg <- data.frame(
  element = c("#upload_package-introJS-help", "#type-package-group", "#upload-file-grp", "#upload_package-upload_format"),
  intro = c(
    "Click here anytime you need help.",
    "Type in the name of the package(s) you would like to assess.",
    "Or you can Upload a CSV file with the package(s) if you have a lot.",
    "Follow format of this sample data when creating your csv. Or you can even download it to use as a template."
  ),
  position = c("right", rep("top", 3))
)

upload_adm <- data.frame(
  element = c("#rem-package-group", "#upload_package-automate-auto_dropdown"),
  intro = c("If you are an admin, you can also remove packages here.", "Decision automation: Automatically assign package risk decisions based on {riskmetric}-derived risk scores when uploading packages to the database."),
  position = c("top", "left")
)

upload_pkg_comp <-  data.frame(
      element = c("#upload_summary_div", "#upload_package-upload_pkgs_table"),
      intro = c(
        "Text description of packages uploaded. Counts by type: 'Total', 'New', 'Undiscovered', 'Duplicate'.",
        "Confirm uploaded packages list, filter by type"
      ),
      position = c("bottom", "top")
)
      
upload_pkg_complete <- union(upload_pkg,
                             data.frame(
                               element = c("#upload_summary_div", "#upload_package-upload_pkgs_table"),
                               intro = c(
                                 "Text description of packages uploaded. Counts by type: 'Total', 'New', 'Undiscovered', 'Duplicate'.",
                                 "Confirm uploaded packages list, filter by type"
                               ),
                               position = c("bottom", "top")
                             )
)


# Maintenance metrics.
mm_steps <- 
  data.frame(
    element = c(".card-group", "#comments_for_mm"),
    intro = c(
      "Several ways to measuring package maintenance are assessed here. Please review each!",
      "Have something to share within your organization? Add a comment and keep track of the on-going conversation for this package's maintainence metrics"
    ),
    position = c(rep("left", 2))
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
    "Have something to share within your organization? Add a comment and keep track of the on-going conversation for this package's community usage metrics"
  ),
  position = c(rep("left", 3))
  
)


