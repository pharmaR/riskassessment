# -----------------------------------------------------------------------------
# ------------ INTROJS TEXT
# -----------------------------------------------------------------------------

# Upload package steps.
upload_pkg <- data.frame(
  element = c("#help", ".form-group > #uploaded_file-label", "#upload_format"),
  intro = c(
    "Click here anytime you need help.",
    "Upload a CSV file with the package(s) you would like to assess.",
    "You can use this sample dataset to explore the app."
  ),
  position = c("right", rep("top", 2))
)

# Sidebar metrics.
sidebar_steps <-
  data.frame(
    element = c("#assessment_criteria_bttn", "#db_dash_bttn", "#sel_pack", "#sel_ver",
                "#status", "#score", "#overall_comment", "#decision"),
    intro = c(
      "Click here to understand the package assessment process & criteria",
      "See an overview of the R packages that already exist in the database",
      "Click this dropdown to select assess a specific package",
      "The latest package version will autopopulate here.",
      "The status can be either 'Under Review' or 'Reviewed'.",
      "The score can take any value between 0 (no risk) and 1 (highest risk).",
      "After reviewing your package, you can leave an overall comment.",
      "Provide your input on the overall risk of the selected package."
    ),
    position = c(rep("left", 2), rep("bottom", 6))
  )

# Maintenance metrics.
mm_steps <- 
  data.frame(
    # Note that we access chooseCSVtext with '.' instead of '#', because we track its class and not its id.
    element = c("#mm_infoboxes", "#mm_add_comment", "#mm_prev_comments"),
    intro = c(
      "Several ways of measuring package maintenance best practices are assessed here. Please review!",
      "Have something to share within your organization? Add a comment.",
      "Keep track of the on-going conversation for this package's maintainence metrics"
    ),
    position = c(rep("top", 3))
  )


# Report Preview.
rp_steps <- data.frame(
  # Note that we access chooseCSVtext with '.' instead of '#', because we track its class and not its id.
  element = c( "#dwnld_rp", "#rep_prev"),
  intro = c(
    "Select file output type for report seen below and download for later use",
    "The current assessment of this package including your comments and overall decision have been collected from the other tabs to prepare the following report for convenience."
  ),
  position = c("left", "top")
)


# Community Usage Metrics.
cum_steps <- data.frame(
  # Note that we access chooseCSVtext with '.' instead of '#', because we track its class and not its id.
  element = c("#cum_infoboxes", "#cum_plot", "#cum_add_comment", "#cum_prev_comments"),
  intro = c(
    "Several ways of measuring community usage assessed here. Please review!",
    "Digest downloads per month by selecting a pre-defined time periods or toggling the date slider at bottom of plot for custom date range",
    "Have something to share within your organization? Add a comment.",
    "Keep track of the on-going conversation for this package's community usage"
  ),
  position = c("bottom", rep("top", 3))
  
)

upload_pkg <- data.frame(
  # Note that we access chooseCSVtext with '.' instead of '#', because we track its class and not its id.
  element = c("#help", ".chooseCSVtext", ".sample_dataset_link"),
  intro = c(
    "Click here anytime you need help.",
    "Upload a CSV file with the package(s) you would like to assess.",
    "You can use this sample dataset to explore the app."
  ),
  position = c("right", rep("top", 2))
)
