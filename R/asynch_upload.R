asynch_upload <- function(uploaded_pkgs) {
  # Load required packages.
  source("global.R")
  
  # Load source files.
  source(file.path("R", "dbupload.R"))
  source(file.path("R", "utils.R"))
  
  names(uploaded_pkgs) <- tolower(names(uploaded_pkgs))
  uploaded_pkgs$package <- trimws(uploaded_pkgs$package)
  uploaded_pkgs$version <- trimws(uploaded_pkgs$version)
  
  # Current packages on the db.
  curr_pkgs <- dbSelect("SELECT name FROM package")
  
  # Save the uploaded packages that were not in the db.
  new_pkgs <- uploaded_pkgs %>% dplyr::filter(!(package %in% curr_pkgs$name))
  
  if(nrow(new_pkgs) != 0){
    for (pkg in new_pkgs$package) {
      # Get and upload pkg general info to db.
      insert_pkg_info_to_db(pkg)
      # Get and upload maintenance metrics to db.
      insert_maintenance_metrics_to_db(pkg)
      # Get and upload community metrics to db.
      insert_community_metrics_to_db(pkg)
    }
  }

  all_pkgs <- dbSelect("SELECT name FROM package")

  # Data frame indicating which packages where duplicate, new, and not found.
  uploaded_pkgs <- uploaded_pkgs %>%
    mutate(status = case_when(
      !(package %in% all_pkgs$name) ~ 'not found',
      package %in% curr_pkgs$name ~ 'duplicate',
      TRUE ~ 'new')
    )

  return(uploaded_pkgs)
}
