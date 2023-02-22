
# TODO: error check; support non-CRAN
install_to_tmp_lib <- function(pkg_name, pkg_version = NA,
                               lib_loc = get_assessment_lib_path()) {
  if (is.na(pkg_version)) {
    utils::install.packages(pkg_name, lib = lib_loc)
  } else {
    devtools::install_version(pkg_name, pkg_version, lib = lib_loc)
  }
}


assess_pkg_install <- function(pkg_name, lib_loc) {
  ref <- riskmetric::pkg_ref(
    x = pkg_name,
    source = "pkg_install",
    lib.loc = lib_loc
  )

  assess <- ref %>%
    dplyr::as_tibble() %>%
    riskmetric::pkg_assess()

  assess
}


get_assessment_lib_path <- function() {
  assessment_lib_path <- get_golem_config("assessment_lib")
  if (!file.exists(assessment_lib_path)) {
    dir.create(assessment_lib_path)
  }
  assessment_lib_path
}
