


#' Contributor Card
#' 
#' Generate a professional card containing role, a photo, name, site and org
#'
#' @param role character, role within the project, usually 'Workstream Lead',
#'   'Core Contributor', or 'Contributor'
#' @param pic character, file path to image, 1.3" in width
#' @param site character, if they have a personal of GitHub site to reference
#' @param name character, first and last name of the contributor
#' @param org character, a organization that employes this individual, if
#'   applicable
#'
#' @import shiny
#' @importFrom bslib card card_header card_body card_image card_title
#'
#' @examples
#' contrib_card(role = "Workstream Lead",
#'              pic = 'inst/app/www/images/aaron_clark.png',
#'              site = 'https://github.com/aclark02-arcus',
#'              name = "Aaron Clark",
#'              org = "Arcus Biosciences")
#'
#' @keywords internal
#' @family contrib_cards
#' 
contrib_card <- function(role, pic, site, name, org){
  bslib::card(
    bslib::card_header(
      class = "d-flex justify-content-between",
      role
    ),
    bslib::card_body(
      class = "align-items-center",
      gap = 0,
      fillable = TRUE,
      bslib::card_image(
        border_radius = "top",
        file = pic,
        href = ifelse(is.na(site) | is.null(site), 'https://www.pharmar.org', site),
        target = "_blank" # doesn't work as hoped
      ),
      bslib::card_title(name),
      tags$i(org)
    )
  )
}
# me <- contrib_card(role = "Workstream Lead",
#              pic = 'inst/app/www/images/aaron_clark.png',
#              site = 'https://github.com/aclark02-arcus',
#              name = "Aaron Clark",
#              org = "Arcus Biosciences")
# jeff <- contrib_card(role = "Core Contributor",
#                    pic = 'inst/app/www/images/jeff_thompson.png',
#                    site = 'https://github.com/jthompson-arcus',
#                    name = "Jeff Thompson",
#                    org = "Arcus Biosciences")
# robert <- contrib_card(role = "Core Contributor",
#                        pic = 'inst/app/www/images/robert_krajcik.png',
#                        site = 'https://github.com/jthompson-arcus',
#                        name = "Robert Krajcik",
#                        org = "Cytel")
# ph <- contrib_card(role = "Core Contributor",
#                    pic = 'inst/app/www/images/person_placeholder.png',
#                    site = 'https://github.com/jthompson-arcus',
#                    name = "Barbara Mikulasova ",
#                    org = "Katalyze Data")
# contrib_group <- list(me, jeff, robert, ph)
# bslib::layout_column_wrap(fixed_width = TRUE, height = 650, !!!contrib_group)






#' Make contributor cards
#'
#' Generate group of several 'contributor cards' from a data frame that contains
#' the following columns:
#' - role
#' - photo_file
#' - site
#' - name
#' -org
#'
#' @param df character, role within the project, usually 'Workstream Lead',
#'   'Core Contributor', or 'Contributor'
#'
#' @importFrom bslib layout_column_wrap
#' @importFrom purrr pmap
#'
#' @examples
#' make_contrib_cards(team_info_df %>% filter(status %in% "current"))
#' make_contrib_cards(team_info_df %>% filter(status %in% "past"))
#'
#' @keywords internal
#' @family contrib_cards
#' 
make_contrib_cards <- function(df = team_info_df){
  contrib_group <- purrr::pmap(df, function(role, photo_file, site, name, org, ...){
    contrib_card(
      role = role,
      pic = file.path('inst/app/www/images',photo_file),
      # pic = app_sys(file.path('www/images',photo_file)),
      site = site,
      name = name,
      org = org)
  })
  bslib::layout_column_wrap(fixed_width = TRUE, !!!contrib_group)
}









