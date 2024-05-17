test_that("contrib_card() works", {
  
  # First, make sure data frame has the columns we expect
  testthat::expect_true(all(c("name", "role", "site", "org", "photo_file") %in% names(team_info_df)))
  
  # contrib_card(
  #   role = team_info_df$role[1],
  #   pic = team_info_df$photo_file[1],
  #   site = team_info_df$site[1],
  #   name = team_info_df$name[1],
  #   org = team_info_df$org[1])
  # 
  contrib_card(
    role = team_info_df$role[1],
    pic = app_sys("app","www","images",team_info_df$photo_file[1]),
    # pic = NULL,
    site = team_info_df$site[1],
    name = team_info_df$name[1],
    org = team_info_df$org[1])
  
})



me <- contrib_card(
  role = "Workstream Lead",
  pic = 'inst/app/www/images/aaron_clark.png',
  site = 'https://github.com/aclark02-arcus',
  name = "Aaron Clark",
  org = "Arcus Biosciences")
jeff <- contrib_card(
  role = "Core Contributor",
  pic = 'inst/app/www/images/jeff_thompson.png',
  site = 'https://github.com/jthompson-arcus',
  name = "Jeff Thompson",
  org = "Arcus Biosciences")
robert <- contrib_card(
  role = "Core Contributor",
  pic = 'inst/app/www/images/robert_krajcik.png',
  site = 'https://github.com/jthompson-arcus',
  name = "Robert Krajcik",
  org = "Cytel")
ph <- contrib_card(
  role = "Core Contributor",
  pic = 'inst/app/www/images/person_placeholder.png',
  site = 'https://github.com/jthompson-arcus',
  name = "Barbara Mikulasova ",
  org = "Katalyze Data")