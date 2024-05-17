

# First, make sure data frame has the columns we expect
test_that("team_info_df is in expected format", {
  
  testthat::expect_true(all(c("name", "role", "site", "org", "photo_file") %in% names(team_info_df)))
  
})

# Make sure the png's are squares
test_that("Pngs formatted correctly", {
  
  if(rlang::is_installed("png")) {
    purrr::walk(team_info_df$photo_file, function(x){
      img <- png::readPNG(app_sys("app","www","images",x))
      testthat::expect_equal(dim(img)[1], dim(img)[2]) # insures they are square images
    })
  }
  
})


test_that("contrib_card() works", {

  # # custom
  # contrib_card(
  #   role = "Workstream Lead",
  #   pic = 'inst/app/www/images/aaron_clark.png',
  #   site = 'https://github.com/aclark02-arcus',
  #   name = "Aaron Clark",
  #   org = "Arcus Biosciences")
  # 
  # # based on team_info_Df
  # team_member1 <- contrib_card(
  #   role = team_info_df$role[1],
  #   pic = NULL,
  #   site = team_info_df$site[1],
  #   name = team_info_df$name[1],
  #   org = team_info_df$org[1])
  # team_member1
  # class(team_member1)  
  # attributes(team_member1)
  
})



test_that("make_contrib_cards() works", {
  
  # make_contrib_cards(
  #   team_info_df |>
  #      filter(status %in% "current") 
  #   )
  # 
  # make_contrib_cards(team_info_df %>% filter(status %in% "past"))
  # class(team_member1)  
  # attributes(team_member1)
  
})

