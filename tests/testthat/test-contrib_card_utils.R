

# First, make sure data frame has the columns we expect
test_that("team_info_df has the 'most crucial' columns", {
  
  testthat::expect_true(all(c("name", "role", "site", "org", "photo_file") %in% names(team_info_df)))
  
})

# Make sure the png's are squares
test_that("Pngs formatted as squares", {
  
  if(rlang::is_installed("png")) {
    purrr::walk(team_info_df$photo_file, function(x){
      img <- png::readPNG(app_sys("app","www","images",x))
      testthat::expect_equal(dim(img)[1], dim(img)[2]) # insures they are square images
    })
  }
  
})


test_that("contrib_card() produces bslib shiny.tag", {

  # custom
  team_member1 <- contrib_card(
    role = "Workstream guy",
    pic = NULL,
    site = 'https://github.com/pharmar/riskassessment',
    name = "Some Guy",
    org = "Some Company")

  testthat::expect_equal(class(team_member1),
                         c("bslib_fragment","shiny.tag"))
  
})



test_that("make_contrib_cards() works", {
  
  if(all(c("name", "role", "site", "org", "photo_file") %in% names(team_info_df))) {

    past <- make_contrib_cards(team_info_df %>% filter(status %in% "past"))
    testthat::expect_equal(class(past),
                           c("bslib_fragment","shiny.tag"))
  }
  
})

