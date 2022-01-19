## code to prepare `community` dataset goes here

community <- readr::read_csv("data-raw/community.csv")
usethis::use_data(community, overwrite = TRUE)
