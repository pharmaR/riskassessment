## code to prepare `upload_format` dataset goes here
upload_format <- readr::read_csv("data-raw/upload_format.csv")
usethis::use_data(upload_format, overwrite = TRUE)
