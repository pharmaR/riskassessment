library(testthat)
library(shiny)
library(riskassessment)
# Load application support files into testing environment
library(shinytest2)
options(shinytest2.load_timeout = 45*1000)
options(shinytest2.timeout = 45*1000)

chromote::set_chrome_args(
  c(
    chromote::default_chrome_args(),
    "--no-sandbox"
  )
)

tmpt <- Sys.time()
while (!chromote::has_default_chromote_object() && Sys.time() - tmpt < 1) {
  try(chromote::set_default_chromote_object(chromote::Chromote$new()), silent = TRUE)
}

test_check("riskassessment")
test_file("tests/testthat/test-downloadHandler.R")
