library(testthat)
library(shiny)
library(riskassessment)
# Load application support files into testing environment
library(shinytest2)
options(shinytest2.load_timeout = 30*1000)
options(shinytest2.timeout = 30*1000)

chromote::set_chrome_args(
  c(
    chromote::default_chrome_args(),
    "--no-sandbox"
  )
)
chromote::set_default_chromote_object(chromote::Chromote$new())

test_check("riskassessment")
