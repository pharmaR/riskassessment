library(testthat)
library(riskassessment)
# Load application support files into testing environment
library(shinytest2)
options(shinytest2.load_timeout = 30*1000)
options(shinytest2.timeout = 30*1000)

test_check("riskassessment")
