pkg_dir <- 
c("build/vignette.rds", "DESCRIPTION", "inst/doc/magrittr.html", 
  "inst/doc/magrittr.R", "inst/doc/magrittr.Rmd", "inst/doc/tradeoffs.html", 
  "inst/doc/tradeoffs.R", "inst/doc/tradeoffs.Rmd", "inst/logo-hex.png", 
  "inst/logo-hex.svg", "inst/logo.png", "inst/logo.svg", "LICENSE", 
  "man/aliases.Rd", "man/compound.Rd", "man/debug_fseq.Rd", "man/debug_pipe.Rd", 
  "man/exposition.Rd", "man/faq-pipe-gender.Rd", "man/figures/exposition-1.png", 
  "man/figures/logo.png", "man/freduce.Rd", "man/fseq.Rd", "man/functions.Rd", 
  "man/magrittr-package.Rd", "man/pipe-eager.Rd", "man/pipe.Rd", 
  "man/pipe_eager_lexical.Rd", "man/print.fseq.Rd", "man/tee.Rd", 
  "MD5", "NAMESPACE", "NEWS.md", "R/aliases.R", "R/debug_pipe.R", 
  "R/freduce.R", "R/functions.R", "R/getters.R", "R/magrittr.R", 
  "R/pipe.R", "README.md", "src/pipe.c", "src/utils.c", "src/utils.h", 
  "tests/testthat.R", "tests/testthat/_snaps/pipe.md", "tests/testthat/test-aliases.R", 
  "tests/testthat/test-anonymous-functions.r", "tests/testthat/test-compound.R", 
  "tests/testthat/test-freduce.R", "tests/testthat/test-fseq.r", 
  "tests/testthat/test-multiple-arguments.r", "tests/testthat/test-pipe.R", 
  "tests/testthat/test-single-argument.r", "tests/testthat/test-tee.r", 
  "vignettes/magrittr.jpg", "vignettes/magrittr.Rmd", "vignettes/tradeoffs.Rmd"
)

test_that("Test that make_nodes() works", {
  leaves <- make_nodes(pkg_dir)
  
  expect_equal(
    names(leaves), 
    c("build", "DESCRIPTION", "inst", "LICENSE", "man", "MD5", "NAMESPACE", "NEWS.md", "R", "README.md", "src", "tests", "vignettes")
  )
  
  expect_equal(length(leaves[["tests"]][["testthat"]]), 10)
  
  expect_equal(leaves[["DESCRIPTION"]], structure("", sttype = "file"))
  
  expect_equal(leaves[["build"]][["vignette.rds"]], structure("", sttype = "file"))
})

leaves <- list(
  DESCRIPTION = "DESCRIPTION", LICENSE = "LICENSE", 
  MD5 = "MD5", NAMESPACE = "NAMESPACE", 
  NEWS.md = "NEWS.md", 
  R = list(
    aliases.R = "aliases.R", 
    debug_pipe.R = "debug_pipe.R", freduce.R = "freduce.R", 
    functions.R = "functions.R", getters.R = "getters.R", 
    magrittr.R = "magrittr.R", pipe.R = "pipe.R"), 
  README.md = "README.md", 
  src = list(pipe.c = "pipe.c", utils.c = "utils.c", utils.h = "utils.h"), 
  tests = list(
    testthat.R = "testthat.R", 
    testthat = list(
      `_snaps` = list(pipe.md = "pipe.md"), 
      `test-aliases.R` = "test-aliases.R", `test-anonymous-functions.r` = "test-anonymous-functions.r", 
      `test-compound.R` = "test-compound.R", `test-freduce.R` = "test-freduce.R", 
      `test-fseq.r` = "test-fseq.r", `test-multiple-arguments.r` = "test-multiple-arguments.r", 
      `test-pipe.R` = "test-pipe.R", `test-single-argument.r` = "test-single-argument.r", 
      `test-tee.r` = "test-tee.r"))
)

test_that("Test that get_list_element() works", {
  expect_equal(get_list_element("anything", NULL), NULL)
  
  expect_equal(get_list_element(NULL, leaves), leaves)
  
  expect_equal(get_list_element(list(DESCRIPTION = ""), leaves), "DESCRIPTION")
  
  expect_equal(get_list_element(list(tests = list(testthat = list("test-aliases.R" = ""))), leaves), "test-aliases.R")
  
  expect_equal(get_list_element(list(build = list()), leaves), NULL)
  
  expect_equal(get_list_element(list(R = ""), leaves), leaves[["R"]])
})

test_that("Test that get_selected_path() works", {
  expect_equal(get_selected_path(leaves), "DESCRIPTION")
  
  expect_equal(get_selected_path(leaves["R"]), "R/aliases.R")
  
  expect_equal(get_selected_path(list(tests = list("testthat.R" = ""))), "tests/testthat.R")
})
