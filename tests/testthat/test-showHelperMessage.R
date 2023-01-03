
test_that(desc = "Check output of showHelperMessage()", 
          code = {
            msg <- showHelperMessage(message = "This is a test message!")
            testthat::expect_equal(object = msg$children[[1]][1], expected = "This is a test message!")
            testthat::expect_equal(object = msg$name, expected = "h6")
            
            ## check HTML attributes of the message
            attribs <- stringr::str_split(msg$attribs$style, pattern = "[;\n]")[[1]]  ## split at either semi-colon OR newline
            testthat::expect_equal(object = attribs[1], expected = "text-align: center")
            testthat::expect_equal(object = stringr::str_split(string = attribs[3], pattern = "^\\s+")[[1]][2], 
                                   expected = "color: gray")  ## split at one or more white spaces in the beginning
            testthat::expect_equal(object = stringr::str_split(string = attribs[5], pattern = "^\\s+")[[1]][2], 
                                   expected = "padding-top: 50px")
          })