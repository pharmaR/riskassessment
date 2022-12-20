describe("metricBox.R", {
  describe("metricBoxUI", {
    it("should return a Shiny UI element", {
      ui_element <- metricBoxUI("1234")
      
      expect_true(!is.null(ui_element))
      expect_true(is.object(ui_element))
    })
  })
})

test_that(
  "metricBoxServer should have the correct input parameter",
  {
    testing_options <- list(
      id = "1234",
      title = "hello",
      desc = "This is a test",
      value = 0.8,
      is_perc = TRUE
    )
    app_server_test <- replace_get_golem_options(metricBoxServer, testing_options)
    
    shiny::testServer(app_server_test, {
      session
      stopifnot(
        !is.null(testing_options$title),
        !is.null(testing_options$desc),
        !is.null(testing_options$value)
      )
      expect_type(metricBoxServer, "closure")
      expect_true(!(testing_options$value %in% c(0, "pkg_metric_error", "NA", "", 'FALSE')))
      if(testing_options$is_perc) {
        testing_options$value <- glue::glue('{round(as.numeric(testing_options$value), 1)}%')
        expect_equal(testing_options$value, "0.8%")
      }
    })
  }
)
