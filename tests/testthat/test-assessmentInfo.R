

# For more info on testing shiny modules, please refer to:
# https://mastering-shiny.org/scaling-testing.html#modules

test_that("module ui works", {
  ui <- assessmentInfoUI(id = "test")
  golem::expect_shinytaglist(ui)
  # Check that formals have not been removed
  fmls <- formals(assessmentInfoUI)
  for (i in c("id")){
    expect_true(i %in% names(fmls))
  }
})


test_that("Check static description(s)", {
  
  testServer(assessmentInfoServer, args = list(metric_weights = mw), {
    # print(substring(paste(output$riskcalc_desc$html), 1, 33))
    expect_equal(substring(paste(output$riskcalc_desc$html), 1, 33), 
                 "Per the <b>riskmetric</b> package")
    
    # print(paste(substring(output$maintenance_desc$html, 1, 30)))
    expect_equal(substring(paste(output$maintenance_desc$html), 1, 30),
                 "Best practices in software dev")
    
    # print(paste(substring(output$community_usage_desc, 1, 37)))
    expect_equal(substring(paste(output$community_usage_desc), 1, 37),
                 "The user community plays an important")
  }) 
})

# For tests below
metrics <- c('has_vignettes', 
             'has_news', 
             'news_current',
             'has_bug_reports_url',
             'has_website',
             'has_maintainer',
             'has_source_control',
             'export_help',
             'bugs_status',
             'license',
             'downloads_1yr',
             'covr_coverage')

weights <- c(rep(1, length(metrics) - 1), 0)


test_that("Check for table updates with changing weights", {
  mw <- reactiveVal()
  testServer(assessmentInfoServer, args = list(metric_weights = mw), {
    ns <- session$ns
    expect_true(
      inherits(ns, "function")
    )
    expect_true(
      grepl(id, ns(""))
    )
    expect_true(
      grepl("test", ns("test"))
    )
    # Here are some examples of tests you can
    # run on your module
    # - Testing the setting of inputs
    # session$setInputs(x = 1)
    # expect_true(input$x == 1)
    # - If ever your input updates a reactiveValues
    # - Note that this reactiveValues must be passed
    # - to the testServer function via args = list()
    # expect_true(r$x == 1)
    # - Testing output
    # expect_true(inherits(output$tbl$html, "html"))
    
    mw(data.frame(name = metrics, weight = weights))
    session$flushReact()
    dt_table <- rjson::fromJSON(output$riskcalc_weights_table) 
    # print(dt_table)
    # print(dt_table$jsHooks)
    expect_equal(dt_table$x$options$pageLength, 15)
  }) 
})




# session$getReturned() # Use to checked returned values of a module

