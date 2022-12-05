

# For more info on testing shiny modules, please refer to:
# https://mastering-shiny.org/scaling-testing.html#modules


test_that("Check static description(s)", {
  mw <- reactiveVal()
  
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

library(rjson)

test_that("Check for table updates with changing weights", {
  mw <- reactiveVal()
  testServer(assessmentInfoServer, args = list(metric_weights = mw), {
    mw(data.frame(name = metrics, weight = weights))
    dt_table <- rjson::fromJSON(output$riskcalc_weights_table) #maintenance_table
    # print(dt_table$x$options$pageLength)
    # print(unlist(dt_table$jsHooks))
    expect_equal(dt_table$x$options$pageLength, 15)
  }) 
})


# session$getReturned() # Use to checked returned values of a module

