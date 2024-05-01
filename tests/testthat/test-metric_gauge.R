
# Helper function used to extra attributes from metric_gauge() shiny.tag.list output
my_attr <- function(x, attr_nm = "label") {
  x |> toString() |> rvest::read_html() |>
    rvest::html_elements(xpath = glue::glue("//{attr_nm}")) |>
    rvest::html_text()
}

meter_val <- function(x) {
  gsub('\"', "", gsub('value=\"', "",
    x |> toString() |> rvest::read_html() |>
    rvest::html_elements("meter") |>
    toString() |> stringr::word(7)
  ))
}

test_that("metric_gauge() 'Label' tag working...", {
  expect_equal(class(metric_gauge("NA")), c("shiny.tag.list", "list"))
  
  expect_equal(metric_gauge("NA") |> my_attr("label") ,"NA")
  expect_equal(metric_gauge("0") |> my_attr("label") |> substr(1,1),"1")
  expect_equal(metric_gauge(".25") |> my_attr("label") ,"0.75")
  expect_equal(metric_gauge(".45") |> my_attr("label") ,"0.55")
  expect_equal(metric_gauge("1") |> my_attr("label") |> substr(1,1),"0")
})
test_that("metric_gauge() 'value' working...", {
  expect_equal(metric_gauge("NA") |> meter_val() ,"0")
  expect_equal(metric_gauge("0") |> meter_val(),"0.08")
  expect_equal(metric_gauge(".06") |> meter_val(),"0.08")
  expect_equal(metric_gauge(".25") |> meter_val() ,"0.25")
  expect_equal(metric_gauge(".45") |> meter_val() ,"0.45")
  expect_equal(metric_gauge("1") |> meter_val(), "1")
})

# For interactive use only, to visualize the meters in a browser
if(interactive()) {
  # library(bslib)
  # library(shiny)
  ui <- fluidPage(
    tags$head(tags$style(
           "meter::-webkit-meter-optimum-value {background: #9CFF94;}
            meter::-webkit-meter-suboptimum-value{background:#FFD070;}
            meter::-webkit-meter-even-less-good-value{background:#FF765B;}
            meter::-moz-meter-bar {background: #FF765B;}  /* color of bar*/
            meter::-moz-meter-optimum-value {background: #9CFF94;}
            meter::-moz-meter-suboptimum-value{background:#FFD070;}")),
    metric_gauge("NA"), br(),
    metric_gauge("0"), br(),
    metric_gauge(".25"), br(),
    metric_gauge(".45"), br(),
    metric_gauge("1")
  )
  server <- function(input, output, session) {}
  shinyApp(ui, server)
}


# div(
#   metric_gauge("NA")#,
#   # tags$script(glue::glue("$('#{\"meter\"}').tooltip({{placement: 'right', title: 'Legend description for \"NA\"', html: false, trigger: 'hover'}});"))
#   # tags$script(glue::glue("$('#{ns(\"meter\")}').tooltip({{placement: 'right', title: 'Legend description', html: false, trigger: 'hover'}});"))
# ),  br(),
# div(
#   metric_gauge("0")#, id = "meter2")#,
#   # tags$script(glue::glue("$('#{\"meter2\"}').tooltip({{placement: 'right', title: 'Legend description for zero', html: false, trigger: 'hover'}});"))
#   ), br(),



