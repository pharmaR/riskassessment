
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

test_that("metric_gauge() working...", {
  expect_equal(class(metric_gauge("NA")), "shiny.tag.list")
  
  expect_equal(metric_gauge("NA") |> my_attr("label") ,"NA")
  expect_equal(metric_gauge(".25") |> my_attr("label") ,"0.75")
  expect_equal(metric_gauge(".45") |> my_attr("label") ,"0.55")
  expect_equal(metric_gauge("1") |> my_attr("label") |> substr(1,1),"0")
 
  expect_equal(metric_gauge("NA") |> meter_val() ,"0")
  expect_equal(metric_gauge(".25") |> meter_val() ,"0.25")
  expect_equal(metric_gauge(".45") |> meter_val() ,"0.45")
  expect_equal(metric_gauge("1") |> meter_val(), "1")
})
