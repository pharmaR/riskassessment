
test_that("Comments can be added via the addComment module", {
  # delete app DB if exists to ensure clean test
  app_db_loc <- test_path("test-apps", "database.sqlite")
  if (file.exists(app_db_loc)) {
    file.remove(app_db_loc)
  }

  # copy in already instantiated database to avoid need to rebuild
  # this is a database that has been built via inst/testdata/upload_format.csv
  test_db_loc <- system.file("testdata", "upload_format.database", package = "riskassessment")
  file.copy(
    test_db_loc,
    app_db_loc
  )

  # confirm no comments exist in the database
  con <- DBI::dbConnect(RSQLite::SQLite(), app_db_loc)
  comments <- DBI::dbGetQuery(con, "select * from comments")
  expect_equal(
    nrow(comments),
    0
  )
  
  # set up new app driver object
  app <- shinytest2::AppDriver$new(app_dir = test_path("test-apps"), load_timeout = 600*1000)

  # select dplyr package
  app$set_inputs(`sidebar-select_pkg` = "dplyr")

  # navigate to maintenance metrics tab
  app$set_inputs(tabs = "Maintenance Metrics")
  app$wait_for_idle(500)

  # confirm no comments are currently shown
  expect_equal(
    app$get_text(selector = "#maintenanceMetrics-view_comments-view_comments > div"),
    "No comments"
  )
  
  # click submit with empty comment box does not add a comment
  app$click("maintenanceMetrics-add_comment-submit_comment")
  app$wait_for_idle(500)
  expect_equal(
    app$get_text(selector = "#maintenanceMetrics-view_comments-view_comments > div"),
    "No comments"
  )

  # enter text in the comment area and submit
  maintenance_comment <- "This is a maintenance comment"
  app$set_inputs(`maintenanceMetrics-add_comment-add_comment` = maintenance_comment)
  app$click("maintenanceMetrics-add_comment-submit_comment")
  app$wait_for_idle(500)

  # parse the comment div on the page
  added_comments <- app$get_html(selector = "#maintenanceMetrics-view_comments-view_comments > div > div")

  # confirm one comment has been added
  expect_equal(
    length(added_comments),
    1
  )

  # confirm comment contents match. actual comment shows up after the last <br> tag
  comment_text <- rev(strsplit(added_comments, split = "<br>")[[1]])[1]
  comment_text <- gsub("</div>", "", comment_text)
  expect_equal(
    comment_text,
    maintenance_comment
  )

  # confirm user name and user role are set appropriately
  user_name <- strsplit(
    strsplit(added_comments, split = "user: ")[[1]][[2]],
    split = ","
  )[[1]][1]
  expect_equal(
    user_name,
    "test_user"
  )

  user_role <- strsplit(
    strsplit(added_comments, split = "role: ")[[1]][[2]],
    split = ","
  )[[1]][1]
  expect_equal(
    user_role,
    "admin"
  )
  
  # confirm comment is in database and has correct metadata
  comments <- DBI::dbGetQuery(con, "select * from comments")
  expect_equal(
    nrow(comments),
    1
  )
  expect_equal(
    comments$comment,
    maintenance_comment
  )
  
  # close connection
  DBI::dbDisconnect(con)
  
  if (file.exists(app_db_loc)) {
    file.remove(app_db_loc)
  }
  
})


test_that("Comment input box is rendered according to the tab and user state", {
  # delete app DB if exists to ensure clean test
  app_db_loc <- test_path("test-apps", "database.sqlite")
  # if (file.exists(app_db_loc)) {
  #   file.remove(app_db_loc)
  # }

  # copy in already instantiated database to avoid need to rebuild
  # this is a database that has been built via inst/testdata/upload_format.csv
  test_db_loc <- system.file("testdata", "upload_format.database", package = "riskassessment")
  file.copy(
    test_db_loc,
    app_db_loc
  )

  # set up new app driver object
  app <- shinytest2::AppDriver$new(app_dir = test_path("test-apps"), load_timeout = 600*1000)
  
  # select dplyr package
  app$set_inputs(`sidebar-select_pkg` = "dplyr")

  # navigate to maintenance metrics tab
  app$set_inputs(tabs = "Maintenance Metrics")
  app$wait_for_idle(500)

  # helper fn
  get_element_attribute <- function(app, element_id, el_attribute) {
    app$get_js(
      sprintf(
        'document.getElementById("%s").getAttribute("%s")',
        element_id,
        el_attribute
      )
    )
  }

  # confirm label & placeholder text are rendered properly
  expect_equal(
    app$get_text("#maintenanceMetrics-add_comment-add_comment-label h5"),
    "Add Comment for Maintenance Metrics"
  )

  expect_equal(
    get_element_attribute(
      app, "maintenanceMetrics-add_comment-add_comment",
      "placeholder"
    ),
    "Commenting as user: test_user, role: admin"
  )


  # change to Community Usage Metrics tab
  app$set_inputs(tabs = "Community Usage Metrics")
  app$wait_for_idle(500)

  # confirm label & placeholder text are rendered properly
  expect_equal(
    app$get_text("#communityMetrics-add_comment-add_comment-label h5"),
    "Add Comment for Community Usage Metrics"
  )

  expect_equal(
    get_element_attribute(
      app, "communityMetrics-add_comment-add_comment",
      "placeholder"
    ),
    "Commenting as user: test_user, role: admin"
  )
})

