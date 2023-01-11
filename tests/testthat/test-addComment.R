
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
  app <- AppDriver$new(app_dir = test_path("test-apps"))

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
    user_name,
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
  
})
