
# this is a broad test to confirm comment functionality is working as expected
# across the applicatio the following modules are in scope for this testing:

# - viewComments[UI/Server]
# - addComment




test_that("Uploaded packages show up in summary table", {
  # delete app DB if exists to ensure clean test
  db_loc <- test_path("test-apps", "database.sqlite")
  if (file.exists(db_loc)) {
    file.remove(db_loc)
  }

  # set up new app driver object
  app <- AppDriver$new(app_dir = test_path("test-apps"))

  # test package data to upload
  test_csv <- system.file("extdata", "upload_format.csv", package = "riskassessment")

  # upload file to application
  app$upload_file(
    `upload_package-uploaded_file` = test_csv
  )

  # wait for table to be shown
  app$wait_for_value(
    output = "upload_package-upload_pkgs_table",
    ignore = list(NULL),
    timeout = 30 * 1000 # CI keeps failing here...
  )
  app$wait_for_idle(1000)

  
  # leave a comment on the dplyr package ---------------------------------------
  
  # select dplyr package
  app$set_inputs(`sidebar-select_pkg` = "dplyr")

  # add a comment for dplyr
  overall_comment <- "This is an overall comment"
  
  app$set_inputs(`sidebar-overall_comment` = overall_comment)
  app$click("sidebar-submit_overall_comment")
  app$wait_for_idle(500)
  
  # dismiss modal showing comment was added 
  app$click(selector = ".modal-dialog .btn")
  app$wait_for_idle(1000)
  
  # click on the report preview tab to see the overall comment
  app$click(selector = '#tabs > li:nth-child(4) > a')
  app$wait_for_idle(500)

  
    
  # parse the comment div on the report page
  comment_well <- app$get_html(selector = "#reportPreview-overall_comments-view_comments > div > div")

  # actual comment shows up after the last <br> tag
  comment_text <- rev(strsplit(comment_well, split = "<br>")[[1]])[1]
  comment_text <- gsub("</div>", "", comment_text)
  
  expect_identical(
    comment_text, 
    overall_comment
  )
  
  # leave a comment on the maintenance metrics tab
  # click on the report preview tab to see the overall comment
  app$click(selector = '#tabs > li:nth-child(2) > a')
  app$wait_for_idle(500)
  
  maintenance_comment <- "This is a maintenance comment"
  app$set_inputs(`maintenanceMetrics-add_comment-add_comment` = maintenance_comment)
  
  app$click("maintenanceMetrics-add_comment-submit_comment")
  app$wait_for_idle(500)
  
  
})


