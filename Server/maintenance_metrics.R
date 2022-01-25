# IntroJS.
introJSServer(id = "mm_introJS", text = mm_steps)

# Call module that creates section to add comments.
mm_comment_added <- addCommentServer(id = "add_comment_for_mm",
                                     metric_abrv = 'mm',
                                     user_name = reactive(user$name),
                                     user_role = reactive(user$role),
                                     pkg_name = selected_pkg$name)

# Call module that creates comments view.
viewCommentsServer(id = "view_mm_comments",
                   comment_added = mm_comment_added,
                   pkg_name = selected_pkg$name,
                   comment_type = 'mm')

metricGridServer("mm_metricGrid", metrics = metrics)
