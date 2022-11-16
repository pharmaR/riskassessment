#' showHelperMessage
#' 
#' Displays a helper message. By default, it informs the user that he should
#' select a package.
#' 
#' @param message a string
#' 
#' @import shiny
showHelperMessage <- function(message = "Please select a package"){
  h6(message,
     style = 
       "text-align: center;
        color: gray;
        padding-top: 50px;")
}

#' showComments
#' 
#' Displays formatted comments
#' 
#' @param pkg_name a string
#' @param pkg_name a data.frame
#' 
#' @import shiny
#' @export
showComments <- function(pkg_name, comments){
  if (length(pkg_name) == 0)
    return("")
  
  ifelse(
    length(comments$user_name) == 0, 
    "No comments",
    paste0(
      "<div class='well'>",
      icon("user-tie"), " ", "user: ", comments$user_name, ", ", 
      icon("user-shield"), " ", "role: ", comments$user_role, ", ",
      icon("calendar-days"), " ", "date: ", comments$added_on,
      br(), br(), 
      comments$comment,
      "</div>",
      collapse = ""
    )
  )
}
