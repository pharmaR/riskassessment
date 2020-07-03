#####################################################################################################################
# login_screen.R - Login Screen Source file for Server Module.  
# 
# Author: Aravind
# Created: 02/06/2020.
#####################################################################################################################

# Observe Event to show the warning modal pop up window for invalid details to proceed the APP.

observeEvent(input$submit_details, {
  name<-input$name
  values$name<-trimws(name)
  role<-input$role
  values$role<-trimws(role)
  if(values$name!="" && values$role!=""){
    values$current_screen<-"dashboard_screen"
    shinyjs::show("assessment_criteria_bttn")
    values$mm_tab_redirect<-"redirect"
  } else{
    showModal(
      modalDialog(
        title = HTML("<h3 class = 'txt-danger'>WARNING!</h3>"),
        if(values$name=="" && values$role==""){
          tags$h4("Please enter your Name and Role")
        } else if(values$name==""){
          tags$h4("Please enter your Name")  
        } else if(values$role==""){
          tags$h4("Please enter your Role")
        }
      )
    )
  }
})  # End of the Observe Event.