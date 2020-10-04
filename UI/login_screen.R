#####################################################################################################################
# login.R - The login page of the app
# Author: K Aravind Reddy
# Date: July 13th, 2020
# License: MIT License
#####################################################################################################################

# Start of the login screen Source file for UI Module.

output$screen <- renderUI({
 fluidRow(class = "first_screen",
         column(
           width = 12,
           class = "container",
           box(
             width = 4,
             class = "card w-40 p-4 box_initial_screen",
             tags$h2("Please fill in your details", class = "card-title text-center txt-color font-weight-bold"),
             tags$hr(class = "bg-color"),
             id = "initial_screen",
             tags$section(
               tags$div(
                 class = "col-md-12 mt-4 name-div",
                 column(width = 12,
                        tags$h3("User ID", class = "mt-7")),
                 column(width = 12,
                        textInput("name", NULL))  # Text input to enter the name.
               ),
               tags$div(
                 class = "col-md-12 role-div",
                 column(
                   width = 12,
                   class = "",
                   tags$h3("Role", class = "mt-7")
                 ),
                 column(width = 12,
                        disabled(textInput("role", NULL, value = "RinPharma-Participant")))  # Text input to enter the Role.
               ),
               tags$div(class = "col-md-12 proceed-div",
                        column(
                          width = 12,
                          align = "center",
                          actionButton("submit_details", class = "btn btn-secondary p-3 btn-block mb-4 mt-4", "PROCEED TO APP"),  # Action Button to proceed the app.
                          p(class = "card-text", "*This information will be stored in DB along with comments")
                        ))
             ),
            
           )
         )
      )

})


# End of the login screen Source File for UI module.
