#####################################################################################################################
# login.R - The login page of the app
# Author: K Aravind Reddy
# Date: July 13th, 2020
# License: MIT License
#####################################################################################################################

# Start of the login screen Source file for UI Module.

output$screen <- renderUI({
 fluidRow(#class = "first_screen",
         column(
           width = 12,
           class = "container",
           box(
             width = 12,
             # class = "main-component-child",
             
             tags$h2("Package Review History", class = "card-title text-center txt-color font-weight-bold"),
             tags$hr(class = "bg-color"),
             id = "db_dash_screen",
             tags$section(
               
               # Start Package DB Dashboard
               fluidRow(
                 class="float-right r_p_format_row",
                 tags$div(
                   class="col-sm W-40 text-left float-right",
                   selectInput("report_formats", "Select Format", c("html", "docx")),  # Select input to select the format for report.
                 ),
                 tags$div(
                   class="col-sm float-right",
                   downloadButton("dwnld_sel_db_pkgs_btn", "Dwnld Report for Selection(s)", class = "download_report_btn_class btn-secondary"),  # Download button to export the report.
                 ),
               ),
               DT::dataTableOutput("db_pkgs"),
               br()
               # End Package DB Dashboard
               
               # tags$div(
               #   class = "col-md-12 mt-4 name-div",
               #   column(width = 12,
               #          tags$h3("User ID", class = "mt-7")),
               #   column(width = 12,
               #          disabled(textInput("name", NULL, value = GetUserName(), )))  # Text input to enter the name.
               # ),
               # tags$div(
               #   class = "col-md-12 role-div",
               #   column(
               #     width = 12,
               #     class = "",
               #     tags$h3("Role", class = "mt-7")
               #   ),
               #   column(width = 12,
               #          textInput("role", NULL))  # Text input to enter the Role.
               # ),
               # tags$div(class = "col-md-12 proceed-div",
               #          column(
               #            width = 12,
               #            align = "center",
               #            actionButton("submit_details", class = "btn btn-secondary p-3 btn-block mb-4 mt-4", "PROCEED TO APP"),  # Action Button to proceed the app.
               #            p(class = "card-text", "*This information will be stored in DB along with comments")
               #          ))
             ) # end tags$section()
            
           )
         )
      )

})


# End of the login screen Source File for UI module.
