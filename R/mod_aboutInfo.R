#' UI for 'About' Module
#' 
#' @param id a module id name
#' 
#' @importFrom DT dataTableOutput
#' 
#' @keywords internal
#' 
aboutInfoUI <- function(id) {
  fluidPage(
    fluidRow(
      column(
        width = 10, offset = 1,
        h2("About", align = "center", `padding-bottom`="20px"),
        br(),
        # Assessment criteria sub-tab
        tabsetPanel(
          tabPanel(
            title = "Assessment Criteria",
            icon = icon("circle-info"),
            br(),
            fluidRow(
              column(
                width = 10, offset = 1,
            assessmentInfoUI(NS(id,"assessmentInfo"))  # call assessment module UI
               ))
            ),
        # Contacts sub-tab  
          tabPanel(
            title = "Contact",
            icon = icon("envelope"),
            
            # contacts description.
            br(),
            h2("Contact", align = "center", `padding-bottom`="20px"),
            br(),
            fluidRow(
              column(
                width = 10, offset = 1,
            br(),
            shiny::HTML(
               "<h3><i class='fa fa-github' aria-hidden='true'></i> Report Issues </h3>
                <p> To propose new features or report a bug, please review our open issues first on our 
                <a target='_blank' href='https://github.com/pharmaR/riskassessment/issues' > GitHub page</a>. If your topic hasn't been addressed, we highly encourage you to <a target='_blank' href='https://github.com/pharmaR/riskassessment/issues/new' >open a new issue</a>!</p>
                <br>
                <br>
                
                <h3><i class='fa fa-coffee' aria-hidden='true'></i> Development Team </h3> 
                <p> Most all inquiries should flow through the GitHub page reference above (start there), but you may reach out to the lead developer at any time, especially if you would like to start contributing to the project!</p>
                <br>
                <ul>  
                  <li>Aaron Clark [ <a href= 'mailto:clark.aaronchris@gmail.com?subject={riskassessment} inquiry'> Send Email </a> ] </li>
                </ul> 
                <br>                           
                
                <h3><img src='www/R_validation_hub_logo.png' alt='R Validation Hub Logo' style = 'height:50px;padding-right:8px;'>R Validation Hub </h3>
                <p> To learn more about the work and initiatives of R Validation Hub, please visit <a target='_blank' href= 'https://www.pharmar.org/about/'> 
                                            https://www.pharmar.org/about/</a>. </p>  ")
          ))
          ),
        # Contributors sub-tab
          tabPanel(
            title = "Contributors and Companies",
            icon = icon("address-card"),
            
            br(),
            h2("Contributors and Companies", align = "center", `padding-bottom`="20px"),
            br(),
            fluidRow(
              column(
                width = 10, offset = 1,
            br(),
            shiny::HTML(
               "<h3> Official Workstream of the <a target='_blank' href='https://www.pharmar.org/'>R Validation Hub</a>.
                <br/>Sponsored by the <a target='_blank' href='https://www.r-consortium.org//'>R Consortium</a></h3>
                <br>
                <img src='www/pharmaRlogo_large.png' alt='R Validation Hub Logo' style = 'height:130px;padding-right:50px;padding-top:15px;padding-bottom:15px;'>  
                <img src='www/rc_logo.png' alt = 'ProCogia logo' style='height:130px;padding-top:15px;padding-bottom:15px;'> 
                <br>
                
                <br>
                <h3> Collaborative Deployment hosted by <a target='_blank' href='https://procogia.com/'> ProCogia </a> </h3>
                <br>
                <img src='www/procogia_logo.png' alt = 'ProCogia logo' style='height:100px'> 
                <br>
                
                <br>
                <h3> Current Contributors </h3>
                <ul>
                  <li> Aaron Clark, Arcus Biosciences </li>
                  <li> Narayanan Iyer, Pfizer </li>
                  <li> Robert Krajcik, Cytel </li>
                  <li> Barbara Mikulasova, Katalyze Data </li>
                  <li> Jeff Thompson, Arcus Biosciences </li>
                </ul>
                
                <br>
                
                <h3> Past Contributors </h3>
                
                <ul>
                  <li> Eduardo Almeida, Appsilon </li>
                  <li> Lars Andersen, Boehringer Ingelheim </li>
                  <li> Andrew Borgman, Biogen </li>
                  <li> Maya Gans, Cytel </li>
                  <li> Marly Gotti, Biogen </li>
                  <li> Munshi Imran Hossain, Cytel </li>
                  <li> Aravid Reddy Kallem, Fission Labs </li>
                  <li> Scott Schumacker, Canary Medical </li>
                </ul>
                <br>
                
                <h3> Contributing Companies </h3>
                
                <img src='www/appsilon_logo.jpg' alt='Appsilon' style = 'height:100px;padding-right:40px;padding-top:15px;padding-bottom:15px;'> 
                <img src='www/arcus_logo.png' alt='Arcus Biosciences' style = 'height:100px;padding-right:40px;padding-top:15px;padding-bottom:15px;'> 
                <img src='www/bi_logo.png' alt='Boehringer Ingelheim' style = 'height:100px;padding-right:40px;padding-top:15px;padding-bottom:15px;'>
                <img src='www/biogen_logo.jpg' alt='Biogen' style = 'height:100px;padding-right:40px;padding-top:15px;padding-bottom:15px;'>
                <img src='www/cytel.png' alt='Cytel' style = 'height:100px;padding-right:40px;padding-top:15px;padding-bottom:15px;'>
                <img src='www/fission_logo.png' alt='Fission Labs India Pvt Ltd' style = 'height:100px;padding-right:40px;padding-top:15px;padding-bottom:15px;'>
                <img src='www/gcp_logo.png' alt='GCP-Service International' style = 'height:100px;padding-right:40px;padding-top:15px;padding-bottom:15px;'>
                <img src='www/GSK_logo.jpg' alt='GSK' style = 'height:100px;padding-right:40px;padding-top:15px;padding-bottom:15px;'>
                <img src='www/katalyzedata_logo.jpg' alt='Katalyze Data' style = 'height:100px;padding-right:40px;padding-top:15px;padding-bottom:15px;'>
                <img src='www/merck_logo.png' alt='Merck' style = 'height:100px;padding-right:40px;padding-top:15px;padding-bottom:15px;'>
                <img src='www/pfizer_logo.png' alt='Pfizer' style = 'height:100px;padding-right:40px;padding-top:15px;padding-bottom:15px;'>
                <img src='www/roche_logo.png' alt='Roche' style = 'height:100px;padding-top:15px;padding-bottom:15px;'>
                <br>
                <br>
                <br>
                <br>
               ")
          ))
          )
        )
      )))
}

#' Server Logic for 'About' Module
#' 
#' @param id a module id name
#' @param metric_weights object outputted from reweightViewServer()
#' 
#' @keywords internal
#' 
aboutInfoServer <- function(id, metric_weights) {
  moduleServer(id, function(input, output, session) {
    
    # Load server of the assessment criteria module.
    assessmentInfoServer("assessmentInfo", metric_weights = metric_weights) 
    
  })
}
