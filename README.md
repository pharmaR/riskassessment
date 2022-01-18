## Risk Assessment Shiny Application

The Risk Assessment App is an interactive web application serving as a front end
application for the [`riskmetric`](https://github.com/pharmaR/riskmetric) R package. `riskmetric` is a framework to quantify risk by assessing a number of
metrics meant to evaluate development best practices, code documentation,
community engagement, and development sustainability. The app and `riskmetric`
aim to provide some context for validation within regulated industries.

Furthermore, the app extends the functionalities of `riskmetric` by allowing
the reviewer to

- analyze `riskmetric` output without the need to code in R,
- comment on the value of individual metrics,
- provide an overall assessment on the package (i.e., low, medium, or high risk)
based on the output of the evaluating metrics cohort,
- download a report with the package risk, metrics, and reviewer comments, and
- store assessments on the database for future viewing.


### Installation
The application is portable without code modifications. It can be pulled from GitHub and run in any local environment.


### Deployment
To run the application, open the `app.R` file with RStudio and click 'Run App'.


### Key features

- An interactive UI platform that allows users to upload a list of packages to assess

- Sidebar panel to select a package and version number from the list of uploaded packages

- Set of panels that displays the details and metrics of the selected package

- Comment functionality that allows users to comment on individual metrics as well as provide an overall comment on the selected package

- Download report functionality to obtain either an html or a word document with the metrics displayed in the application


### License
The MIT License (MIT)<br>
Copyright © 2020 Fission Labs and R Validation Hub contributors
 
Note: Permission and Copyright notices to be added as per client requirements.

