## R Package Risk Assessment Project

Author: Fission Labs India Pvt Ltd<br>
Date: June 24, 2020

The R Package Risk Assessment app is an interactive web application providing a front end for the collection of metrics for R packages via RiskMetrics package including visualizations and comparison metrics.

### Shiny App Installation
The App is portable without code modifications and can be pulled from GitHub and run in the local environment by the users. User needs to copy the following files/folders to run the application:<br>

1. Data
2. Modules
3. Reports
4. Server
5. UI
6. Utils
7. conf
8. www
9. app.R
10. loggit.json
11. risk_assessment_app.db

### After Installation
-Open app.R file with RStudio<br>
-Run the app from RStudio


Risk assessment application offers a few key features:<br>
     --An interactive UI platform that allows user to upload list of packages<br>
     --Sidebar panel to select package and version number<br>
     --The main panel that displays the details and metrics of the selected packages<br> 
     --Comment functionality to leave a comment for individual metrics as well as (overall comment) on the package<br>
     --Download report functionality to download the package details in html or word doc format

### R Design
-RStudio as a development environment<br>
-Web Scraping of ‘packages’ details from cran<br>
-RiskMetrics package to get the metrics

### Credits
Lead Developer - Aravind Reddy Kallem

### License
The MIT License (MIT)<br>
Copyright © 2020 Fission Labs
 
Note: Permission notice and Copyright notice to be added as per client requirements.

