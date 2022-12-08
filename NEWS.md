# riskassessment (development version)

* Added new pdf report download option to 'Database' and 'Report Preview' tabs (#280) 
* Updated README to include more description, with a focus on improving the 'Usage' section, which now shows our audience how to install & run the app for the first time. Also, new links to the demo app & a short video walk through were added. Plus, very brief notes were included regarding deployment environments.
* Created a new argument for `run_app()` called `app_version` which allows deployment users to write their own custom app_version name via a text string. This is displayed on authentication screen and any downloaded reports. By default, it will display the installed version of `riskassessment`.
* Created a argument for `run_app()` called `login_note` which allows deployment users to add custom log-in notes on the authentication screen. By default, it now displays a note about how to use default usernames and passwords to gain entry for the first time.
* Fixed bug causing Community Usage metrics to not be added on Mac computers
* Fixed bug causing the report downloads to fail if no Community Metrics are available for a package. Instead a message is displayed "Community Usage Metrics not available for {package}"
* Add founders/copyright holders to `DESCRIPTION` file
* Fixed bug where Assessment Criteria tables were not being displayed.
* Risk score is now available in the Report Preview tab and also in the downloaded HTML and DOCX format reports (#264).
* Improved metric 'card' aesthetics in order to remove scroll bars (#198).
* Allow an automatic log in option when running the application in development mode
* Some general re-organizing of the package's infrastructure to make testing and development easier.
* Added console warnings + login note & modals to warn user there is a bug with latest version of {fontawesome} and they should install v0.3.0 if they want to download HTML reports. For more info, the bug status can be tracked [here](https://github.com/rstudio/fontawesome/issues/99).



# riskassessment 0.0.1
* Initiated simple `app.R` for easier deployment using `runURL("https://github.com/pharmaR/risk_assessment/archive/master.zip")` and `shiny::runGitHub('risk_assessment', 'pharmaR')`


# riskassessment 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.
* Used `{golem}` to complete "packagization" of `{riskassessment}`
