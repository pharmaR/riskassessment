# riskassessment (development version)
* Fix for Chromote error provided by Winston Chang
* Implemented usage of database configuration file instead of passing arguments in `run_app()` (#459)
* Added decision categories as an option in the database configuration file (#459)
* Added decision automation rules as an option in the database configuration file (#459)
* Added Decision-related columns to the database view
* Fix bug preventing icons from displaying on admin tab (#427)
* Added Package Dependencies tab, and two new cards to Maintenance Metrics (#261)

# riskassessment 0.1.1

### Enhancements
* Upgraded to leverage {riskmetric} to v0.2.0 which impacts risk score calculations
* Add button that redirects to pkg metrics
* Added 'search' capability to database view tab
* Added Excel & CSV export options on Database View tab
* Added hyperlink to the documentation site / user guide within the application
* Added check that URLs needed to upload packages are reachable, just in case!
* Added `riskassessment` logo for docx and pdf reports
* Some general aesthetic improvements, for example: made sure tab headers were a consistent size

### Squashed Bugs
* Fixed bug where card displaying "Number of downloads in last 12 months" was faulty, summing all months from previous year and none from current year.
* Report headings were changed to "asis" sentence case to avoid altering case on package names in reports
* Fix failing tests: prefix AppDriver$new with 'shinytest2::'
* Fix for Chromote error provided by Winston Chang

### Docs
* Added User Feedback survey to `README` (#411)
* Fixed broken R-CMD-CHECK badge in `README`
* Updated a few vignettes to describe new 'Decision Automation' feature

### For Devs
* Removed {golem} testing option `pre_auth_user` from the arguments of `run_app()`
* Improved stability and ease-of-use for testing `introJS` module
* Preload information for a select list of packages to improve tests utilizing `shinytest2`
* Upgraded to R Version v4.2.2


# riskassessment 0.1.0

### All New Features
* Allowed users to type in package names to be assessed in the app, instead of uploading CSV file. Also allow point-and-click deletion of packages.
* Added decision automation capabilities where the user can set decision rules for uploaded packages to be auto-assigned.

### Enhancements
* Added "PDF report" download option (#280)
* Added risk score to the Report Preview tab and downloaded reports (#264).
* Exported `generate_comm_data()` to help users build the community usage data needed run `build_comm_plotly()` (also exported) to produce the "downloads by month"" plot as seen on the Community Usage tab of the application. (#266).

### For deployment
* Created a argument for `run_app()` called `login_note` which allows users deploying the app to add custom log-in notes on the authentication screen. By default, it displays a note about how to use default usernames and passwords to gain entry for the first time.
* Engineered a new argument to `run_app()` called `app_version` which allows users deploying the application to publish their own custom  text string in the app's authentication screen and any downloaded reports. By default, it will display the installed version.

### Docs
* Updated `README` to include more description, 'Usage' info, including how to install & run the app for the first time. Last, included links to the demo app & a short video walk-through. Very brief notes were included regarding deployment environments.
* Added several vignettes to documentation site, including:
  * "Get started with `riskassessment`" vignette
  * "Administrative Tools and Options" vignette
* Designed hex logo
* updated documentation to reflect new repo name 'riskassessment' without the '_' to separate the two words.
* suggest installation of `riskmetric` from GitHub, and not CRAN (for now)

### Squashed Bugs
* Fixed bug causing Community Usage metrics to fail on Mac computers
* Fixed bug causing the report to fail when no Community Metrics were available for a package.
* Add founders/copyright holders to `DESCRIPTION` file
* Fixed bug where `Assessment Criteria` tables were failing to render.
* Improved metric 'card' aesthetics in order to remove scroll bars (#198).
* Added console warnings, log-in note, and modals to warn user there is a bug with {fontawesome} v0.4.0. For more info, the bug status can be tracked [here](https://github.com/rstudio/fontawesome/issues/99).
* Fixed summary of community usage data when there was more than one package version released in the same month
* Fixed bug where the logging file was not being set
* Fixed package delete button shadow; fix introjs for admin/non-admin roles
* Fixed display of 'Report Bugs' metric to align with current `riskmetric` presentation as 0 or 1

### For Devs
* Some general re-organizing of the package's infrastructure to make testing and development easier.
* Added unit tests to attain test coverage >85% (#295).
* Added `renv` and a `renv.lock` file as well as corresponding vignette for developers/contributors to align on.
* Reduce number of package dependencies from 33 to 26
* Changed risk-based color gradient to use colorblind-friendly color palette (#324).
* Adopt (temporary) CRAN-first data collection method for pkg info via `riskmetric::pkg_ref()`


# riskassessment 0.0.1

* Initiated simple `app.R` for easier deployment using `runURL("https://github.com/pharmaR/riskassessment/archive/master.zip")` and `shiny::runGitHub('riskassessment', 'pharmaR')`


# riskassessment 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.
* Used `{golem}` to complete "packagization" of `{riskassessment}`
