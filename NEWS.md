# riskassessment (development version)

* Updated README to include more description, with a focus on improving the 'Usage' section, which now shows our audience how to install & run the app for the first time. Also, new links to the demo app & a short video walk through were added. Plus, very brief notes were included regarding deployment environments.
* created a new argument for `run_app()` called `app_version` which allows deployment users to write their own custom app_version name via a text string. This is displayed on authentication screen and any downloaded reports. By default, it will display the installed version of `riskassessment`.
* created a argument for `run_app()` called `login_note` which allows deployment users to add custom login notes on the authentication screen. By default, it will display a note about how to use default usernames and passwords to login for the first time.
* fixed bug causing Community Usage metrics to not be added on Mac computers
* fixed bug causing the report downloads to fail if no Community Metrics are available for a package. Instead a message is displayed "Community Usage Metrics not available for {package}"
* Add founders/copyright holders to `DESCRIPTION` file
* fixed bug where Assessment Criteria tables were not being displayed


# riskassessment 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.
* Used `{golem}` to complete "packagization" of `{riskassessment}`
