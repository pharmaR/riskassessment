# riskassessment (development version)

* Updated README to include more description, with a focus on improving the 'Usage' section, which now shows our audience how to install & run the app for the first time. Also, new links to the demo app & a short video walk through were added. Plus, very brief notes were included regarding deployment environments.
* created a new argument for `run_app()` called `app_version` which allows deployment users to write their own custom app_version name via a text string. This is displayed on authentication screen and any downloaded reports. By default, it will display the installed verion of `riskassessment`.
* created a argument for `run_app()` called `login_note` which allows deployment users to add custom login notes on the authentication screen. By default, it will display a note about how to use default usernames and passwords to login for the first time.


# riskassessment 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.
* Used `{golem}` to complete "packagization" of `{riskassessment}`
