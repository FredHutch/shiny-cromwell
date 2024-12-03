# shiny-cromwell v1.2.0

* Changing over shiny app framework to bslib (@sckott and @codebeaker)
    * Cleaner and more modern-looking interface, improved usability for researchers ([#162](https://github.com/FredHutch/shiny-cromwell/pull/162), [#169](https://github.com/FredHutch/shiny-cromwell/pull/169), [#174](https://github.com/FredHutch/shiny-cromwell/pull/174), [#193](https://github.com/FredHutch/shiny-cromwell/pull/193))
    * Updated version of bootstrap, easier to use from a programming standpoint ([#129](https://github.com/FredHutch/shiny-cromwell/pull/129))
    * Improved metadata reactions ([#170](https://github.com/FredHutch/shiny-cromwell/pull/170), [#171](https://github.com/FredHutch/shiny-cromwell/pull/171), [#191](https://github.com/FredHutch/shiny-cromwell/pull/191))
* Updating base image of R in Dockerfile (@dtenenba in [#143](https://github.com/FredHutch/shiny-cromwell/pull/143))
* Removing unnecessary code (@sckott in [#149](https://github.com/FredHutch/shiny-cromwell/pull/149))

# shiny-cromwell v1.1.0

* Adding workflow labels to the "Workflows Run" table (@sckott in [#117](https://github.com/FredHutch/shiny-cromwell/pull/117))
* Introducing `make run_branch` command for local Docker build (@dtenenba in [#124](https://github.com/FredHutch/shiny-cromwell/pull/124))
* Switching help dropdown from email to issue filing (@sckott in [#128](https://github.com/FredHutch/shiny-cromwell/pull/128))
* Adding new tab for visualizing input json's (@sckott in [#120](https://github.com/FredHutch/shiny-cromwell/pull/120))
* Adding new tab for creating WDL script mermaid diagrams (@dtenenba in [#125](https://github.com/FredHutch/shiny-cromwell/pull/125))
* Utilizing cookies to persist user login (@sckott in [#130](https://github.com/FredHutch/shiny-cromwell/pull/130))

# shiny-cromwell v1.0.0

* Adding links to PROOF documentation on the welcome tab (@tefirman in #95, #101)
* Adding PROOF server start datetime (@seankross in #93, #101)
* Enabling replicas and stickiness to account for increasing volumes (@dtenenba in #103, #104, #101)
* Abbreviating long entries in display tables (@sckott in #101)
* Beautifying browser title to "PROOF" rather than url (@dtenenba in #101)
* Initializing semantic versioning at v1.0.0 and instituting releases