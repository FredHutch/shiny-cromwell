### What is this app?
This is a basic shiny app that you can use once you have used the Fred Hutch HPC system to get a Cromwell server running on the `gizmo` cluster.  To learn more about how to get your Cromwell server running, read through the instructions in this [GitHub repo](https://github.com/FredHutch/diy-cromwell-server) or contact Amy Paguirigan for assistance (Fred Hutch email `apaguiri`, or GitHub user `vortexing`).  Once you have a Cromwell server running, then you can use this app on campus (or on VPN) to interact with your server.

#### Cromwell Servers Tab
This tab allows you to:
- Start or delete your PROOF based Cromwell server
- Get metadata for your PROOF based Cromwell server

#### Submit Jobs Tab
This tab allows you to:
- validate a workflow you'd like to run,
- run said workflow.

#### Track Jobs Tab
This tab allows you to:
- query your Cromwell database for the jobs run the most recent days (your choice how far back to go),
- see how all of your workflows status',
- look within a workflow at the individual calls, failures and call caching results,
- download a list of the final workflow outputs for further processing.

#### Troubleshoot Tab
This tab allows you to:
- abort a workflow
- troubleshoot the workflow itself by looking at the entire, raw json of workflow metadata (sometimes it's helpful but especially for complex workflows, this is can be large and daunting to parse!).

If you have issues or wishlist items for this app specifically, please file them as issues in the GitHub repo [for this Cromwell app](https://github.com/FredHutch/shiny-cromwell).  You can also ask questions about the app, Cromwell or WDL in general over in the [FH-Data Slack channel for #workflow-managers](https://fhdata.slack.com/archives/CJFP1NYSZ).
