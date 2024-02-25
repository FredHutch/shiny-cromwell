This is a [Shiny](https://shiny.posit.co/) app being developed by the [Fred Hutch Data Science Lab, DaSL](https://hutchdatascience.org/) that simplifies user interactions with a [Cromwell](https://cromwell.readthedocs.io/en/stable/) server, an open source [WDL](https://openwdl.org/) workflow engine that can be used with a range of HPC backends. We are developing this application alongside Fred Hutch oriented infrastructure with the intention to develop open source resources to enable others to do the same. 

## Fred Hutch Users
At Fred Hutch, we offer a service for supporting the use of our on-premise HPC cluster to run WDL workflows called PROOF.  To use this app at Fred Hutch, choose the "PROOF Login" button at the top of the page and enter your Fred Hutch credentials to get started. 

### Resources
- Find curated WDL workflow repositories and containers in Fred Hutch [DaSL's getWILDS GitHub organization](https://github.com/orgs/getwilds/repositories?q=wdl).
- Find Cromwell and WDL Resources in [Fred Hutch's GitHub organization](https://github.com/FredHutch?utf8=%E2%9C%93&q=wdl+OR+cromwell&type=&language=).
- Learn about Cromwell and WDL on the [Fred Hutch Biomedical Data Science Wiki](https://sciwiki.fredhutch.org/compdemos/Cromwell/).
- Join the community in the [#workflow-managers FH-Data Slack channel](https://fhdata.slack.com/archives/CJFP1NYSZ), (open to all Fred Hutch, UW and Seattle Children's staff using their work emails).
- Get Help from Fred Hutch DaSL staff through [Data House Calls](https://hutchdatascience.org/datahousecalls/).


## Users Elsewhere
This Shiny app and the R packages it relies on are all open source.  Thus, if you are at a different institution and want to use this application to support your work or your users, we encourage you to look through our resources and fork our repositories.  Feel free to contact DaSL developers by filing issues or emailing `wilds@fredhutch.org`. For this use case, we have provided the more general "My Own Cromwell" login button at the top. 

### Resources
  - DIY: Get a Cromwell server running by yourself (does not require logging in). To learn more about how to get your Cromwell server running, read through the instructions in the GitHub repo [FredHutch/diy-cromwell-server](https://github.com/FredHutch/diy-cromwell-server)
- `rcromwell`:  An [R package](https://github.com/getwilds/rcromwell) that makes interacting with the Cromwell API easier. 
- `shiny-cromwell`:  This app's [GitHub repo](https://github.com/FredHutch/shiny-cromwell) contains information on how to set started with this application.  


