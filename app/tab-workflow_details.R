library(listviewer)

source("tab-troubleshoot.R")

tab_workflow_details <- card(
  id = "workflow_details",
  card_header(
    uiOutput("selectedWorkflowUI"),
    class = "d-flex gap-1 justify-content-between"
  ),
  navset_underline(
    nav_panel(
      title = "Job List",
      downloadButton("downloadJobs", "Download Workflow Jobs Data", style = "width:20%"),
      DTOutput("tasklistBatch")
    ),
    nav_panel(
      title = "Workflow Description",
      card_body(
        uiOutput("workflowDescribe")
      )
    ),
    nav_panel(
      title = "Diagram",
      uiOutput("mermaid_diagram")
    ),
    nav_panel(
      title = "Job Failures",
      p("Specific information for jobs with a status of 'Failed', only available upon request."),
      actionButton(
        inputId = "getFailedData",
        label = "Get/Refresh Failed Job Metadata",
        icon("refresh")
      ),
      downloadButton("downloadFails", "Download Call Failure Data"),
      DTOutput("failurelistBatch")
    ),
    nav_panel(
      title = "Call Caching ",
      p("Only available upon request.  Note: this can be slow for very complex workflows.  "),
      actionButton(
        inputId = "getCacheData",
        label = "Get/Refresh Call Caching Metadata",
        icon("refresh")
      ),
      downloadButton("downloadCache", "Download Call Caching Data"),
      DTOutput("cachingListBatch")
    ),
    nav_panel(
      title = "Workflow Options",
      DTOutput("workflowOpt")
    ),
    nav_panel(
      title = "Workflow Inputs",
      reactjsonOutput("workflowInp", height = "100%")
    ),
    nav_panel(
      title = "Workflow Outputs",
      p("The specific outputs to the entire workflow itself are listed here only upon request and only if they are all available. "),
      actionButton(
        inputId = "getOutputData",
        label = "Get/Refresh Workflow Output Metadata",
        icon("refresh")
      ),
      downloadButton("downloadOutputs", "Download Workflow Output Data"),
      DTOutput("outputslistBatch")
    ),
    tab_troublehsoot
  )
)
