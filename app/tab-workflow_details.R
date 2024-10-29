library(listviewer)

source("tab-troubleshoot.R")
source("utils.R")

panel_job_list <- nav_panel(
  title = "Job List",
  downloadButton("downloadJobs",
    "Download Workflow Jobs Data", style = "width:20%"),
  load_spinner(
    DTOutput("tasklistBatch")
  )
)

panel_workflow_description <- nav_panel(
  title = "Workflow Description",
  card_body(
    load_spinner(
      uiOutput("workflowDescribe")
    )
  )
)

panel_diagram <- nav_panel(
  title = "Diagram",
  load_spinner(
    uiOutput("mermaid_diagram")
  )
)

panel_job_failures <- nav_panel(
  title = "Job Failures",
  p("Specific information for jobs with a status of 'Failed', only available upon request."),
  actionButton(
    inputId = "getFailedData",
    label = "Get/Refresh Failed Job Metadata",
    icon("refresh")
  ),
  downloadButton("downloadFails", "Download Call Failure Data"),
  DTOutput("failurelistBatch")
)

panel_call_caching <- nav_panel(
  title = "Call Caching ",
  p("Only available upon request.  Note: this can be slow for very complex workflows.  "),
  actionButton(
    inputId = "getCacheData",
    label = "Get/Refresh Call Caching Metadata",
    icon("refresh")
  ),
  downloadButton("downloadCache", "Download Call Caching Data"),
  DTOutput("cachingListBatch")
)

panel_options <- nav_panel(
  title = "Workflow Options",
  card(
    class = "border border-primary",
    card_body(
      fillable = TRUE,
      load_spinner(
        uiOutput("workflowOpt")
      )
    )
  )
)

panel_inputs <- nav_panel(
  title = "Workflow Inputs",
  load_spinner(
    reactjsonOutput("workflowInp", height = "100%")
  )
)

panel_outputs <- nav_panel(
  title = "Workflow Outputs",
  p("The specific outputs to the entire workflow itself are listed here only upon request and only if they are all available. "),
  actionButton(
    inputId = "getOutputData",
    label = "Get/Refresh Workflow Output Metadata",
    icon("refresh")
  ),
  downloadButton("downloadOutputs", "Download Workflow Output Data"),
  load_spinner(
    DTOutput("outputslistBatch")
  )
)

tab_workflow_details <- card(
  id = "workflow_details",
  card_header(
    uiOutput("selectedWorkflowUI"),
    class = "d-flex gap-1 justify-content-between"
  ),
  navset_underline(
    panel_job_list,
    panel_workflow_description,
    panel_diagram,
    panel_job_failures,
    panel_call_caching,
    panel_options,
    panel_inputs,
    panel_outputs,
    panel_troublehsoot
  )
)
