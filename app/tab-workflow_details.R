library(listviewer)

source("tab-troubleshoot.R")
source("utils.R")

panel_job_list <- nav_panel(
  title = "Job List",
  id = "detailsJobs",
  card_header(
    downloadButton(
      outputId = "downloadJobs",
      label = "Download Workflow Jobs Data",
      style = "width:20%"
    ),
    actionButton(
      inputId = "refreshJobList",
      label = "Refresh data",
      icon = icon("refresh")
    )
  ),
  card_body(
    load_spinner(
      DTOutput("tasklistBatch")
    )
  ),
  card_footer(table_footer(copy = FALSE))
)

panel_workflow_description <- nav_panel(
  title = "Workflow Description",
  id = "detailsWorkflowDesc",
  card_body(
    load_spinner(
      uiOutput("workflowDescribe")
    )
  )
)

panel_diagram <- nav_panel(
  title = "Diagram",
  id = "detailsDiagram",
  load_spinner(
    uiOutput("mermaid_diagram")
  )
)

panel_job_failures <- nav_panel(
  title = "Job Failures",
  id = "detailsJobFailures",
  p("Specific information for jobs with a status of 'Failed', only available upon request."),
  actionButton(
    inputId = "getFailedData",
    label = "Get/Refresh Failed Job Metadata",
    icon = icon("refresh"),
    width = "300px"
  ),
  downloadButton(
    outputId = "downloadFails",
    label = "Download Call Failure Data",
    style = "width:300px;"
  ),
  uiOutput("failuresAlert"),
  DTOutput("failurelistBatch")
)

panel_call_caching <- nav_panel(
  title = "Call Caching ",
  id = "detailsCallCaching",
  p("Only available upon request.  Note: this can be slow for very complex workflows.  "),
  actionButton(
    inputId = "getCacheData",
    label = "Get/Refresh Call Caching Metadata",
    icon = icon("refresh"),
    width = "300px"
  ),
  downloadButton(
    outputId = "downloadCache",
    label = "Download Call Caching Data",
    style = "width:300px;"
  ),
  uiOutput("cachingAlert"),
  load_spinner(
    DTOutput("cachingListBatch")
  )
)

panel_options <- nav_panel(
  title = "Workflow Options",
  id = "detailsWorkflowOptions",
  br(),
  uiOutput("workflowOptAlert"),
  load_spinner(
    DTOutput("workflowOpt")
  )
)

panel_inputs <- nav_panel(
  title = "Workflow Inputs",
  id = "detailsWorkflowInputs",
  load_spinner(
    reactjsonOutput("workflowInp", height = "100%")
  )
)

panel_outputs <- nav_panel(
  title = "Workflow Outputs",
  id = "detailsWorkflowOutputs",
  p("The specific outputs to the entire workflow itself are listed here only upon request and only if they are all available. "),
  actionButton(
    inputId = "getOutputData",
    label = "Get/Refresh Workflow Output Metadata",
    icon = icon("refresh"),
    width = "350px"
  ),
  downloadButton(
    outputId = "downloadOutputs",
    label = "Download Workflow Output Data",
    style = "width:350px;"
  ),
  uiOutput("outputsAlert"),
  DTOutput("outputslistBatch")
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
