library(listviewer)

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
      DTOutput("workflowDescribe")
    ),
    nav_panel(
      title = "Workflow Options",
      DTOutput("workflowOpt")
    ),
    nav_panel(
      title = "Workflow Inputs",
      reactjsonOutput("workflowInp", height = "100%")
      # actionButton("linkToViewerTab", "View list")
    ),
    nav_panel(
      title = "Mermaid",
      uiOutput("mermaid_diagram")
      # actionButton("linkToViewerTab", "View list")
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
      title = "Workflow Outputs",
      p("The specific outputs to the entire workflow itself are listed here only upon request and only if they are all available. "),
      actionButton(
        inputId = "getOutputData",
        label = "Get/Refresh Workflow Output Metadata",
        icon("refresh")
      ),
      downloadButton("downloadOutputs", "Download Workflow Output Data"),
      DTOutput("outputslistBatch")
    )
  )
)


# fluidRow(h3("Workflow Specific Job Information"),
#   fluidRow(
#     box(
#       width = 12,
#       title = "Workflow Description",
#       footer = table_footer(),
#       DTOutput("workflowDescribe")
#     )
#   ),
#   fluidRow(
#     box(
#       width = 6,
#       title = "Workflow Options",
#       actionButton(inputId = "wdlview",
#         label = bsicons::bs_icon("search"),
#         class = "btn-sm"),
#       DTOutput("workflowOpt")
#     ),
#     box(
#       width = 6,
#       title = "Workflow Inputs",
#       actionButton("linkToViewerTab", "View list")
#     )
#   ),
#   fluidRow(
#     align = "center",
#     box(
#       width = 12,
#       title = "Workflow Call Duration",
#       collapsible = TRUE, solidHeader = TRUE,
#       plotOutput("workflowTiming")
#     )
#   ),
#   fluidRow(
#     box(
#       width = 12,
#       title = "Job List",
#       collapsible = TRUE,
#       solidHeader = TRUE,
#       collapsed = FALSE,
#       footer = table_footer(copy = FALSE),
#       downloadButton("downloadJobs", "Download Workflow Jobs Data"),
#       DTOutput("tasklistBatch")
#     )
#   ),
#   fluidRow(
#     box(
#       width = 12,
#       title = "Job Failures",
#       p("Specific information for jobs with a status of 'Failed', only available upon request."),
#       collapsible = TRUE, solidHeader = TRUE, collapsed = FALSE,
#       actionButton(
#         inputId = "getFailedData",
#         label = "Get/Refresh Failed Job Metadata",
#         icon("refresh")
#       ),
#       downloadButton("downloadFails", "Download Call Failure Data"),
#       DTOutput("failurelistBatch")
#     )
#   ),
#   fluidRow(
#     align = "center",
#     infoBoxOutput("cacheHits", width = 6),
#     infoBoxOutput("cacheMisses", width = 6)
#   ),
#   fluidRow(
#     box(
#       width = 12,
#       title = "Call Caching ",
#       p("Only available upon request.  Note: this can be slow for very complex workflows.  "),
#       collapsible = TRUE, solidHeader = TRUE, collapsed = FALSE,
#       actionButton(
#         inputId = "getCacheData",
#         label = "Get/Refresh Call Caching Metadata",
#         icon("refresh")
#       ),
#       downloadButton("downloadCache", "Download Call Caching Data"),
#       DTOutput("cachingListBatch")
#     )
#   ),
#   fluidRow(
#     box(
#       width = 12,
#       title = "Get Workflow Outputs",
#       p("The specific outputs to the entire workflow itself are listed here only upon request and only if they are all available. "),
#       collapsible = TRUE, solidHeader = TRUE, collapsed = FALSE,
#       actionButton(
#         inputId = "getOutputData",
#         label = "Get/Refresh Workflow Output Metadata",
#         icon("refresh")
#       ),
#       downloadButton("downloadOutputs", "Download Workflow Output Data"),
#       DTOutput("outputslistBatch")
#     )
#   )
