source("ui_components.R")

tab_tracking <- tabItem(
  tabName = "tracking",
  #fluidRow(h2("Cromwell Workflow Tracking"), align = "center"),

  fluidRow(
    box(
        title = "Track your Workflows",
        width = 12, solidHeader = FALSE, status = "info",
        collapsible = FALSE, collapsed = FALSE,
        p("Once you've submitted workflows, you can track the status of all the workflows you've submitted
    in the specified time range by clicking `Update View`.  If you use Cromwell a lot, 
    this and the filtering tools below can help you return only the workflows you're interested in monitoring,
    making tracking and the application itself much faster. "),
     
      numericInput("daysToShow", "Days of History to Display:",
        min = 1, max = 21, value = 1, step = 1),
     
      textInput("workName", "Filter for workflows with name:",
        value = "",
        placeholder = "myCustomWorkflow"
      ),
      selectInput("workStatus",
        label = "Filter for Workflows with Status(es):",
        choices = c(
          "Submitted", "Running",
          "Succeeded", "Failed", "Aborting",
          "Aborted"
        ), multiple = TRUE
      ),
      actionButton(
          inputId = "trackingUpdate",
          label = "Update View",
          icon = icon("refresh")
      )
    )
  ),
  fluidRow(
    box(
      width = 12,
      infoBoxOutput("submittedBox", width = 6),
      infoBoxOutput("inprogressBox", width = 6),
      infoBoxOutput("successBox", width = 6),
      infoBoxOutput("failBox", width = 6)
    )
  ),
  fluidRow(
    box(
      width = 12,
      title = "Workflow Timing",
      collapsible = TRUE, solidHeader = TRUE,
      plotOutput("workflowDuration")
    )
  ),
  fluidRow(
    box(
      width = 12,
      title = "Workflows Run",
      collapsible = TRUE,
      solidHeader = TRUE,
      footer = table_footer(),
      DTOutput("joblistCromwell")
    )
  ),
  fluidRow(h3("Workflow Specific Job Information"),
    align = "center",
    p("Select a row in the above table for a specific workflow id in order to populate the tables below.  "),
    valueBoxOutput("pendingBatch", width = 3),
    infoBoxOutput("runningBatch", width = 3),
    infoBoxOutput("succeededBatch", width = 3),
    infoBoxOutput("failedBatch", width = 3)
  ),
  fluidRow(
    box(
      width = 12,
      title = "Workflow Description",
      footer = table_footer(),
      DTOutput("workflowDescribe")
    )
  ),
  fluidRow(
    box(
      width = 6,
      title = "Workflow Options",
      DTOutput("workflowOpt")
    ),
    box(
      width = 6,
      title = "Workflow Inputs",
      DTOutput("workflowInp")
    )
  ),
  fluidRow(
    align = "center",
    box(
      width = 12,
      title = "Workflow Call Duration",
      collapsible = TRUE, solidHeader = TRUE,
      plotOutput("workflowTiming")
    )
  ),
  fluidRow(
    box(
      width = 12,
      title = "Job List",
      collapsible = TRUE,
      solidHeader = TRUE,
      collapsed = FALSE,
      footer = table_footer(copy = FALSE),
      downloadButton("downloadJobs", "Download Workflow Jobs Data"),
      DTOutput("tasklistBatch")
    )
  ),
  fluidRow(
    box(
      width = 12,
      title = "Job Failures",
      p("Specific information for jobs with a status of 'Failed', only available upon request."),
      collapsible = TRUE, solidHeader = TRUE, collapsed = FALSE,
      actionButton(
        inputId = "getFailedData",
        label = "Get/Refresh Failed Job Metadata",
        icon("refresh")
      ),
      downloadButton("downloadFails", "Download Call Failure Data"),
      DTOutput("failurelistBatch")
    )
  ),
  fluidRow(
    align = "center",
    infoBoxOutput("cacheHits", width = 6),
    infoBoxOutput("cacheMisses", width = 6)
  ),
  fluidRow(
    box(
      width = 12,
      title = "Call Caching ",
      p("Only available upon request.  Note: this can be slow for very complex workflows.  "),
      collapsible = TRUE, solidHeader = TRUE, collapsed = FALSE,
      actionButton(
        inputId = "getCacheData",
        label = "Get/Refresh Call Caching Metadata",
        icon("refresh")
      ),
      downloadButton("downloadCache", "Download Call Caching Data"),
      DTOutput("cachingListBatch")
    )
  ),
  fluidRow(
    box(
      width = 12,
      title = "Get Workflow Outputs",
      p("The specific outputs to the entire workflow itself are listed here only upon request and only if they are all available. "),
      collapsible = TRUE, solidHeader = TRUE, collapsed = FALSE,
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
