library(shiny)
library(shinyBS)
library(shinyjs)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)

library(DT)
library(jsonlite)

library(RColorBrewer)
library(lubridate)

library(rclipboard)

ui <- dashboardPage(
  skin = "black",
  dashboardHeader(
    title = tagList(
        span(class = "logo-lg", h4(HTML("Fred Hutch<br>Cromwell Dashboard"))),
        img(src = "fred-hutch.svg")),
    tags$li(
      class = "dropdown",
      style = "padding: 12px;",
      textOutput("userName")
    ),
    tags$li(
      class = "dropdown",
      style = "padding: 8px;",
      uiOutput("loggedInOut")
    ),
    dropdownMenu(
      type = "notifications",
      badgeStatus = NULL,
      icon = icon("circle-question", "fa-solid fa-lg"),
      headerText =
        helpText(
          HTML('<u>Need Help?:</u>
                <p>
                    <br>
                    <b>Email:</b> <a href="mailto:sachamber@fredhutch.org"><span class="badge bg-secondary">sachamber@fredhutch.org</span></a>
                </p>
                <br>
                <b>
                    Bug? Feature request? <a href="https://github.com/FredHutch/shiny-cromwell/issues" , target="_blank"><span class="badge bg-secondary">Open a ticket</span></a>
                </b>
                <br><br>
                <p>
                    <b>Questions</b> about this app, Cromwell or WDL? <a href="https://fhdata.slack.com/archives/CJFP1NYSZ"><span class="badge bg-secondary"><i class="fa-brands fa-slack"></i> FH-Data Slack</span></a> channel for #workflow-managers.
                </p>
                <p></p>
                <p>'))
    ),
    dropdownMenu(
      type = "notifications",
      badgeStatus = NULL,
      icon = icon("github", "fa-solid fa-lg"),
      headerText = helpText(htmlOutput("gitHtml"))
    )
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Welcome",
        tabName = "welcome", icon = icon("book-open"),
        badgeLabel = "info", badgeColor = "black"
      ),
      # menuItem(
      #   tabName = "serverConf",
      #   startExpanded = TRUE,
      #   actionButton(
      #     inputId = "getStarted",
      #     label = "Connect to Server",
      #     icon = icon("plug")
      #   )
      # ),
      menuItem("Cromwell Servers",
        tabName = "cromwell", icon = icon("server"),
        badgeLabel = "cromwell", badgeColor = "yellow"
      ),
      menuItem("Validate",
        tabName = "validate", icon = icon("stethoscope"),
        badgeLabel = "check", badgeColor = "blue"
      ),
      menuItem("Submit Jobs",
        tabName = "submission", icon = icon("paper-plane"),
        badgeLabel = "compute", badgeColor = "green"
      ),
      menuItem("Track Jobs",
        tabName = "tracking", icon = icon("binoculars"),
        badgeLabel = "monitor", badgeColor = "aqua"
      ),
      menuItem("Troubleshoot",
        tabName = "troubleshoot", icon = icon("wrench"),
        badgeLabel = "troubleshoot", badgeColor = "red"
      )
    )
  ),
  dashboardBody(
    shinyjs::useShinyjs(),
    rclipboard::rclipboardSetup(),
    tags$script(src = "js/keyup.js"),
    # tags$style(HTML("
    #   .alert {
    #      width: 75%;
    #      margin: auto;
    #   }
    # ")),
    tabItems(
      tabItem(
        tabName = "welcome",
        fluidRow(
          column(width = 12,
            h2("What is this app?"),
            shiny::includeMarkdown("about.md")
          )
        ),
        # fluidRow(
        #   shinyBS::bsAlert("alert_proof_only")
        # ),
        fluidRow(
          align = "center",
          h2("Dashboard Tabs"),
          box(
            title = "Cromwell Servers", width = 4, solidHeader = TRUE, status = "warning", icon = icon("server"),
            shiny::markdown("This tab allows you to:
            - Start or delete your PROOF based Cromwell server
            - Get metadata for your PROOF based Cromwell server"),
            align = "left"
          ),
          box(
            title = "Validate", width = 4, solidHeader = TRUE, status = "primary", icon = icon("stethoscope"),
            shiny::markdown("This tab allows you to:
            - Validate a workflow you'd like to run"),
            align = "left"
          ),
          box(
            title = "Submit Jobs", width = 4, solidHeader = TRUE, status = "success", icon = icon("paper-plane"),
            shiny::markdown("This tab allows you to:
              - Run a workflow"),
            align = "left"
          )
        ),
        fluidRow(
          align = "center",
          box(
            title = "Track Jobs", width = 6, solidHeader = TRUE, status = "info", icon = icon("binoculars"),
            shiny::markdown("This tab allows you to:
            - Query your Cromwell database for the jobs run the most recent days (your choice how far back to go),
            - See how all of your workflows status',
            - Look within a workflow at the individual calls, failures and call caching results,
            - Download a list of the final workflow outputs for further processing"),
            align = "left"
          ),
          box(
            title = "Troubleshoot", width = 6, solidHeader = TRUE, status = "danger", icon = icon("wrench"),
            shiny::markdown("This tab allows you to:
            - Abort a workflow
            - Troubleshoot the workflow itself by looking at the entire, raw json of workflow metadata (sometimes it's helpful but especially for complex workflows, this is can be large and daunting to parse!)"),
            align = "left"
          )
        )
      ),
      tabItem(
        tabName = "cromwell",
        fluidRow(h2("Manage Your PROOF Based Cromwell Server"), align = "center"),
        fluidRow(
          align = "left",
          box(
            width = 12, solidHeader = FALSE, status = "info",
            collapsed = FALSE,
            title = "Start/delete your Cromwell Server",
            p(strong("Note"), " stopping your server requires making sure you mean it :)"),
            br(),
            br(),
            actionButton(
              inputId = "cromwellStart",
              label = "Start",
              icon = icon("play"),
              class = "btn-primary btn-lg "
            ),
            actionButton(
              inputId = "cromwellDelete",
              label = "Delete",
              icon = icon("stop"),
              class = "btn-warning btn-lg"
            )
          )
        ),
        fluidRow(
          align = "left",
          box(
            width = 12, solidHeader = FALSE, status = "info",
            collapsed = FALSE,
            title = "Status",
            p("Cromwell server details will show below when your server is running"),
            br(),
            br(),
            shinyBS::bsAlert("alert_loggedin"),
            shinyBS::bsAlert("alert_server_status"),
            htmlOutput("proofStatusJobStatus"),
            htmlOutput("proofStatusUrlStr"),
            htmlOutput("proofStatusWorkflowLogDir"),
            htmlOutput("proofStatusScratchDir"),
            htmlOutput("proofStatusSlurmJobId"),
            htmlOutput("proofStatusCromwellDir"),
            htmlOutput("proofStatusServerLogDir"),
            htmlOutput("proofStatusSingularityCacheDir"),
            htmlOutput("proofStatusServerTime"),
            htmlOutput("proofStatusUseAWS"),
            htmlOutput("proofStatusSlurmJobAccount")
          ),
        ),
      ),
      tabItem(
        tabName = "validate",
        fluidRow(h2("Validate a Workflow"), align = "center"),
        fluidRow(
          align = "left",
          ## Validate a Workflow
          box(
            width = 12, solidHeader = FALSE, status = "info",
            collapsible = TRUE, collapsed = FALSE,
            title = "Validate a Workflow",
            p("This tool will check to see if the WDL (and it's input JSON if you choose to upload it)
                                                are both in the correct format required and if not will give you some
                                                hints as to what might be wrong.  If your workflow does not validate, the feedback often hints
                                                at a problem just below where your error actually is. "),
            fileInput(
              inputId = "validatewdlFile", "Upload WDL File (required):",
              accept = ".wdl"
            ),
            fileInput(
              inputId = "validateinputFile", "Upload Consolidated Input JSON (optional):",
              accept = ".json"
            ),
            actionButton(
              inputId = "validateWorkflow",
              label = "Validate Workflow",
              class = "btn-info"
            ),
            br(),
            br(),
            actionButton('resetValidate', 'Reset'),
            verbatimTextOutput(outputId = "validationResult")
          )
        )
      ),
      tabItem(
        tabName = "submission",
        fluidRow(h2("Run Workflows on Cromwell"), align = "center"),
        fluidRow(
          box(
            width = 12, solidHeader = FALSE, status = "success",
            collapsible = TRUE, collapsed = FALSE,
            title = "Submit a Workflow",
            p("Here you can submit your workflow to your Cromwell server for execution.  Only a WDL is required. Up to two different input JSONs
                                                 can be uploaded (if variables are specified in both, the second input's variable value will overwrite the first). Workflow options
                                                 can be provided if desired.  Workflow labels are user-defined values you'd like to use to describe your workflows for your own
                                                 future reference. "),
            column(
              width = 6,
              fileInput(
                inputId = "wdlFile", "Upload WDL (required):",
                accept = ".wdl"
              ),
              fileInput(
                inputId = "inputJSON", "Upload First Input JSON (optional):",
                accept = ".json"
              ),
              fileInput(
                inputId = "input2JSON", "Upload Second Input JSON (optional):",
                accept = ".json"
              ),
              actionButton(
                inputId = "submitWorkflow",
                label = "Submit Workflow",
                icon = icon("paper-plane"),
                class = "btn-info"
              ),
              verbatimTextOutput(outputId = "submissionResult"),
              br(),
              actionButton('resetSubmission', 'Reset')
            ),
            column(
              width = 6,
              fileInput(
                inputId = "workOptions", "Upload Workflow Options JSON (optional):",
                accept = ".json"
              ),
              textInput(
                inputId = "labelValue", "Workflow Label (optional)",
                value = "",
                placeholder = "e.g., First Try"
              ),
              textInput(
                inputId = "seclabelValue", "Secondary Workflow Label (optional)",
                value = "",
                placeholder = "e.g., Cohort 2"
              )
            )
          )
        )
      ),
      tabItem(
        tabName = "tracking",
        fluidRow(h2("Cromwell Workflow Tracking"), align = "center"),
        fluidRow(
          box(
            width = 6,
            numericInput("daysToShow", "Days of History to Display:",
              min = 1, max = 21, value = 1, step = 1
            ),
            actionButton(
              inputId = "trackingUpdate",
              label = "Update View",
              icon = icon("refresh")
            )
          ),
          box(
            width = 6,
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
            footer = "Dates are in Pacific time zone",
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
            collapsible = TRUE, solidHeader = TRUE, collapsed = FALSE,
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
      ),
      tabItem(
        tabName = "troubleshoot",
        fluidRow(
          align = "left",
          box(
            title = "Abort a Workflow",
            p("Aborting a workflow cannot be undone and can take some time to fully stop all jobs submitted in complex or highly parallelized workflows."),
            collapsible = TRUE, collapsed = FALSE,
            width = 12, solidHeader = FALSE, status = "danger",
            textInput(
              inputId = "abortWorkflowID",
              label = "Workflow id to abort:",
              value = "",
              placeholder = "577b9aa4-b26b-4fd6-9f17-7fb33780bbd0",
              width = "30%"
            ),
            actionButton(
              inputId = "abortWorkflow",
              label = "Abort Workflow",
              icon = icon("thumbs-down"),
              class = "btn-info"
            ),
            actionButton('resetAbort', 'Reset'),
            verbatimTextOutput(outputId = "abortResult")
          )
        ),
        fluidRow(
          align = "left",
          ## Troubleshoot a workflow via Glob
          box(
            width = 12, solidHeader = FALSE, status = "info",
            collapsible = TRUE, collapsed = FALSE,
            title = "Troubleshoot a Workflow",
            p("When a workflow fails but no jobs were started, or there appears to be no clear reason for a workflow to have failed, this tool can provide you the entire set of workflow metadata Cromwell has for your workflow in it's raw and unprocessed (json) form. For complex workflows, this can be rather large (and ugly!)."),
            textInput(
              inputId = "troubleWorkflowID",
              label = "Cromwell workflow id to get metadata for:",
              value = "",
              placeholder = "577b9aa4-b26b-4fd6-9f17-7fb33780bbd0",
              width = "30%"
            ),
            actionButton(
              inputId = "troubleWorkflow",
              label = "Get Complete Workflow Metadata",
              icon = icon("question-circle"),
              class = "btn-info"
            ),
            actionButton('resetTrouble', 'Reset'),
            verbatimTextOutput(outputId = "troubleResult")
          )
        )
      )
    )
  )
)
