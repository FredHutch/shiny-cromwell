## remotes::install_github('getwilds/rcromwell')
library(shiny); library(shinydashboard);
library(data.table); library(DT); library(tidyverse); 
library(RColorBrewer) 
library(rcromwell)
# for rendering the About page:
library(markdown)
library(shinyWidgets)
library(jsonlite)
library(lubridate)

ui <- dashboardPage( skin = "black",
                     dashboardHeader(title = "Fred Hutch Cromwell Dashboard"),
                     dashboardSidebar(
                       sidebarMenu(
                         menuItem("Welcome", tabName = "welcome", icon = icon("book-open"), 
                                  badgeLabel = "info", badgeColor = "green"),
                         menuItem(tabName = "serverConf", 
                                  startExpanded = TRUE,
                                  actionButton(inputId = "getStarted",
                                               label = "Connect to Server",
                                               icon = icon("plug"))
                         ),
                         menuItem("Submit Jobs", tabName = "submission", icon = icon("paper-plane"),
                                  badgeLabel = "compute", badgeColor = "light-blue"),
                         menuItem("Track Jobs", tabName = "tracking", icon = icon("binoculars"),
                                  badgeLabel = "monitor", badgeColor = "purple"),
                         menuItem("Troubleshoot", tabName = "troubleshoot", icon = icon("wrench"),
                                  badgeLabel = "troubleshoot", badgeColor = "teal")
                       )
                     ),
                     dashboardBody(
                       tabItems(
                         tabItem(tabName = "welcome",
                                 fluidRow(align = "center",
                                          box(title = "Find Cromwell and WDL Resources at Fred Hutch's GitHub", 
                                              actionButton(inputId='githubLink', label="What resources are available?",
                                                           icon = icon("retweet"),
                                                           onclick ="window.open('https://github.com/FredHutch?utf8=%E2%9C%93&q=wdl+OR+cromwell&type=&language=', '_blank')")
                                          ),
                                          box(title = "Learn about Cromwell and WDL at SciWIki",
                                              actionButton(inputId='sciwikiLink', label="I want to go read!",
                                                           icon = icon("book-open"),
                                                           onclick ="window.open('https://sciwiki.fredhutch.org/compdemos/Cromwell/', '_blank')")
                                          )
                                 ),
                                 fluidRow(align = "center",
                                          column(width = 11, align = "left",
                                                 includeMarkdown("about.md"))
                                 )
                         ),
                         tabItem(tabName = "submission",
                                 fluidRow(h2("Run Workflows on Cromwell"), align = "center"),
                                 fluidRow(align = "left", 
                                          ## Validate a Workflow
                                          box(width = 12,  solidHeader = FALSE, status = "info",
                                              collapsible = TRUE, collapsed = FALSE,
                                              title = "Validate a Workflow",
                                              p("This tool will check to see if the WDL (and it's input JSON if you choose to upload it)
                                                are both in the correct format required and if not will give you some
                                                hints as to what might be wrong.  If your workflow does not validate, the feedback often hints 
                                                at a problem just below where your error actually is. "),
                                              fileInput(inputId = "validatewdlFile", "Upload WDL File (required):",
                                                        accept = ".wdl"),
                                              fileInput(inputId = "validateinputFile", "Upload Consolidated Input JSON (optional):",
                                                        accept = ".json"),
                                              actionButton(inputId = "validateWorkflow",
                                                           label = "Validate Workflow",
                                                           icon = icon("question-circle")),
                                              verbatimTextOutput(outputId = "validationResult")
                                          )),
                                 fluidRow(
                                           box(width = 12, solidHeader = FALSE, status = "success",
                                               collapsible = TRUE, collapsed = FALSE,
                                               title = "Submit a Workflow",
                                               p("Here you can submit your workflow to your Cromwell server for execution.  Only a WDL is required. Up to two different input JSONs
                                                 can be uploaded (if variables are specified in both, the second input's variable value will overwrite the first). Workflow options
                                                 can be provided if desired.  Workflow labels are user-defined values you'd like to use to describe your workflows for your own
                                                 future reference. "),
                                               column( width = 6,
                                                fileInput(inputId = "wdlFile", "Upload WDL (required):",
                                                         accept = ".wdl"),
                                               fileInput(inputId = "inputJSON", "Upload First Input JSON (optional):",
                                                         accept = ".json"),
                                               fileInput(inputId = "input2JSON","Upload Second Input JSON (optional):",
                                                         accept = ".json")
                                               ),
                                                column( width = 6,
                                               fileInput(inputId = "workOptions","Upload Workflow Options JSON (optional):",
                                                         accept = ".json"),
                                               textInput(inputId = "labelValue", "Workflow Label (optional)",
                                                         value = "",
                                                         placeholder = "e.g., First Try"),
                                               textInput(inputId = "seclabelValue", "Secondary Workflow Label (optional)",
                                                         value = "",
                                                         placeholder = "e.g., Cohort 2"),
                                               actionButton(inputId = "submitWorkflow",
                                                            label = "SubmitWorkflow",
                                                            icon = icon("paper-plane")),
                                               verbatimTextOutput(outputId = "submissionResult")
                                               )))
                                 ),
                         tabItem(tabName = "tracking", 
                                 fluidRow(h2("Cromwell Workflow Tracking"), align = "center"),
                                 fluidRow(
                                     box( width = 6,
                                          numericInput("daysToShow", "Days of History to Display:",
                                                       min = 1, max = 21, value = 1, step = 1),
                                          actionButton(inputId = "trackingUpdate",
                                                       label = "Update View",
                                                       icon = icon("refresh"))),

                                     box(width = 6,
                                          textInput("workName", "Filter for workflows with name:",
                                                    value = "",
                                                    placeholder = "myCustomWorkflow"),
                                         selectInput('workStatus', label = "Filter for Workflows with Status(es):",
                                                     choices = c("Submitted", "Running",
                                                                 "Succeeded", "Failed", "Aborting", 
                                                                 "Aborted"),  multiple = TRUE)
                                         )),
                                 fluidRow(
                                   box(width = 12,
                                       infoBoxOutput("submittedBox", width = 6),
                                       infoBoxOutput("inprogressBox", width = 6),
                                       infoBoxOutput("successBox", width = 6),
                                       infoBoxOutput("failBox", width = 6)
                                   )),
                                   fluidRow(

                                     box(width = 12, 
                                         title = "Workflow Timing",
                                         collapsible = TRUE, solidHeader = TRUE,
                                         plotOutput("workflowDuration")
                                     )
                                   ),
                                   fluidRow(
                                     box(width = 12,
                                         title = "Workflows Run",
                                         collapsible = TRUE, solidHeader = TRUE,
                                         DTOutput("joblistCromwell")
                                     )
                                   ),
                                   fluidRow(h3("Workflow Specific Job Information"), align = "center",
                                            p("Select a row in the above table for a specific workflow id in order to populate the tables below.  "),
                                   valueBoxOutput("pendingBatch", width = 3),
                                   infoBoxOutput("runningBatch", width = 3),
                                   infoBoxOutput("succeededBatch", width = 3),
                                   infoBoxOutput("failedBatch", width = 3)
                                 ),
                                 fluidRow(
                                     box(width = 12,
                                         title = "Workflow Description",
                                         DTOutput("workflowDescribe"))
                                 ),
                                 fluidRow(
                                     box(width = 6,
                                         title = "Workflow Options",
                                         DTOutput("workflowOpt")),
                                     box(width = 6,
                                         title = "Workflow Inputs",
                                         DTOutput("workflowInp"))
                                 ),
                                 fluidRow( align = "center",
                                 box(width = 12,
                                     title = "Workflow Call Duration",
                                     collapsible = TRUE, solidHeader = TRUE,
                                     plotOutput("workflowTiming")
                                 )),
                                 fluidRow(
                                   box(width = 12,
                                       title = "Job List",
                                       collapsible = TRUE,solidHeader = TRUE,collapsed = FALSE,
                                       downloadButton("downloadJobs", "Download Workflow Jobs Data"),
                                       DTOutput("tasklistBatch"))
                                 ),
                                 fluidRow(
                                   box(width = 12,
                                       title = "Job Failures",
                                       p("Specific information for jobs with a status of 'Failed', only available upon request."),
                                       collapsible = TRUE,solidHeader = TRUE,collapsed = FALSE,
                                       actionButton(inputId = "getFailedData",
                                                    label = "Get/Refresh Failed Job Metadata",
                                                    icon("refresh")),
                                       downloadButton("downloadFails", "Download Call Failure Data"),
                                       DTOutput("failurelistBatch"))
                                 ),
                                 fluidRow(align = "center",
                                              infoBoxOutput("cacheHits", width = 6),
                                              infoBoxOutput("cacheMisses", width = 6)
                                          ),
                                 fluidRow(
                                   box(width = 12,
                                       title = "Call Caching ", 
                                       p("Only available upon request.  Note: this can be slow for very complex workflows.  "),
                                       collapsible = TRUE,solidHeader = TRUE,collapsed = FALSE,
                                       actionButton(inputId = "getCacheData",
                                                    label = "Get/Refresh Call Caching Metadata",
                                                    icon("refresh")),
                                       downloadButton("downloadCache", "Download Call Caching Data"),
                                       DTOutput("cachingListBatch"))
                                 ),
                                 fluidRow(
                                   box(width = 12,
                                       title = "Get Workflow Outputs",
                                       p("The specific outputs to the entire workflow itself are listed here only upon request and only if they are all available. "),
                                       collapsible = TRUE, solidHeader = TRUE, collapsed = FALSE,
                                       actionButton(inputId = "getOutputData",
                                                    label = "Get/Refresh Workflow Output Metadata",
                                                    icon("refresh")),
                                       downloadButton("downloadOutputs", "Download Workflow Output Data"),
                                       DTOutput("outputslistBatch"))
                                 )
                         ),
                         tabItem(tabName = "troubleshoot",
                                 fluidRow(align = "left", 
                                     box(title = "Abort a Workflow",
                                         p("Aborting a workflow cannot be undone and can take some time to fully stop all jobs submitted in complex or highly parallelized workflows."),
                                         collapsible = TRUE, collapsed = FALSE,
                                         width = 12,  solidHeader = FALSE, status = "danger",
                                         textInput("abortWorkflowID", "Workflow id to abort:",
                                                   value = "",
                                                   placeholder = "577b9aa4-b26b-4fd6-9f17-7fb33780bbd0"),
                                         actionButton(inputId = "abortWorkflow",
                                                      label = "Abort Workflow",
                                                      icon = icon("thumbs-down")),
                                         verbatimTextOutput(outputId = "abortResult")
                                     )
                                 ),
                                 
                                 fluidRow(align = "left", 
                                          ## Troubleshoot a workflow via Glob
                                          box(width = 12,  solidHeader = FALSE, status = "info",
                                              collapsible = TRUE, collapsed = FALSE,
                                              title = "Troubleshoot a Workflow",
                                              p("When a workflow fails but no jobs were started, or there appears to be no clear reason for a workflow to have failed, this tool can provide you the entire set of workflow metadata Cromwell has for your workflow in it's raw and unprocessed (json) form. For complex workflows, this can be rather large (and ugly!)."),
                                              textInput("troubleWorkflowID", "Cromwell workflow id to get metadata for:",
                                                        value = "",
                                                        placeholder = "577b9aa4-b26b-4fd6-9f17-7fb33780bbd0"),
                                              actionButton(inputId = "troubleWorkflow",
                                                           label = "Get Complete Workflow Metadata",
                                                           icon = icon("question-circle")),
                                              verbatimTextOutput(outputId = "troubleResult")
                                          ))
                         )
                       )
                     )
)
