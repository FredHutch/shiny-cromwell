## remotes::install_github('FredHutch/fh.wdlR@v0.1.1')
library(shiny); library(shinydashboard);
library(data.table); library(DT); library(tidyverse); 
library(RColorBrewer) 
library(fh.wdlR)
# for rendering the About page:
library(markdown)

ui <- dashboardPage( skin = "black",
                     dashboardHeader(title = "Fred Hutch Cromwell Dashboard",
                                     titleWidth = 450),
                     dashboardSidebar(
                       sidebarMenu(
                         menuItem("Welcome", tabName = "welcome", icon = icon("book-open"), 
                                  badgeLabel = "info", badgeColor = "green"),
                         menuItem("Submit Jobs", tabName = "submission", icon = icon("paper-plane"),
                                  badgeLabel = "compute", badgeColor = "light-blue"),
                         menuItem("Track Jobs", tabName = "tracking", icon = icon("binoculars"),
                                  badgeLabel = "monitor", badgeColor = "purple")
                       )
                     ),
                     dashboardBody(
                       tabItems(
                         tabItem(tabName = "welcome",
                                 fluidRow( align = "center",
                                           ## Provide a valid cromwell server node and port
                                           box(title = "Provide (or update) a Running Cromwell Server Node and Port ",
                                               width = 12, solidHeader = FALSE, status = "primary",
                                               textInput("cromwellURL", label = "(e.g.: `gizmoz45:8000`)",
                                                         value = ""),
                                               actionButton(inputId = "setCromwellURL",
                                                            label = "Submit",
                                                            icon = icon("refresh")),
                                               verbatimTextOutput(outputId = "connectionResult"))
                                 ),
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
                                 fluidRow(
                                           box(width = 8, solidHeader = FALSE, status = "success",
                                               collapsible = TRUE, collapsed = FALSE,
                                               title = "Submit a Workflow",
                                               fileInput(inputId = "wdlFile", "Upload WDL File:",
                                                         accept = ".wdl"),
                                               fileInput(inputId = "inputJSON", "Upload First Input JSON:",
                                                         accept = ".json"),
                                               fileInput(inputId = "input2JSON","Upload Second Input JSON:",
                                                         accept = ".json"),
                                               fileInput(inputId = "workOptions","Upload Workflow Options JSON:",
                                                         accept = ".json"),
                                               actionButton(inputId = "submitWorkflow",
                                                            label = "SubmitWorkflow",
                                                            icon = icon("paper-plane")),
                                               verbatimTextOutput(outputId = "submissionResult")),
                                               box(title = "Abort a Workflow",
                                                   collapsible = TRUE, collapsed = FALSE,
                                                   width = 4,  solidHeader = FALSE, status = "danger",
                                                   textInput("abortWorkflowID", "Cromwell workflow id to abort:",
                                                             value = ""),
                                                   actionButton(inputId = "abortWorkflow",
                                                                label = "Abort Workflow",
                                                                icon = icon("thumbs-down")),
                                                   verbatimTextOutput(outputId = "abortResult")
                                               )
                                           ),
                                 fluidRow(align = "center", 
                                          ## Validate a Workflow
                                          box(width = 12,  solidHeader = FALSE, status = "info",
                                              collapsible = TRUE, collapsed = FALSE,
                                              title = "Validate a Workflow",
                                              fileInput(inputId = "validatewdlFile", "Upload WDL File:",
                                                        accept = ".wdl"),
                                              fileInput(inputId = "validateinputFile", "Upload Consolidated Input JSON:",
                                                        accept = ".json"),
                                              actionButton(inputId = "validateWorkflow",
                                                           label = "Validate Workflow",
                                                           icon = icon("question-circle")),
                                              verbatimTextOutput(outputId = "validationResult")
                                          )),
                                 fluidRow(align = "center", 
                                          ## Troubleshoot a workflow via Glob
                                          box(width = 12,  solidHeader = FALSE, status = "info",
                                              collapsible = TRUE, collapsed = FALSE,
                                              title = "Troubleshoot a Workflow",
                                              textInput("troubleWorkflowID", "Cromwell workflow id to get metadata for:",
                                                        value = ""),
                                              actionButton(inputId = "troubleWorkflow",
                                                           label = "Get Workflow Metadata",
                                                           icon = icon("question-circle")),
                                              verbatimTextOutput(outputId = "troubleResult")
                                          ))
                         ),
                         tabItem(tabName = "tracking", 
                                 fluidRow(h2("Cromwell Workflow Tracking"), align = "center"),
                                 fluidRow(
                                   box(width = 4, 
                                       numericInput("daysToShow", "Days of History to Display:",
                                                    min = 1, max = 21, value = 1, step = 1),
                                       selectInput('workStatus', label = "Show Workflows with Status:",
                                                   choices = c("Submitted", "Running",
                                                               "Succeeded", "Failed", "Aborting", 
                                                               "Aborted"),  multiple = TRUE),
                                       textInput("workName", "Show workflows with this name \n(must be a valid name or nothing will show up):",
                                                 value = ""),
                                       actionButton(inputId = "trackingUpdate",
                                                    label = "Update View",
                                                    icon = icon("refresh"))
                                   ),
                                   box(width = 8,
                                       infoBoxOutput("submittedBox", width = 6),
                                       infoBoxOutput("inprogressBox", width = 6),
                                       infoBoxOutput("successBox", width = 6),
                                       infoBoxOutput("failBox", width = 6)
                                   )),
                                   fluidRow(

                                     box(width = 12, 
                                         title = "Workflows Run",
                                         collapsible = TRUE, solidHeader = TRUE,
                                         plotOutput("workflowDuration")
                                     )
                                   ),
                                   fluidRow(
                                     box(width = 12,
                                         title = "Cromwell Overview",
                                         collapsible = TRUE, solidHeader = TRUE,
                                         DTOutput("joblistCromwell")
                                     )
                                   ),
                                   fluidRow(h3("Workflow Specific Job Information (select a row above)"), align = "center",
                                   valueBoxOutput("pendingBatch", width = 3),
                                   infoBoxOutput("runningBatch", width = 3),
                                   infoBoxOutput("succeededBatch", width = 3),
                                   infoBoxOutput("failedBatch", width = 3)
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
                                       title = "Job Failures (only upon request)",
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
                                       title = "Call Caching (only upon request)", 
                                       collapsible = TRUE,solidHeader = TRUE,collapsed = FALSE,
                                       actionButton(inputId = "getCacheData",
                                                    label = "Get/Refresh Call Caching Metadata (may be slow for workflows with many calls and/or inputs)",
                                                    icon("refresh")),
                                       downloadButton("downloadCache", "Download Call Caching Data"),
                                       DTOutput("cachingListBatch"))
                                 ),
                                 fluidRow(
                                   box(width = 12,
                                       title = "Workflow Outputs (only upon request and after workflow succeeds)",
                                       collapsible = TRUE, solidHeader = TRUE, collapsed = FALSE,
                                       actionButton(inputId = "getOutputData",
                                                    label = "Get/Refresh Workflow Output Metadata",
                                                    icon("refresh")),
                                       downloadButton("downloadOutputs", "Download Workflow Output Data"),
                                       DTOutput("outputslistBatch"))
                                 )
                         )
                       )
                     )
)
