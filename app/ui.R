library(shiny); library(shinydashboard);
library(data.table); library(DT); library(tidyverse); 
library(RColorBrewer) 
library(fh.wdlR)

ui <- dashboardPage( skin = "black",
  dashboardHeader(title = "Fred Hutch Cromwell Dashboard",
                  titleWidth = 450),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Job Tracking", tabName = "tracking", icon = icon("binoculars"),
               badgeLabel = "workflows", badgeColor = "purple")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "tracking", 
              fluidRow(h2("Cromwell Workflow Tracking"), align = "center"),
              fluidRow(
                box(width = 6, 
                    textInput("currentCromwellURL", "Current Cromwell host:port:",
                              value = ""),
                    numericInput("daysToShow", "Days of History to Display:",
                                 min = 1, max = 21, value = 1, step = 1),
                    actionButton(inputId = "trackingUpdate",
                                 label = "Update View",
                                 icon = icon("refresh")),
                ),
                box(width = 6,
                    infoBoxOutput("submittedBox", width = 6),
                    infoBoxOutput("inprogressBox", width = 6),
                    infoBoxOutput("successBox", width = 6),
                    infoBoxOutput("failBox", width = 6)
                ),
                fluidRow(
                  box(width = 6, 
                      title = "Workflow Status",
                      collapsible = TRUE, solidHeader = TRUE,
                      plotOutput("workflowStatus")
                  ),
                  box(width = 6, 
                      title = "Workflow Duration",
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
                fluidRow(h3("Workflow Specific Job Information"), align = "center"),
                valueBoxOutput("pendingBatch", width = 4),
                infoBoxOutput("runningBatch", width = 4),
                infoBoxOutput("failedBatch", width = 4),
                infoBoxOutput("succeededBatch", width = 4),
                infoBoxOutput("cacheHits", width = 4),
                infoBoxOutput("cacheMisses", width = 4)
              ),
              box(width = 12,
                  title = "Workflow Call Duration",
                  collapsible = TRUE, solidHeader = TRUE,
                  plotOutput("workflowTiming")
              ),
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
                    collapsible = TRUE,solidHeader = TRUE,collapsed = FALSE,
                    downloadButton("downloadFails", "Download Call Failure Data"),
                    DTOutput("failurelistBatch"))
              ),
              fluidRow(
                box(width = 12,
                    title = "Call Caching",
                    collapsible = TRUE,solidHeader = TRUE,collapsed = FALSE,
                    downloadButton("downloadCache", "Download Call Caching Data"),
                    DTOutput("cachingListBatch"))
              ),
              fluidRow(
                box(width = 12,
                    title = "Workflow Outputs",
                    collapsible = TRUE, solidHeader = TRUE, collapsed = FALSE,
                    downloadButton("downloadOutputs", "Download Workflow Output Data"),
                    DTOutput("outputslistBatch"))
              )
      )
    )
)
)
