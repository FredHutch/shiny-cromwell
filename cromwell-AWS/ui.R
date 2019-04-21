#require(devtools)
#devtools::install_github('FredHutch/tgR@v0.1.1') # Minimum version v0.1.1
library(shiny); library(shinydashboard);
library(data.table); library(DT);
library(tgR); library(tidyverse); 
library(RColorBrewer) 

##### Creds
# source("setEnviron.R") # for local use
# Requires Env vars to be set:  CROMWELLURL

daysOfHistory <- 3

ui <- dashboardPage( skin = "black",
  dashboardHeader(title = "Cromwell-AWS Batch Dashboard",
                  titleWidth = 450),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Job Submission", tabName = "submission", icon = icon("paper-plane"), 
               badgeLabel = "datasets", badgeColor = "maroon"),
      menuItem("Workflow Engine Tracking", tabName = "tracking", icon = icon("binoculars"),
               badgeLabel = "compute", badgeColor = "purple")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "submission", 
              fluidRow(align = "center",
                h2("Job Submission"),
                       actionButton(inputId = "submissionUpdate", 
                                    label = "Update View", 
                                    icon = icon("refresh"))),
              hr()
               ),
      tabItem(tabName = "tracking", 
              fluidRow(h2("Cromwell and AWS Batch Dashboard"), align = "center"),
              fluidRow(
                column(width = 6, align = "center",
                       infoBoxOutput("submittedBox", width = NULL)),
                column(width = 6, align = "center",
                       numericInput("daysToShow", "Days of History:",
                                    min = 1, max = 21, value = daysOfHistory, step = 1))
                      ),
              fluidRow(
                column(width = 6, align = "center",
                       infoBoxOutput("inprogressBox", width = NULL)),
                column(width = 6, align = "center",
                      actionButton(inputId = "trackingUpdate", 
                             label = "Update View", 
                             icon = icon("refresh")))
              ),
              fluidRow( 
                box(
                  title = "Workflow Status", color = "orange", solidHeader = TRUE,
                  collapsible = TRUE,
                  plotOutput("workflowStatus")
                )#,
                # box(
                #   title = "Workflow Duration", color = "orange", solidHeader = TRUE,
                #   collapsible = TRUE,
                #   plotOutput("workflowDuration")
                # )
              ),
              fluidRow( 
                box(width = 12,
                    title = "FieldRoast Overview", color = "purple", solidHeader = TRUE,
                    collapsible = TRUE,
                    DTOutput("joblistCromwell")
                )
              ),
              fluidRow(h3("Workflow Specific Job Information")),
              fluidRow(
                valueBoxOutput("pendingBatch", width = 3),
                infoBoxOutput("runningBatch", width = 3),
              # ),
              # fluidRow(
                infoBoxOutput("failedBatch", width = 3),
                infoBoxOutput("succeededBatch", width = 3)
              ),
              
              fluidRow(
                box(width = 12,
                  title = "AWS Batch Job List", color = "purple", solidHeader = TRUE,
                  collapsible = TRUE,
                  DTOutput("tasklistBatch")
                )),
              fluidRow(
                box(width = 12,
                      title = "AWS Batch Job Failures", color = "purple", solidHeader = TRUE,
                      collapsible = TRUE,
                      DTOutput("failurelistBatch")
                  )
              ))
      )
    )
)
