library(listviewer)

tab_viewer <- tabItem(
  tabName = "viewer",
  fluidRow(
    box(
      title = "View Workflow Inputs",
      width = 12,
      textOutput("currentWorkflowId"),
      p(""),
      actionButton("linkToTrackingTab", "Back to Track Jobs Tab"),
      p(""),
      reactjsonOutput("workflowInp", height = "100%")
    )
  )
)
