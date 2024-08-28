library(listviewer)

tab_details <- card(
  id = "details",
  navset_underline(
    nav_panel(
      title = "View Workflow Inputs",
      textOutput("currentWorkflowId"),
      p(""),
      actionButton("linkToTrackingTab", "Back to Track Jobs Tab", width = "250px"),
      p(""),
      reactjsonOutput("workflowInp", height = "100%")
    ),
    nav_panel(
      title = "Mermaid",
      actionButton("linkToTrackingTab", "Back to Track Jobs Tab", width = "250px"),
      uiOutput("mermaid_diagram")
    )
  )
)
