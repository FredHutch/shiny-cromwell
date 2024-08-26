tab_wdl <- tabItem(
  tabName = "wdl",
  fluidPage(
    actionButton("linkToTrackingTab", "Back to Track Jobs Tab"),
    uiOutput("mermaid_diagram")
  )
)
