tab_servers <- tabItem(
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
      p("Hover over the ", icon("question-circle"), " icon to get more information about each item"),
      h4("Server information"),
      uiOutput("proofStatusJobStatus"),
      uiOutput("proofStatusWorkflowLogDir"),
      uiOutput("proofStatusScratchDir"),
      uiOutput("proofStatusServerTime"),
      uiOutput("proofStatusSlurmJobAccount"),
      br(),
      h4("Troubleshooting"),
      uiOutput("proofStatusSlurmJobId"),
      uiOutput("proofStatusCromwellDir"),
      uiOutput("proofStatusServerLogDir"),
      uiOutput("proofStatusSingularityCacheDir"),
      uiOutput("proofStatusUseAWS"),
      uiOutput("proofStatusUrlStr")
    ),
  ),
)
