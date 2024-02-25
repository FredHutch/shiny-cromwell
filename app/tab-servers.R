tab_servers <- tabItem(
  tabName = "cromwell",
  #fluidRow(h2("Manage Your PROOF Server"), align = "center"),
  fluidRow(
    align = "left",
    box(
      width = 12, solidHeader = FALSE, status = "warning",
      collapsed = FALSE,
      title = "Manage your PROOF Server",
      p("Note: Hover over the ", icon("question-circle"), " icons to get more information about each item."),
      h3("Get a PROOF Server Running"),
      p("If you have a PROOF server running, 'Job Status' should indicate 'RUNNING'.  If not, click the 'Start a PROOF server' button."),
      uiOutput("proofStatusJobStatus"),
      actionButton(
          inputId = "cromwellStart",
          label = "Start a PROOF Server",
          icon = icon("play"),
          class = "btn-success"
      ),
      
      br(),
      br(),
      h4("Current Server information (if live)"),
      uiOutput("proofStatusWorkflowLogDir"),
      uiOutput("proofStatusScratchDir"),
      uiOutput("proofStatusServerTime"),
      uiOutput("proofStatusSlurmJobAccount"),
      br(),
      br(),
      h3("Stop your PROOF Server"),
      p(strong("Note"), " stopping your server requires making sure you mean it :)"),
      actionButton(
        inputId = "cromwellDelete",
        label = "Stop a PROOF Server",
        icon = icon("stop"),
        class = "btn-danger"
      ),
      br(),
      br(),
      h3("Troubleshoot Your PROOF Server"),
      p(strong("Note"), "If you're having trouble using your PROOF server, this information can be useful in getting help."),
      uiOutput("proofStatusSlurmJobId"),
      uiOutput("proofStatusCromwellDir"),
      uiOutput("proofStatusServerLogDir"),
      uiOutput("proofStatusSingularityCacheDir"),
      uiOutput("proofStatusUseAWS"),
      uiOutput("proofStatusUrlStr")
    ),
  ),
)
