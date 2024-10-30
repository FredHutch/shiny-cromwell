alert_light <- function(...) {
  div(
    strong("Note:"),
    ...,
    class = "alert alert-light",
    role = "alert"
  )
}

card1 <- card(
  id = "cromwell_start_stop",
  class = "border border-warning",
  card_header(h2("Manage your PROOF Server")),
  alert_light("Hover over the ", icon("question-circle"), " icons to get more information about each item."),
  h4("Get a PROOF Server Running"),
  p("If you have a PROOF server running, 'Job Status' should indicate 'RUNNING'.  If not, click the 'Start a PROOF server' button."),
  uiOutput("proofStatusJobStatus"),
  actionButton(
    inputId = "cromwellStart",
    label = "Start a PROOF Server",
    icon = icon("play"),
    class = "btn-success",
    width = "250px"
  ),
  h5("Current PROOF server information (if live)"),
  uiOutput("proofStatusServerStartTime"),
  uiOutput("proofStatusWorkflowLogDir"),
  uiOutput("proofStatusScratchDir"),
  uiOutput("proofStatusServerTime"),
  uiOutput("proofStatusSlurmJobAccount"),
  h4("Stop your PROOF Server"),
  alert_light(
    "Stopping your server cannot be undone, but you can always make another one!",
    "Also, stopping your server does not stop your running jobs."
  ),
  actionButton(
    inputId = "cromwellDelete",
    label = "Stop a PROOF Server",
    icon = icon("stop"),
    class = "btn-danger",
    width = "250px"
  )
)

card2 <- card(
  id = "cromwell_troubleshoot",
  class = "border border-warning",
  card_header(h2("Troubleshoot Your PROOF Server")),
  alert_light("If you're having trouble using your PROOF server, this information can be useful in getting help."),
  uiOutput("proofStatusSlurmJobId"),
  uiOutput("proofStatusCromwellDir"),
  uiOutput("proofStatusServerLogDir"),
  uiOutput("proofStatusSingularityCacheDir"),
  uiOutput("proofStatusUseAWS"),
  uiOutput("proofStatusUrlStr")
)

tab_servers <- layout_column_wrap(
  width = 1/2,
  card1,
  card2
)
