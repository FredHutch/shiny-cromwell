tab_troublehsoot <- tabItem(
  tabName = "troubleshoot",
  fluidRow(
    align = "left",
    box(
      title = "Abort a Workflow",
      p("Aborting a workflow cannot be undone and can take some time to fully stop all jobs submitted in complex or highly parallelized workflows."),
      collapsible = TRUE, collapsed = FALSE,
      width = 12, solidHeader = FALSE, status = "danger",
      textInput(
        inputId = "abortWorkflowID",
        label = "Workflow id to abort:",
        value = "",
        placeholder = "577b9aa4-b26b-4fd6-9f17-7fb33780bbd0",
        width = "30%"
      ),
      actionButton(
        inputId = "abortWorkflow",
        label = "Abort Workflow",
        icon = icon("thumbs-down"),
        class = "btn-info"
      ),
      actionButton("resetAbort", "Reset"),
      verbatimTextOutput(outputId = "abortResult")
    )
  ),
  fluidRow(
    align = "left",
    ## Troubleshoot a workflow via Glob
    box(
      width = 12, solidHeader = FALSE, status = "info",
      collapsible = TRUE, collapsed = FALSE,
      title = "Troubleshoot a Workflow",
      p("When a workflow fails but no jobs were started, or there appears to be no clear reason for a workflow to have failed, this tool can provide you the entire set of workflow metadata Cromwell has for your workflow in it's raw and unprocessed (json) form. For complex workflows, this can be rather large (and ugly!)."),
      textInput(
        inputId = "troubleWorkflowID",
        label = "Cromwell workflow id to get metadata for:",
        value = "",
        placeholder = "577b9aa4-b26b-4fd6-9f17-7fb33780bbd0",
        width = "30%"
      ),
      actionButton(
        inputId = "troubleWorkflow",
        label = "Get Complete Workflow Metadata",
        icon = icon("question-circle"),
        class = "btn-info"
      ),
      actionButton("resetTrouble", "Reset"),
      verbatimTextOutput(outputId = "troubleResult")
    )
  )
)