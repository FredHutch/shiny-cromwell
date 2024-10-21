tab_troublehsoot <- nav_panel(NULL,
  card(
    class = "border border-primary",
    full_screen = TRUE,
    card_header(h2("Troubleshoot a Workflow")),
    card_body(
      fillable = FALSE,
      p("When a workflow fails but no jobs were started, or there appears to be no clear reason for a workflow to have failed, this tool can provide you the entire set of workflow metadata Cromwell has for your workflow in it's raw and unprocessed (json) form. For complex workflows, this can be rather large (and ugly!)."),
      textInput(
        inputId = "troubleWorkflowID",
        label = "Workflow id to get metadata for:",
        value = "",
        placeholder = "577b9aa4-b26b-4fd6-9f17-7fb33780bbd0",
        width = "25%"
      ),
      actionButton(
        inputId = "troubleWorkflow",
        class = "btn-sm",
        label = "Get Workflow Metadata",
        icon = icon("wrench"),
        width = "250px"
      ),
      actionButton("resetTrouble", "Reset", class = "btn-sm", width = "250px"),
      br(),
      verbatimTextOutput(outputId = "troubleResult")
    )
  )
)
