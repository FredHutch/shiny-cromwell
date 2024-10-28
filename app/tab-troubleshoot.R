library(shinyjs)

tab_troublehsoot <- nav_panel(title = "Troubleshoot",
  card(
    class = "border border-primary",
    full_screen = TRUE,
    card_header(h2("Troubleshoot a Workflow")),
    card_body(
      fillable = FALSE,
      p("When a workflow fails but no jobs were started, or there appears to be no clear reason for a workflow to have failed, this tool can provide you the entire set of workflow metadata Cromwell has for your workflow in it's raw and unprocessed (json) form. For complex workflows, this can be rather large (and ugly!)."),
      br(),
      verbatimTextOutput(outputId = "troubleResult")
    )
  ),
  shinyjs::useShinyjs()
)
