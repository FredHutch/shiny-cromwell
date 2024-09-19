tab_validate <- card(
  id = "validate",
  class = "border border-primary",
  card_header(h2("Validate a Workflow")),
  p("Note: Your WDL may validate but still fail; for instance if you lack permissions for files or directories that the workflow needs to execute its tasks"),
  fileInput(
    inputId = "validatewdlFile", "Upload WDL File (required):",
    accept = ".wdl"
  ),
  fileInput(
    inputId = "validateinputFile", "Upload Consolidated Input JSON (optional):",
    accept = ".json"
  ),
  actionButton(
    inputId = "validateWorkflow",
    label = "Validate Workflow",
    icon = icon("stethoscope"),
    width = "250px"
  ),
  actionButton("resetValidate", "Reset", width = "250px"),
  verbatimTextOutput(outputId = "validationResult")
)
