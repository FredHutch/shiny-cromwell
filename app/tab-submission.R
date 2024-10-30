tab_submission <- card(
  id = "submission",
  card_header(h2("Submit a Workflow")),
  class = "border border-success",
  tags$ul(
    tags$li("Only a WDL is required for submission (requires v1.0)"),
    tags$li("Label your WDL workflow to aid in tracking it after submission (we use random strings if these are missing)"),
    tags$li("If two input JSON files are uploaded with identical variables, the second input's variable value will overwrite the first")
  ),
  layout_columns(
    col_widths = 6,
    fillable = FALSE,
    card(
      fileInput(
        inputId = "wdlFile", "Upload WDL (required):",
        accept = ".wdl"
      ),
      textInput(
        inputId = "labelValue", "Workflow Label (optional)",
        value = "",
        placeholder = "e.g., First Try"
      ),
      textInput(
        inputId = "seclabelValue", "Secondary Workflow Label (optional)",
        value = "",
        placeholder = "e.g., Cohort 2"
      ),
      shinyjs::disabled(actionButton(
        inputId = "submitWorkflow",
        label = "Submit Workflow",
        icon = icon("paper-plane"),
        width = "250px",
        class = "btn-success"
      )),
      card(
        uiOutput(outputId = "submissionResult")
      ),
      actionButton(
        inputId = "resetSubmission",
        label = "Reset Form",
        width = "250px"
      )
    ),
    layout_columns(
      col_widths = 12,
      fillable = FALSE,
      card(
        fileInput(
          inputId = "inputJSON", "First Input JSON (optional):",
          accept = ".json"
        ),
        fileInput(
          inputId = "input2JSON", "Second Input JSON (optional):",
          accept = ".json"
        ),
        fileInput(
          inputId = "workOptions", "Workflow Options JSON (optional):",
          accept = ".json"
        )
      )
    )
  )
)
