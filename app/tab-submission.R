tab_submission <- tabItem(
  tabName = "submission",
  fluidRow(h2("Run Workflows on Cromwell"), align = "center"),
  fluidRow(
    box(
      width = 12, solidHeader = FALSE, status = "success",
      collapsible = TRUE, collapsed = FALSE,
      title = "Submit a Workflow",
      p("Here you can submit your workflow to your Cromwell server for execution.  Only a WDL is required. Up to two different input JSONs
                                           can be uploaded (if variables are specified in both, the second input's variable value will overwrite the first). Workflow options
                                           can be provided if desired.  Workflow labels are user-defined values you'd like to use to describe your workflows for your own
                                           future reference. "),
      column(
        width = 6,
        fileInput(
          inputId = "wdlFile", "Upload WDL (required):",
          accept = ".wdl"
        ),
        fileInput(
          inputId = "inputJSON", "Upload First Input JSON (optional):",
          accept = ".json"
        ),
        fileInput(
          inputId = "input2JSON", "Upload Second Input JSON (optional):",
          accept = ".json"
        ),
        actionButton(
          inputId = "submitWorkflow",
          label = "Submit Workflow",
          icon = icon("paper-plane"),
          class = "btn-info"
        ),
        verbatimTextOutput(outputId = "submissionResult"),
        br(),
        actionButton("resetSubmission", "Reset")
      ),
      column(
        width = 6,
        fileInput(
          inputId = "workOptions", "Upload Workflow Options JSON (optional):",
          accept = ".json"
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
        )
      )
    )
  )
)
