tab_submission <- tabItem(
  tabName = "submission",
  fluidRow(
    box(
        title = "Submit a Workflow",
      width = 12, solidHeader = FALSE, status = "success",
      collapsible = FALSE, collapsed = FALSE,
      #title = "Submit a Workflow",
      p("Submit your validated workflow to your PROOF server for execution by uploading your files and clicking `Submit Workflow`. 
                                        Only a WDL is required, and up to two optional input JSONs
                                           can be uploaded (if identical variables are specified in both, the second input's variable value will overwrite the first). A json describing workflow options
                                           can be provided if desired.  Workflow labels are user-defined values you'd like to use to describe your workflows in the job tracking tab of this app. 
                                            Note: to submit new workflows, simply upload your new files and hit `Submit Workflow` again.  You can use the `Reset Form` button if you need to clear files before uploading new ones. "),
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
          icon = icon("paper-plane")
        ),
        actionButton("resetSubmission", "Reset Form"),
        verbatimTextOutput(outputId = "submissionResult")
        
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
