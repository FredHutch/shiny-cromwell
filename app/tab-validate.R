tab_validate <- tabItem(
  tabName = "validate",
  fluidRow(h2("Validate a Workflow"), align = "center"),
  fluidRow(
    align = "left",
    ## Validate a Workflow
    box(
      width = 12, solidHeader = FALSE, status = "info",
      collapsible = TRUE, collapsed = FALSE,
      title = "Validate a Workflow",
      p("This tool will check to see if the WDL (and it's input JSON if you choose to upload it)
                                          are both in the correct format required and if not will give you some
                                          hints as to what might be wrong.  If your workflow does not validate, the feedback often hints
                                          at a problem just below where your error actually is. "),
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
        class = "btn-info"
      ),
      br(),
      br(),
      actionButton("resetValidate", "Reset"),
      verbatimTextOutput(outputId = "validationResult")
    )
  )
)
