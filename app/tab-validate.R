tab_validate <- tabItem(
  tabName = "validate",
  #fluidRow(h2("Validate a Workflow"), align = "center"),
  fluidRow(
    align = "left",
    ## Validate a Workflow
    box(
      width = 12, solidHeader = FALSE, status = "primary",
      collapsible = FALSE, collapsed = FALSE,
      title = "Validate a Workflow",
      p("Before running a WDL, you'll want to check to see if the WDL (and its input JSON, if you choose to upload it)
                                          are in the correct format required.   If they are not, the app will give you some
                                          hints as to what might be wrong.  
                                          Tip:  If your workflow does not validate, the feedback often hints
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
        icon = icon("stethoscope")
      ),
      #br(),
      #br(),
      actionButton("resetValidate", "Reset"),
      verbatimTextOutput(outputId = "validationResult")
    )
  )
)
