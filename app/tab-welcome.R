welcome_servers_box <- box(
  id = "boxServers", title = "PROOF Server", width = 4, solidHeader = TRUE, status = "warning", icon = icon("truck-fast"),
  shiny::markdown("This tab allows you to:
  - Start or delete your PROOF server
  - Get metadata for your PROOF server"),
  align = "left"
)

tab_welcome <- tabItem(
  tabName = "welcome",
  fluidRow(
    column(
      width = 12,
      h2("What is this app?"),
      shiny::includeMarkdown("about.md")
    )
  ),
  fluidRow(
    align = "left",
    h2("Dashboard Tabs"),
    uiOutput("toggleServersBox"),
    box(
      id = "boxValidate", title = "Validate", width = 6, solidHeader = TRUE, status = "primary", icon = icon("stethoscope"),
      shiny::markdown("This tab allows you to:
      - Validate a workflow you'd like to run"),
      align = "left"
    ),
    box(
      id = "boxSubmit", title = "Submit Jobs", width = 6, solidHeader = TRUE, status = "success", icon = icon("paper-plane"),
      shiny::markdown("This tab allows you to:
        - Run a workflow"),
      align = "left"
    )
  ),
  fluidRow(
    align = "center",
    box(
      id = "boxTrack", title = "Track Jobs", width = 6, solidHeader = TRUE, status = "info", icon = icon("binoculars"),
      shiny::markdown("This tab allows you to:
      - Query your server database for the jobs run the most recent days (your choice how far back to go)
      - See statuses of all your workflows
      - Look within a workflow at the individual calls, failures and call caching results
      - Download a list of the final workflow outputs for further processing"),
      align = "left"
    ),
    box(
      id = "boxTrouble", title = "Troubleshoot", width = 6, solidHeader = TRUE, status = "danger", icon = icon("wrench"),
      shiny::markdown("This tab allows you to:
      - Abort a workflow
      - Troubleshoot the workflow itself by looking at the entire raw json of workflow metadata (it's especially helpful for complex workflows)"),
      align = "left"
    )
  )
)
