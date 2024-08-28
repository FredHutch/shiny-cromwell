source("ui_components.R")
library(bsicons)
library(htmltools)
library(glue)

sidebar_tracking <- sidebar(
  numericInput("daysToShow", "Days of History to Display:",
      min = 1, max = 21, value = 1, step = 1),

    textInput("workName", "Filter for workflows with name:",
      value = "",
      placeholder = "myCustomWorkflow",
    ),
    selectInput("workStatus",
      label = "Filter for Workflows with Status(es):",
      choices = c(
        "Submitted", "Running",
        "Succeeded", "Failed", "Aborting",
        "Aborted"
      ),
      multiple = TRUE,
    ),
    actionButton(
        inputId = "trackingUpdate",
        label = "Update View",
        icon = icon("refresh")
    )
)

card_tracking_intro <- card(
  max_height = "200px",
  card_header(h3("Track your Workflows")),
  p("Once you've submitted workflows, you can track the status of all the workflows you've submitted
    in the specified time range by clicking `Update View`.  If you use PROOF a lot,
    this and the filtering tools below can help you return only the workflows you're interested in monitoring,
    making tracking and the application itself much faster. "),
  card_body(
    fillable = FALSE,
    tags$button(
      "Submitted",
      span(
        textOutput("submittedBoxValue"),
        class = "badge text-bg-dark"
      ),
      class = "btn btn-secondary btn-sm"
    ),
    tags$button(
      "Succeeded",
      span(
        textOutput("successBoxValue"),
        class = "badge text-bg-dark"
      ),
      class = "btn btn-success btn-sm"
    ),
    tags$button(
      "Failed",
      span(
        textOutput("failBoxValue"),
        class = "badge text-bg-dark"
      ),
      class = "btn btn-danger btn-sm"
    ),
    tags$button(
      "Running",
      span(
        textOutput("inprogressBoxValue"),
        class = "badge text-bg-dark"
      ),
      class = "btn btn-info btn-sm"
    )
  )
)

card_timing <- card(
  plotOutput("workflowDuration")
)

card_workflows_run <- card(
  DTOutput("joblistCromwell"),
  card_footer(table_footer())
)

tab_tracking <- page_sidebar(
  fillable = TRUE,
  sidebar = sidebar_tracking,
  card_tracking_intro,
  navset_card_underline(
    nav_panel(
      title = "Workflow Timing",
      card_timing
    ),
    nav_panel(
      title = "Workflows Run",
      card_workflows_run
    )
  )
)
