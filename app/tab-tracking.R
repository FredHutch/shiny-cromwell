source("ui_components.R")
library(bsicons)
library(htmltools)
library(glue)
library(bslib)

sidebar_tracking <- sidebar(
  # numericInput("daysToShow", "Days of History to Display:",
  #     min = 1, max = NA, value = 1, step = 1),
  actionButton(
    inputId = "trackingUpdate",
    label = "Refresh data",
    icon = icon("refresh")
  ),
  popover(
    bsicons::bs_icon("question-circle"),
    p("Data for the past 60 days"),
    title = "Help",
    placement = "bottom"
  ),
  hr(),
  textInput(
    inputId = "workName", 
    label = "Workflow name",
    value = "",
    placeholder = "myCustomWorkflow",
  ),
  selectInput(
    inputId = "workStatus",
    label = "Status",
    choices = c(
      "Submitted", "Running",
      "Succeeded", "Failed", "Aborting",
      "Aborted"
    ),
    multiple = TRUE,
  ),
  dateRangeInput(
    inputId = "runs_date", 
    label = "Date Range",
    start = "2024-01-01",
    end = lubridate::today(),
    format = "m/d/yy"
  ),
  selectInput(
    inputId = "",
    label = "Sort",
    choices = c(
      "Newest to oldest",
      "Oldest to newest"
    ),
    selected = "Newest to oldest",
    multiple = FALSE
  ),
  actionButton("resetTrackingFilters", "Reset all filters", class = "btn-sm")
  # actionButton(
  #   inputId = "trackingUpdate",
  #   label = "Refresh data",
  #   icon = icon("refresh")
  # )
)

card_tracking_intro <- card(
  # max_height = "100px",
  fill = FALSE,
  card_header(
    h3("Track your Workflows"),
    popover(
      bsicons::bs_icon("question-circle"),
      p("Once you've submitted workflows, you can track the status of all the workflows you've submitted
        in the specified time range by clicking `Update View`.  If you use PROOF a lot,
        this and the filtering tools below can help you return only the workflows you're interested in monitoring,
        making tracking and the application itself much faster. "),
      title = "Help",
      placement = "left"
    ),
    class = "d-flex align-items-center justify-content-between gap-1"
  ),
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

card_workflow_runs <- card(
  DTOutput("joblistCromwell"),
  card_footer(table_footer())
)

workflow_cards <- layout_column_wrap(
  width = 1/1,
  fillable = FALSE,
  uiOutput("workflows_cards")
)

tab_tracking <- page_sidebar(
  fillable = FALSE,
  sidebar = sidebar_tracking,
  card_tracking_intro,
  navset_card_underline(
    nav_panel(
      title = "Workflow Runs",
      # dateRangeInput("runs_date",
      #   label = "Date Range",
      #   start = "2024-01-01",
      #   end = "2024-08-30"
      # ),
      workflow_cards
      # card_workflow_runs
    ),
    nav_panel(
      title = "Workflow Timing",
      card_timing
    )
  )
)
