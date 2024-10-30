source("ui_components.R")
library(bsicons)
library(htmltools)
library(glue)
library(bslib)
library(shinycssloaders)

sidebar_tracking <- sidebar(
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
)

card_tracking_intro <- card(
  fill = FALSE,
  card_header(
    h3("Track your Workflows"),
    popover(
      bsicons::bs_icon("question-circle"),
      p("Click",  strong("Refresh data"), " to update data on this page.
        Use the filtering tools in the sidebar to help you return only
        the workflows you're interested in monitoring."),
      title = "Help",
      placement = "left"
    ),
    class = "d-flex align-items-center justify-content-between gap-1"
  ),
  card_body(
    fillable = FALSE,
    uiOutput("trackingSummaryStats")
  )
)

card_timing <- card(
  plotOutput("workflowDuration")
)

workflow_cards <- layout_column_wrap(
  width = 1/1,
  fillable = FALSE,
  shinycssloaders::withSpinner(
    uiOutput("workflows_cards")
  )
)

tab_tracking <- page_sidebar(
  fillable = FALSE,
  sidebar = sidebar_tracking,
  card_tracking_intro,
  navset_card_underline(
    nav_panel(
      title = "Workflow Runs",
      workflow_cards
    ),
    nav_panel(
      title = "Workflow Timing",
      card_timing
    )
  )
)
