library(memoise)
library(bsicons)
library(parsedate)
library(uuid)
library(rclipboard)
library(shinyFeedback)
library(shinycssloaders)

# Wrapped in a function so we can change options in one place
load_spinner <- function(...) {
  shinycssloaders::withSpinner(...)
}

# exact copy from shiny:::validateIcon
proofValidateIcon <- function (icon) {
  if (is.null(icon) || identical(icon, character(0))) {
    return(icon)
  } else if (inherits(icon, "shiny.tag") && icon$name == "i") {
    return(icon)
  } else {
    stop("Invalid icon. Use Shiny's 'icon()' function to generate a valid icon")
  }
}

# copy from shinyFeedback::loadingButton adding ability to pass in:
# - onclick (most importantly)
# - icon (less important)
proofLoadingButton <- function(inputId, label,
  class = "btn btn-primary", style = "width: 150px;",
  loadingLabel = "Loading...", loadingSpinner = "spinner",
  loadingClass = NULL, loadingStyle = NULL, icon = NULL, ...) {

  shiny::addResourcePath("shinyfeedback", system.file("assets",
    package = "shinyFeedback"))
  if (is.null(loadingClass)) {
    loadingClass <- class
  }
  if (is.null(loadingStyle)) {
    loadingStyle <- style
  }
  rOptions <- list(label = label, class = class, style = style,
    loadingLabel = loadingLabel, loadingSpinner = loadingSpinner,
    loadingClass = loadingClass, loadingStyle = loadingStyle)
  jsonOptions <- jsonlite::toJSON(rOptions, auto_unbox = TRUE)
  htmltools::span(
    class = "sf-loading-button",
    id = paste0("sf-loading-button-", inputId),
    tags$button(
      id = inputId,
      class = class,
      style = style,
      list(proofValidateIcon(icon), label),
      ...
    ),
    tags$head(
      htmltools::singleton(fontawesome::fa_html_dependency()),
      htmltools::singleton(
        tags$script(src = "shinyfeedback/js/loadingbutton.js?version=1"),
      ),
      tags$script(sprintf("loadingButtons.create('%s', %s)", inputId, jsonOptions))
    )
  )
}

# coerce dates to PT from UTC
as_pt <- function(x) {
  stamp("Mar 1, 1999 1:00", quiet = TRUE)(x)
}

validate_workflowid <- function(x) {
  shiny::validate(
    shiny::need(
      uuid::UUIDvalidate(x),
      "That doesn't look like a workflow ID; check your ID"
    )
  )
}

# get lastet commit - memoised so after first call its cached
git_last <- memoise(
  function(branch = "dev", fallback = "") {
    last <- tryCatch(
      {
        resp <- httr::GET(
          url = "https://api.github.com",
          path = glue("repos/FredHutch/shiny-cromwell/commits/{branch}"),
          query = list(per_page = 1)
        )
        httr::content(resp)
      },
      error = function(e) e
    )
    if (rlang::is_error(last)) fallback else last
  }
)

try_env <- function(x, default) {
  tmp <- tryCatch(Sys.getenv(x), error = function(e) e)
  if (rlang::is_error(tmp)) {
    return(default)
  }
  if (nzchar(tmp)) tmp else default
}

COMMIT_BRANCH <- try_env("CI_COMMIT_BRANCH", "dev")
COMMIT_SHA <- try_env("CI_COMMIT_SHA", "dev")
COMMIT_SHORT_SHA <- try_env(
  "CI_COMMIT_SHORT_SHA",
  git_last(COMMIT_BRANCH)$sha
)
COMMIT_TIMESTAMP <- try_env(
  "CI_COMMIT_TIMESTAMP",
  git_last(COMMIT_BRANCH)$commit$committer$date
)

make_copybtn <- function(x, clip_prefix, tooltip) {
  i <- sample.int(1e4, 1)
  as.character(
    rclipButton(
      paste0(clip_prefix, i),
      label = "",
      clipText = x,
      tooltip = tooltip,
      icon = icon("copy"),
      class = "btn-secondary btn-sm",
      options = list(delay = list(show = 800, hide = 100), trigger = "hover")
    )
  )
}

make_wdlbtn <- function(workflow_id) {
  as.character(
    actionButton(
      inputId = glue("wdlview_{workflow_id}"),
      label = bsicons::bs_icon("search"),
      class = "btn-sm",
      onclick = 'Shiny.setInputValue(\"wdlview_btn\", this.id, {priority: \"event\"})'
    )
  )
}

wdl_to_file <- function(workflow_id, url, token) {
  glob <- cromwell_glob(workflow_id, url = url, token = token)
  wdl_str <- glob$submittedFiles$workflow
  tfile <- tempfile(pattern = "wdlize_wdl", fileext = ".wdl")
  cat(wdl_str, "\n", file = tfile)
  return(tfile)
}

wdl2mermaid <- function(wdlFileName) {
  outfile <- tempfile(pattern = "wdl2mermaid", fileext = ".mermaid")
  system2(
    "wdl_to_mermaid",
    args = c(shQuote(wdlFileName), "--print-flowchart"),
    stdout = outfile
  )
  ret <- readLines(outfile)
  unlink(outfile)
  paste(ret, collapse = "\n")
}

mermaid_container <- function(code) {
  tags$div(
    id = "mermaid-container",
    tags$div(id = "mermaid-diagram", `class` = "mermaid", width = "auto", height = "auto", HTML(code)),
    tags$script(HTML("
      mermaid.initialize({ startOnLoad: true });
      mermaid.init(undefined, '#mermaid-diagram');
    "))
  )
}

card_header_color <- function(status) {
  switch(status,
    Submitted = "primary",
    Pending = "info",
    Running = "warning",
    Succeeded = "success",
    Failed = "danger",
    Aborted = "secondary",
    "secondary"
  )
}

plot_status_color <- function(status) {
  dplyr::case_match(
    status,
    "Submitted" ~ "#316df4",
    "Pending" ~ "#5dc7eb",
    "Running" ~ "#f5c344",
    "Succeeded" ~ "#408558",
    "Failed" ~ "#cb444a",
    "Aborted" ~ "#353a40",
    .default = "#353a40"
  )
}

status_color_map <- list(
  "Submitted" = "#316df4",
  "Pending" = "#5dc7eb",
  "Running" = "#f5c344",
  "Succeeded" = "#408558",
  "Failed" = "#cb444a",
  "Aborted" = "#353a40"
)

parse_date_tz <- function(x, tz = "America/Los_Angeles") {
  parsedate::parse_date(x, default_tz = tz)
}

alert <- function(..., class = "alert alert-primary") {
  div(
    ...,
    class = class,
    role = "alert"
  )
}
