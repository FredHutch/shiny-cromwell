library(memoise)
library(bsicons)

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
    ## FIXME: remove below comments and dummy list when internet back
    # last <- tryCatch(
    #   {
    #     resp <- httr::GET(
    #       url = "https://api.github.com",
    #       path = glue("repos/FredHutch/shiny-cromwell/commits/{branch}"),
    #       query = list(per_page = 1)
    #     )
    #     httr::content(resp)
    #   },
    #   error = function(e) e
    # )
    # if (rlang::is_error(last)) fallback else last
    list(sha = "adsfadf", commit = list(commmitter = list(date = "asdafd")))
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

abbreviate <- function(x, last = 100) {
  if (nchar(x) < 100) return(x)
  paste0(substring(x, 1, last), " ...")
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
    Failed = "danger",
    Succeeded = "success",
    Running = "warning",
    Pending = "info",
    "secondary"
  )
}
