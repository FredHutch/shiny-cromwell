library(memoise)

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
