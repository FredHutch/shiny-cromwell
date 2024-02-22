library(memoise)

# coerce dates to PT from UTC
as_pt <- function(x) {
  stamp("Mar 1, 1999 1:00")(with_tz(ymd_hms(x, tz = "UTC"), "America/Los_Angeles"))
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
git_sha <- memoise(
  function(fallback_ref = "dev") {
    sha <- tryCatch(
      {
        resp <- httr::GET(
          url = "https://api.github.com",
          path = "repos/FredHutch/shiny-cromwell/commits/dev",
          query = list(per_page = 1)
        )
        httr::content(resp)$sha
      },
      error = function(e) e
    )
    if (rlang::is_error(sha)) fallback_ref else sha
  }
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
