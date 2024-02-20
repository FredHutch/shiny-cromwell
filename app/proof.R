proof_wait_for_up <- function(token) {
  not_up <- TRUE
  while (not_up) {
    out <- proof_status(token = token)
    if (out$jobStatus == "RUNNING") not_up <- FALSE
  }
  out$cromwellUrl
}

proof_wait_for_down <- function(token) {
  up <- TRUE
  while (up) {
    cromwell_url <- proof_status(token = token)$cromwellUrl
    if (is.null(cromwell_url)) up <- FALSE
  }
}

proof_loggedin <- function(token) {
  is.character(token) && nzchar(token)
}

cromwell_version_safe <- function(url, token) {
  tmp <- tryCatch(
    cromwell_version(url = url, token = token),
    error = function(e) e
  )
  list(
    result = if (rlang::is_error(tmp)) FALSE else tmp,
    error = if (rlang::is_error(tmp)) tmp$message else NULL
  )
}

proof_loggedin_serverup <- function(url, token) {
  proof_loggedin(token) && rlang::is_list(cromwell_version_safe(url, token)$result)
}

proof_serverup <- function(url, token) {
  rlang::is_list(cromwell_version_safe(url, token)$result)
}

stop_safe <- function(fun, ..., message) if (!fun(...)) stop(safeError(message))
stop_safe_loggedin_serverup <- function(url, token, own) {
  if (own) return()
  stop_safe(proof_loggedin,
    token = token,
    message = "Either use your own Cromwell URL or log in with PROOF")
  stop_safe(proof_serverup,
    url = url,
    token = token,
    message = "Your Cromwell server is not up!")
}
