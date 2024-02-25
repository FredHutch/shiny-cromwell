check_url <- function(x) {
  tmp <- tryCatch(cromwell_version(x), error = function(e) e)
  !rlang::is_error(tmp)
}

validatorOwnCromwell <- function(x) {
  iv <- InputValidator$new()
  # the URL is required
  iv$add_rule("ownCromwellURL", sv_required())
  # If URL contains "fhcrc.org" throw message redirecting them
  iv$add_rule("ownCromwellURL",
    sv_regex("fhcrc\\.org", "That appears to be a URL for a PROOF server. Please click the PROOF Login button", invert = TRUE)
  )
  # https://gizmoj16.fhcrc.org:60963/
  # URL must match this regex
  iv$add_rule("ownCromwellURL",
    sv_regex("^https?://[a-zA-Z0-9]*:[0-9]*/?$", "Doesn't match pattern")
  )
  # URL must be a valid URL via rcromwell::cromwell_version
  iv$add_rule("ownCromwellURL",
    ~ if (!check_url(.)) "Not a valid URL")
  return(iv)
}
