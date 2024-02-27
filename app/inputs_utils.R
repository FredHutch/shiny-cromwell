reactiveInput <- function(rval, path) {
  if (is.null(rval)) {
    return(NULL)
  } else if (rval == 'loaded') {
    return(path)
  } else if (rval == 'reset') {
    return(NULL)
  }
}

reset_inputs <- function(inputs) {
  purrr::map(inputs, shinyjs::reset)
}
