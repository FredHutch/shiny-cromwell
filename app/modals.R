loginModal <- function(failed = FALSE, error = "Invalid username or password") {
  modalDialog(
    textInput("username", "Username",
      placeholder = "HutchNet username ('jane' of jane@fredhutch.org)",
      width = "60%"
    ),
    passwordInput("password", "Password",
      placeholder = "HutchNet password",
      width = "60%"
    ),
    if (failed) {
      div(tags$b(error, style = "color: red;"))
    },
    footer = tagList(
      modalButton("Cancel"),
      actionButton("submit", "Submit")
    ),
    easyClose = TRUE
  )
}

myCromwellModal <- function(failed = FALSE, error = "Invalid host/port") {
  modalDialog(
    textInput("ownCromwellURL", "URL",
      placeholder = "http://gizmot32:8000",
      width = "60%"
    ),
    if (failed) {
      div(tags$b(error, style = "color: red;"))
    },
    footer = tagList(
      modalButton("Cancel"),
      actionButton("submitOwnCromwell", "Submit")
    ),
    easyClose = TRUE
  )
}

cromwellStartModal <- function(failed = FALSE, error = "An error occurred") {
  modalDialog(
    title = "Start your PROOF Cromwell server",
    br(),
    textInput(
      inputId = "slurmAccount",
      label = div(HTML("Slurm account (optional)")),
      value = NULL
    ),
    if (failed) {
      div(tags$b(error, style = "color: red;"))
    },
    footer = tagList(
      modalButton("Cancel"),
      shinyFeedback::loadingButton(
        inputId = "beginCromwell",
        label = "Start",
        class = "btn btn-primary"
      )
    ),
    easyClose = TRUE
  )
}

verifyCromwellDeleteModal <- function(failed = FALSE, error = "Woops, an error! Contact DaSL") {
  modalDialog(
    title = "Delete your PROOF Cromwell server",
    "Permanently delete your PROOF Cromwell server. Although you can't undo this action, you can start up another one anytime!",
    br(),
    br(),
    textInput(
      inputId = "stopCromwell",
      label = div(HTML("To confirm deletion, type <em>delete me</em> into the field."))
    ),
    if (failed) {
      div(tags$b(error, style = "color: red;"))
    },
    footer = tagList(
      modalButton("Cancel"),
      shinyjs::disabled(
        shinyFeedback::loadingButton(
          inputId = "deleteCromwell",
          label = "Delete",
          class = "btn btn-warning"
        )
      )
    ),
    easyClose = TRUE
  )
}
