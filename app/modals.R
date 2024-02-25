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
    p("Do not use your PROOF based Cromwell server here", style = "color: #B5ABAB;"),
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
    title = "Start your PROOF server",
    br(),
    textInput(
      inputId = "slurmAccount",
      label = div(HTML("Specify a non-default Slurm account (optional)")),
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

verifyCromwellDeleteModal <- function(failed = FALSE, error = "Woops, an error! Contact DaSL at wilds@fredhutch.org.") {
  modalDialog(
    title = "Stop your PROOF server",
    "Permanently stop your PROOF server. Although you can't undo this action, you can start up another one anytime!",
    br(),
    br(),
    textInput(
      inputId = "stopCromwell",
      label = div(HTML("To stop your server, confirm deletion by typing the text '<em>delete me</em>' into the field."))
    ),
    if (failed) {
      div(tags$b(error, style = "color: red;"))
    },
    footer = tagList(
      modalButton("Cancel"),
      shinyjs::disabled(
        shinyFeedback::loadingButton(
          inputId = "deleteCromwell",
          label = "Stop Server",
          class = "btn btn-warning"
        )
      )
    ),
    easyClose = TRUE
  )
}
