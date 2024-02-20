logInButton <-
  actionButton(
    inputId = "proofAuth",
    label = "PROOF Login"
  )
logOutButton <-
  actionButton(
    inputId = "proofAuthLogout",
    label = " Log out",
    class = "btn-danger",
    style = "color: white;",
    icon = icon("arrow-right-to-bracket")
  )

ownCromwellButton <-
  actionButton(
    inputId = "ownCrom",
    label = "My Own Cromwell",
    icon = icon("plug-circle-xmark")
  )
