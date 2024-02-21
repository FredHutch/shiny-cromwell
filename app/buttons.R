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

logInCromwellButton <-
  actionButton(
    inputId = "ownCrom",
    label = "My Own Cromwell",
    icon = icon("plug-circle-xmark")
  )
logOutCromwellButton <-
  actionButton(
    inputId = "proofCromwellLogout",
    label = " Exit",
    class = "btn-danger",
    style = "color: white;",
    icon = icon("arrow-right-to-bracket")
  )
