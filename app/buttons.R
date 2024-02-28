logInButton <-
  actionButton(
    inputId = "proofAuth",
    label = "PROOF Login",
    icon = icon("truck-fast")
    
  )
logOutButton <-
  actionButton(
    inputId = "proofAuthLogout",
    label = "PROOF Logout",
    class = "btn-danger",
    style = "color: white;",
    icon = icon("arrow-right-to-bracket")
  )

logInCromwellButton <-
  actionButton(
    inputId = "ownCrom",
    label = "DIY Cromwell",
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
