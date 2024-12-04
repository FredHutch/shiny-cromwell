logInButton <-
  actionButton(
    inputId = "proofAuth",
    label = "PROOF Login",
    class = "btn-success btn-sm",
    style = "color: white;",
    icon = icon("truck-fast")
    
  )
logOutButton <-
  actionButton(
    inputId = "proofAuthLogout",
    label = "PROOF Logout",
    class = "btn-danger btn-sm",
    style = "color: white;",
    icon = icon("arrow-right-to-bracket")
  )

logInCromwellButton <-
  actionButton(
    inputId = "ownCrom",
    label = "DIY Cromwell",
    class = "btn-sm",
    style = "color: white;",
    icon = icon("plug-circle-xmark")
  )
logOutCromwellButton <-
  actionButton(
    inputId = "proofCromwellLogout",
    label = " Exit",
    class = "btn-danger btn-sm",
    style = "color: white;",
    icon = icon("arrow-right-to-bracket")
  )
