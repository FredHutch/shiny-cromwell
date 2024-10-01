ablank <- function(...) {
  htmltools::a(..., target = "_blank")
}

tab_welcome <- nav_panel(
  title = "Welcome",
  card(
    card_body(
      div(
        h1("PROOF"),
        h4("Run WDL workflows on the Fred Hutch cluster")
      ),
      p("This is a",
        ablank("Shiny", href="https://shiny.posit.co/"),
        "app being developed by the",
        ablank("Fred Hutch Data Science Lab, DaSL", href="https://hutchdatascience.org/"),
        "that simplifies user interactions with a",
        ablank("Cromwell", href="https://cromwell.readthedocs.io/en/stable/"),
        "server, an open source",
        ablank("WDL", href="https://openwdl.org/"),
        "workflow engine that can be used with the Fred Hutch scientific computing cluster. We are developing this
        application alongside Fred Hutch oriented infrastructure with the intention to develop
        open source resources to enable others to do the same."),
      h3("To get started"),
      tags$ol(
        tags$li(
          actionLink("proofAuth", "Log in to PROOF"),
          "with your Fred Hutch credentials"
        ),
        tags$li(actionLink("linkToServerTab", "Start your PROOF/Cromwell server")),
        tags$li("(optional)", actionLink("linkToValidateTab", "Validate your WDL")),
        tags$li(
          actionLink("linkToSubmitTab", "Submit your WDL"),
          "and 1-2 optional json input and parameter files"
        ),
        tags$li(
          actionLink("linkTrackingTab", "Track your workflow"),
          "to see how long it takes and if it succeeds or fails"
        ),
        tags$li("Check",
          actionLink("linkToWorkflowDetailsTab", "workflow details")
        ),
        tags$li("Check the",
          actionLink("linkToTroubleshootingTab", "troubleshooting page"),
          " to dig into error messages if needed"
        )
      ),
      h4("To learn more about PROOF and WDL head over to the ",
        actionLink("linkToResourcesTab", "Resources page")
      )
    )
  )
)
