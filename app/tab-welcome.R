welcome_servers_box <- box(
  id = "boxServers", title = "PROOF Server", width = 4, solidHeader = TRUE, status = "warning", icon = icon("truck-fast"),
  shiny::markdown("This tab allows you to:
  - Start or delete your PROOF server
  - Get metadata for your PROOF server"),
  align = "left"
)

ablank <- function(...) {
  htmltools::a(..., target = "_blank")
}

tab_welcome <- nav_panel(
  title = "Welcome",
  card(
    class = "align-items-center",
    card_header(h1("PROOF")),
    card_body(
      class = "align-items-center",
      h3("Run ", ablank("WDL", href = "https://openwdl.org/"), "workflows on the Fred Hutch cluster"),
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
        tags$li(tags$u("Log in to PROOF with your Fred Hutch credentials")),
        tags$li(tags$u("Start your PROOF/Cromwell server")),
        tags$li("(optional)", tags$u(" Validate your WDL")),
        tags$li(tags$u("Submit your WDL"), " and 1-2 optional json input and parameter files"),
        tags$li(tags$u("Track your workflow", " to see how long it takes and if it succeeds or fails")),
        tags$li("Check", tags$u(" workflow details")),
        tags$li("Check the", tags$u(" troubleshooting page to dig into error messages if needed"))
      ),
      h4("To learn more about PROOF and WDL head over to the ",
        actionLink("linkToResourcesTab", "Resources page")
      )
    )
  )
)
