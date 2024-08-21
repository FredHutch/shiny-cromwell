welcome_servers_box <- box(
  id = "boxServers", title = "PROOF Server", width = 4, solidHeader = TRUE, status = "warning", icon = icon("truck-fast"),
  shiny::markdown("This tab allows you to:
  - Start or delete your PROOF server
  - Get metadata for your PROOF server"),
  align = "left"
)

tab_welcome <- nav_panel(title = "Welcome",
    card(
      class = "align-items-center",
      card_header(h1("PROOF")),
      card_body(
        class = "align-items-center",
        h3("Run ", a("WDL", href='https://openwdl.org/', target = "_blank"), "workflows with a range of HPC backends"),
        # actionLink("infoLink", "Information Link", class = "btn-info")
        shiny::includeMarkdown("about.md")
      )
    ),
    card(
      card_header(h2("Dashboard Tabs")),
      layout_column_wrap(
        height = 350,
        card(
          id = "boxValidate",
          class="border border-primary",
          card_header(h2(shiny::icon("stethoscope"), "Validate"), class="bg-primary"),
          shiny::markdown("- Validate a workflow you'd like to run"),
        ),
        card(
          id = "boxSubmit",
          class="border border-success",
          card_header(h2(shiny::icon("paper-plane"), "Submit jobs"), class="bg-success"),
          shiny::markdown("- Run a workflow")
        ),
        card(
          id = "boxTrack",
          class="border border-info",
          card_header(h2(shiny::icon("binoculars"), "Track Jobs"), class="bg-info"),
          shiny::markdown("- Query your server database for the jobs run the most recent days (your choice how far back to go)
            - See statuses of all your workflows
            - Look within a workflow at the individual calls, failures and call caching results
            - Download a list of the final workflow outputs for further processing")
        ),
        card(
          id = "boxTrouble",
          class="border border-danger",
          card_header(h2(shiny::icon("wrench"), "Troubleshoot"), class="bg-danger"),
          shiny::markdown("- Abort a workflow
            - Troubleshoot the workflow itself by looking at the entire raw json of workflow metadata (it's especially helpful for complex workflows)")
        ),
        card(
          id = "boxServers",
          class="border border-warning",
          card_header(h2(shiny::icon("truck-fast"), "Troubleshoot"), class="bg-warning"),
          shiny::markdown("- Start or delete your PROOF server
            - Get metadata for your PROOF server"),
        )
      )
    )
  )
