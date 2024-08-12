library(bslib)

library(shiny)
library(shinyBS)
library(shinyjs)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)

library(DT)
library(jsonlite)

library(RColorBrewer)
library(lubridate)

library(rclipboard)

source("ui_components.R")
source("tab-welcome.R")
source("tab-servers.R")
source("tab-validate.R")
source("tab-submission.R")
source("tab-tracking.R")
source("tab-troubleshoot.R")
source("sidebar.R")

# link_shiny <- tags$a(shiny::icon("github"), "Shiny", href = "https://github.com/rstudio/shiny", target = "_blank")
# link_posit <- tags$a(shiny::icon("r-project"), "Posit", href = "https://posit.co", target = "_blank")

ui <- page_navbar(
  title = "PROOF",
  bg = "#0062cc",
  underline = TRUE,
  nav_panel(title = "Welcome",
    card(
      card_header(h2("What is this app?")),
      shiny::includeMarkdown("about.md")
    ),
    card(
      card_header(h2("Dashboard Tabs")),
      layout_column_wrap(
        height = 200,
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
  ),
  nav_panel(title = "Resources",
    card(
      shiny::includeMarkdown("about2.md")
    )
  ),
  nav_panel(title = "PROOF", tab_servers),
  nav_panel(title = "Validate", tab_validate),
  nav_panel(title = "Submit", tab_submission),
  nav_panel(title = "Track", p("Third tab content")),
  nav_panel(title = "Troubleshoot", tab_troublehsoot),
  nav_spacer(),
  nav_menu(
    # nav_item(
      tags$li(
        class = "dropdown messages-menu",
        # a(href = "#", class = "dropdown-toggle", `data-toggle` = "dropdown", "circle-question", "circle-question icon"),
        tags$ul(
          class = "dropdown-menu",
          tags$li(class = "header", NULL),
          tags$li(tags$ul(class = "menu", help_html))
        )
      ),
    # ),
    icon = icon("circle-question", "fa-solid fa-lg")
  )
  # nav_menu(
  #   title = "Links",
  #   align = "right",
  #   nav_item(link_shiny),
  #   nav_item(link_posit)
  # )
)

# ui <- dashboardPage(
#   skin = "black",
#   dashboardHeader(
#     title = tagList(
#       span(class = "logo-lg", h4(HTML("Fred Hutch<br> PROOF Dashboard"))),
#       img(src = "fred-hutch.svg")
#     ),
#     dropdown_user_name,
#     dropdown_own_cromwell,
#     dropdown_loginout,
#     dropdown_help,
#     dropdown_src
#   ),
#   dashboardSidebar(
#     sidebarMenuOutput("uiSideBar")
#   ),
#   dashboardBody(
#     tags$head(tags$title("PROOF")),
#     tags$script("document.title = 'PROOF';"),
#     shinyjs::useShinyjs(),
#     rclipboard::rclipboardSetup(),
#     enter_to_click,
#     tooltip_style,
#     google_analytics,
#     tabItems(
#       tab_welcome,
#       tab_servers,
#       tab_validate,
#       tab_submission,
#       tab_tracking,
#       tab_troublehsoot
#     )
#   )
# )
