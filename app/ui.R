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

library(cookies)

source("ui_components.R")
source("tab-welcome.R")
source("tab-servers.R")
source("tab-validate.R")
source("tab-submission.R")
source("tab-tracking.R")
source("tab-workflow_details.R")

contact_github <- tags$a(shiny::icon("github"), "Open Ticket", href = "https://github.com/getwilds/proof/issues/new?template=Blank+issue", target = "_blank")
contact_email <- tags$a(shiny::icon("paper-plane"), "Email", href = "mailto:wilds@fredhutch.org", target = "_blank")

ui <- cookies::add_cookie_handlers(
  page_navbar(
    id = "proof",
    title = tags$span(
      tags$a(
        tags$img(
          src = "fred-hutch.png",
          width = "96px",
          height = "auto",
          class = "me-3",
          alt = "Fred Hutch logo"
        ),
        href = "https://www.fredhutch.org"
      ),
      ""
    ),
    bg = "#000000",
    underline = TRUE,
    header = tagList(google_analytics, enter_to_click),
    tab_welcome,
    nav_panel(title = "Server", tab_servers, tooltip_style),
    nav_panel(title = "Validate", tab_validate, shinyjs::useShinyjs()),
    nav_panel(title = "Submit", tab_submission, shinyjs::useShinyjs(),
      tags$style("
        .btn.btn-success:disabled {
          background-color: #d0d0d0;
          opacity: 1.0;
        }"
      ),
    ),
    nav_panel(title = "Track workflows", tab_tracking,
      rclipboard::rclipboardSetup(),
      tooltip_style
    ),
    nav_panel(title = "Workflow Details", tab_workflow_details,
      rclipboard::rclipboardSetup(),
      tags$head(
        tags$script(src = "https://cdn.jsdelivr.net/npm/mermaid@11.4.0/dist/mermaid.min.js")
      ),
      tooltip_style
    ),
    nav_panel(title = "Help",
      card(
        shiny::includeMarkdown("about.md"),
        card_body(
          h1("App details"),
          htmlOutput("gitHtml")
        )
      )
    ),
    nav_menu(
      title = "Contact",
      nav_item(contact_github),
      nav_item(contact_email)
    ),
    nav_spacer(),
    nav_item(
      uiOutput("userName")
    ),
    nav_item(
      uiOutput("ownCromwell")
    ),
    nav_item(
      uiOutput("loggedInOut")
    )
  )
)
