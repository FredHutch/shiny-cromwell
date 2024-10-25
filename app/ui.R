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
source("tab-troubleshoot.R")
source("tab-details.R")

ui <- cookies::add_cookie_handlers(
  page_navbar(
    id = "proof",
    title = tags$span(
      tags$img(
        src = "fred-hutch.png",
        width = "96px",
        height = "auto",
        class = "me-3",
        alt = "Fred Hutch logo"
      ),
      ""
    ),
    bg = "#000000",
    underline = TRUE,
    tab_welcome,
    nav_panel(title = "Server", tab_servers),
    nav_panel(title = "Validate", tab_validate, shinyjs::useShinyjs()),
    nav_panel(title = "Submit", tab_submission, shinyjs::useShinyjs(),
      tags$style("
        .btn.btn-success:disabled {
          background-color: #d0d0d0;
          opacity: 1.0;
        }"
      ),
    ),
    nav_panel(title = "Track workflows", tab_tracking, rclipboard::rclipboardSetup()),
    nav_panel(title = "Workflow Details", tab_workflow_details,
      tags$head(
        tags$script(src = "https://cdn.jsdelivr.net/npm/mermaid@10.9.1/dist/mermaid.min.js")
      )
    ),
    nav_panel(title = "Troubleshoot", tab_troublehsoot, shinyjs::useShinyjs()),
    nav_panel(title = "Help",
      card(
        shiny::includeMarkdown("about.md"),
        card_body(
          h1("App details"),
          htmlOutput("gitHtml")
        )
      )
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
    # nav_menu(
    #   NULL,
    #   icon = icon("circle-question", "fa-solid fa-lg"),
    #   align = "right",
    #   nav_item(help_html)
    # ),
    # nav_menu(
    #   NULL,
    #   icon = icon("github", "fa-solid fa-lg"),
    #   align = "right",
    #   nav_item(helpText(htmlOutput("gitHtml")))
    # )
  )
)
