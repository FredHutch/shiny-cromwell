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
source("sidebar.R")

ui <- cookies::add_cookie_handlers(
  page_navbar(
    id = "proof",
    title = "PROOF",
    bg = "#0062cc",
    underline = TRUE,
    tab_welcome,
    nav_panel(title = "Resources",
      card(
        shiny::includeMarkdown("about2.md")
      )
    ),
    nav_panel(title = "Server", tab_servers),
    nav_panel(title = "Validate", tab_validate, shinyjs::useShinyjs()),
    nav_panel(title = "Submit", tab_submission, shinyjs::useShinyjs()),
    nav_panel(title = "Track workflows", tab_tracking, rclipboard::rclipboardSetup()),
    nav_panel(title = "Workflow Details", tab_workflow_details,
      tags$head(
        tags$script(src = "https://cdn.jsdelivr.net/npm/mermaid@10.9.1/dist/mermaid.min.js")
      )
    ),
    nav_panel(title = "Troubleshoot", tab_troublehsoot, shinyjs::useShinyjs()),
    # nav_panel(title = "Visualize", tab_details,
    #   tags$head(
    #     tags$script(src = "https://cdn.jsdelivr.net/npm/mermaid@10.9.1/dist/mermaid.min.js")
    #   )
    # ),
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
