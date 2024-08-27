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

ui <- page_navbar(
  title = "PROOF",
  bg = "#0062cc",
  underline = TRUE,
  tab_welcome,
  nav_panel(title = "Resources",
    card(
      shiny::includeMarkdown("about2.md")
    )
  ),
  nav_panel(title = "PROOF", tab_servers),
  nav_panel(title = "Validate", tab_validate),
  nav_panel(title = "Submit", tab_submission),
  nav_panel(title = "Track", tab_tracking),
  nav_panel(title = "Troubleshoot", tab_troublehsoot),
  nav_spacer(),
  nav_item(
    textOutput("userName")
  ),
  nav_item(
    uiOutput("ownCromwell")
  ),
  nav_item(
    uiOutput("loggedInOut")
  ),
  nav_menu(
    NULL,
    icon = icon("circle-question", "fa-solid fa-lg"),
    align = "right",
    nav_item(help_html)
  ),
  nav_menu(
    NULL,
    icon = icon("github", "fa-solid fa-lg"),
    align = "right",
    nav_item(helpText(htmlOutput("gitHtml")))
  )
)