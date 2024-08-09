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
source("tab-viewer.R")
source("sidebar.R")

ui <- dashboardPage(
  skin = "black",
  dashboardHeader(
    title = tagList(
      span(class = "logo-lg", h4(HTML("Fred Hutch<br> PROOF Dashboard"))),
      img(src = "fred-hutch.svg")
    ),
    dropdown_user_name,
    dropdown_own_cromwell,
    dropdown_loginout,
    dropdown_help,
    dropdown_src
  ),
  dashboardSidebar(
    sidebarMenuOutput("uiSideBar")
  ),
  dashboardBody(
    tags$head(tags$title("PROOF")),
    tags$script("document.title = 'PROOF';"),
    shinyjs::useShinyjs(),
    rclipboard::rclipboardSetup(),
    enter_to_click,
    tooltip_style,
    google_analytics,
    tabItems(
      tab_welcome,
      tab_servers,
      tab_validate,
      tab_submission,
      tab_tracking,
      tab_troublehsoot,
      tab_viewer
    )
  )
)
