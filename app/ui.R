# pak::pak("getwilds/shinyauthr@remote-api")
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(data.table)
library(DT)
library(tidyverse)
library(RColorBrewer)
library(rcromwell)
library(shinyBS)
library(shinyjs)
library(markdown)
library(shinyWidgets)
library(jsonlite)
library(lubridate)
library(shinyauthr)

cookie_expiry <- 7

ui <- dashboardPage(
  skin = "black",
  dashboardHeader(
    title = "Fred Hutch Cromwell Dashboard",
    tags$li(
      class = "dropdown",
      style = "padding: 8px;",
      shinyauthr::logoutUI("logout")
    ),
    # leftUi = tagList(
    #   dropdownBlock(
    #     id = "mydropdown",
    #     title = textOutput(outputId = "cromwellURI"),
    #     badgeStatus = NULL
    #   )
    # ),
    # leftUi = uiOutput("cromwellinfo"),
    dropdownMenu(
      type = "notifications",
      badgeStatus = NULL,
      icon = icon("circle-question", "fa-solid"),
      headerText =
        helpText(
          HTML('<u>Need Help?:</u> <p>
                <br>
                <b>Email:</b>. <a href="mailto:sachamber@fredhutch.org">sachamber@fredhutch.org</a></p>
                <b>Open a ticket: <a href="https://github.com/FredHutch/shiny-cromwell/issues", target="_blank">here</a></b><P></P><P>'))
    )
  ),
  dashboardSidebar(
    div(textOutput("welcome"), style = "padding: 20px"),
    uiOutput("loggedInSidebar")
  ),
  dashboardBody(
    shinyjs::useShinyjs(),
    shinyauthr::loginUI("login", cookie_expiry = cookie_expiry),
    uiOutput("loggedInBody")
  )
)
