help_html <- helpText(
  HTML('<u>Need Help?:</u>
        <p>
            <br>
            <b>Email:</b> <a href="mailto:sachamber@fredhutch.org"><span class="badge bg-secondary">sachamber@fredhutch.org</span></a>
        </p>
        <br>
        <b>
            Bug? Feature request? <a href="https://github.com/FredHutch/shiny-cromwell/issues" , target="_blank"><span class="badge bg-secondary">Open a ticket</span></a>
        </b>
        <br><br>
        <p>
            <b>Questions</b> about this app, Cromwell or WDL? <a href="https://fhdata.slack.com/archives/CJFP1NYSZ"><span class="badge bg-secondary"><i class="fa-brands fa-slack"></i> FH-Data Slack</span></a> channel for #workflow-managers.
        </p>
        <p></p>
        <p>')
)

dropdown_user_name <- tags$li(
  class = "dropdown",
  style = "padding: 12px;",
  textOutput("userName")
)
dropdown_own_cromwell <- tags$li(
  class = "dropdown",
  style = "padding: 8px;",
  uiOutput("ownCromwell")
)
dropdown_loginout <- tags$li(
  class = "dropdown",
  style = "padding: 8px;",
  uiOutput("loggedInOut")
)
dropdown_help <- dropdownMenu(
  type = "notifications",
  badgeStatus = NULL,
  icon = icon("circle-question", "fa-solid fa-lg"),
  headerText = help_html
)
dropdown_src <- dropdownMenu(
  type = "notifications",
  badgeStatus = NULL,
  icon = icon("github", "fa-solid fa-lg"),
  headerText = helpText(htmlOutput("gitHtml"))
)

tooltip_style <- tags$style(
  HTML("
    .tooltip{
       font-size: 1.5rem;
    }
  ")
)

enter_to_click <- tags$script(src = "js/keyup.js")

table_footer <- function(timezone = TRUE, copy = TRUE) {
  tz <- if (timezone) "Dates are in Pacific time zone" else ""
  cp <- ""
  cp <- if (copy) {
    paste0("Click ",
      "<button class=\"btn btn-default btn-sm\"><i class=\"far fa-copy\"></i></button>",
      " to copy text")
  }
  sep <- if (sum(c(timezone, copy)) == 2) as.character(br()) else ""
  HTML(paste(tz, cp, sep = sep))
}
