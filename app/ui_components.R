help_html <- helpText(
  HTML('<u>Need Help?</u>
        <p>
            <br>
            <b>Problem? Bug?</b> <a href="https://github.com/getwilds/proof/issues/new" target="_blank"><span class="badge bg-secondary">Open an issue</span></a>
        </p>
        <br>
        <p>
            <b>Discussion/Questions</b><br>
            <a href="https://fhdata.slack.com/archives/CJFP1NYSZ" target="_blank"><span class="badge bg-secondary"><i class="fa-brands fa-slack"></i> Slack</span></a> #workflow-managers channel
            <br>
            Email <a href="mailto:wilds@fredhutch.org" target="_blank"><span class="badge bg-secondary">wilds@fredhutch.org</span></a>
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

google_analytics <- tags$head(includeHTML(path = "www/google-analytics.html"))

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
