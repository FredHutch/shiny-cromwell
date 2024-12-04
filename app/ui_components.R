tooltip_style <- tags$style(
  HTML("
    .tooltip{
       font-size: 1.1rem;
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
