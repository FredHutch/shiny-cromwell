menu_item_welcome <- menuItem("Welcome",
  tabName = "welcome", icon = icon("book-open"),
  badgeLabel = "info", badgeColor = "black",
  selected = TRUE, startExpanded = TRUE
)
menu_item_servers <- menuItem("PROOF Server",
  tabName = "cromwell", icon = icon("truck-fast"),
  badgeLabel = "proof", badgeColor = "yellow"
)
menu_item_validate <- menuItem("Validate",
  tabName = "validate", icon = icon("stethoscope"),
  badgeLabel = "check", badgeColor = "blue"
)
menu_item_submit <- menuItem("Submit Jobs",
  tabName = "submission", icon = icon("paper-plane"),
  badgeLabel = "compute", badgeColor = "green"
)
menu_item_track <- menuItem("Track Jobs",
  tabName = "tracking", icon = icon("binoculars"),
  badgeLabel = "monitor", badgeColor = "aqua"
)
menu_item_trouble <- menuItem("Troubleshoot",
  tabName = "troubleshoot", icon = icon("wrench"),
  badgeLabel = "troubleshoot", badgeColor = "red"
)
menu_item_viewer <- menuItem("Viewer",
  tabName = "viewer", icon = icon("wrench"),
  badgeLabel = "viewer", badgeColor = "purple"
)
menu_item_wdl <- menuItem("WDL",
  tabName = "wdl", icon = icon("wrench"),
  badgeLabel = "wdl", badgeColor = "light-blue"
)

proofSidebar <- function() {
  sidebarMenu(
    id = "tabs",
    menu_item_welcome,
    menu_item_servers,
    menu_item_validate,
    menu_item_submit,
    menu_item_track,
    menu_item_trouble,
    menu_item_viewer,
    menu_item_wdl
  )
}

nonProofSidebar <- function() {
  sidebarMenu(
    id = "tabs",
    menu_item_welcome,
    menu_item_validate,
    menu_item_submit,
    menu_item_track,
    menu_item_trouble,
    menu_item_viewer,
    menu_item_wdl
  )
}
