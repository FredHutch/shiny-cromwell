source("ui_components.R")
library(bsicons)

tab_tracking <- page_sidebar(
  fillable = FALSE,
  sidebar = sidebar(
    value_box(
      title = "Submitted",
      value = textOutput("submittedBoxValue"),
      showcase = bsicons::bs_icon("list"),
      height = "50px"
    ),
    value_box(
      title = "Succeeded",
      value = textOutput("successBoxValue"),
      showcase = bsicons::bs_icon("emoji-grin-fill"),
      height = "50px"
    ),
    value_box(
      title = "Failed",
      value = textOutput("failBoxValue"),
      showcase = bsicons::bs_icon("emoji-tear-fill"),
      height = "50px"
    ),
    value_box(
      title = "Running",
      value = textOutput("inprogressBoxValue"),
      showcase = icon("sync"),
      height = "50px"
    )
  ),
  card(
    id = "tracking",
    card_header(h3("Track your Workflows")),
    card_body(
      fillable = FALSE,
      p("Once you've submitted workflows, you can track the status of all the workflows you've submitted
        in the specified time range by clicking `Update View`.  If you use PROOF a lot,
        this and the filtering tools below can help you return only the workflows you're interested in monitoring,
        making tracking and the application itself much faster. "),

      numericInput("daysToShow", "Days of History to Display:",
        min = 1, max = 21, value = 1, step = 1, width = "35%"),

      textInput("workName", "Filter for workflows with name:",
        value = "",
        placeholder = "myCustomWorkflow",
        width = "35%"
      ),
      selectInput("workStatus",
        label = "Filter for Workflows with Status(es):",
        choices = c(
          "Submitted", "Running",
          "Succeeded", "Failed", "Aborting",
          "Aborted"
        ),
        multiple = TRUE,
        width = "35%"
      ),
      actionButton(
          inputId = "trackingUpdate",
          label = "Update View",
          icon = icon("refresh")
      )
    )
  ),
  card(
    card_header(h2("Workflow Timing")),
    plotOutput("workflowDuration")
  )
)


# tab_tracking <- layout_column_wrap(
#   width = 1/1,
#   fillable = FALSE,
#   card(
#     card_header(h3("Track your Workflows")),
#     card_body(
#       fillable = FALSE,
#       p("Once you've submitted workflows, you can track the status of all the workflows you've submitted
#         in the specified time range by clicking `Update View`.  If you use PROOF a lot,
#         this and the filtering tools below can help you return only the workflows you're interested in monitoring,
#         making tracking and the application itself much faster. "),

#       numericInput("daysToShow", "Days of History to Display:",
#         min = 1, max = 21, value = 1, step = 1, width = "35%"),

#       textInput("workName", "Filter for workflows with name:",
#         value = "",
#         placeholder = "myCustomWorkflow",
#         width = "35%"
#       ),
#       selectInput("workStatus",
#         label = "Filter for Workflows with Status(es):",
#         choices = c(
#           "Submitted", "Running",
#           "Succeeded", "Failed", "Aborting",
#           "Aborted"
#         ),
#         multiple = TRUE,
#         width = "35%"
#       ),
#       actionButton(
#           inputId = "trackingUpdate",
#           label = "Update View",
#           icon = icon("refresh")
#       )
#     )
#   ),
#   card(
#     layout_column_wrap(
#       value_box(
#         title = "Submitted",
#         value = textOutput("submittedBoxValue"),
#         showcase = bsicons::bs_icon("list"),
#         height = "50px"
#       ),
#       value_box(
#         title = "Succeeded",
#         value = textOutput("successBoxValue"),
#         showcase = bsicons::bs_icon("emoji-grin-fill"),
#         height = "50px"
#       ),
#       value_box(
#         title = "Failed",
#         value = textOutput("failBoxValue"),
#         showcase = bsicons::bs_icon("emoji-tear-fill"),
#         height = "50px"
#       ),
#       value_box(
#         title = "Running",
#         value = textOutput("inprogressBoxValue"),
#         showcase = icon("sync"),
#         height = "50px"
#       ),
#       height = "100px"
#     )
#   ),
#   card(
#     card_header(h2("Workflow Timing")),
#     plotOutput("workflowDuration")
#   )
# )