# pak::pak("getwilds/shinyauthr@remote-api")
library(shiny)
library(shinydashboard)
library(data.table)
library(uuid)
library(DT)
library(tidyverse)
library(RColorBrewer)
library(rcromwell)
library(glue)
library(shinyBS)
library(shinyjs)
library(shinyFeedback)
library(markdown)
library(shinyWidgets)
library(jsonlite)
library(lubridate)
library(proofr)
library(httr)
library(shinyauthr)

library(dplyr)
library(RSQLite)
library(DBI)

focusID <- 1

# FIXME: maybe remove later, was running into some timeouts during testing
proof_timeout(sec = 10)

# sanitize errors - note that some actual errors will still happen
options(shiny.sanitize.errors = TRUE)

cromwell_url_display <- function() {
  paste0("Cromwell URL: ", Sys.getenv("CROMWELLURL", "No Cromwell Server found"))
}

proof_wait_for_up <- function() {
  not_up <- TRUE
  while (not_up) {
    cromwell_url <- proof_status()$cromwellUrl
    if (!is.null(cromwell_url)) not_up <- FALSE
  }
  cromwell_url
}

proof_wait_for_down <- function() {
  up <- TRUE
  while (up) {
    cromwell_url <- proof_status()$cromwellUrl
    if (is.null(cromwell_url)) up <- FALSE
  }
}

proof_loggedin <- function() {
  !identical(Sys.getenv("PROOF_TOKEN"), "")
}

cromwell_version_safe <- purrr::safely(cromwell_version, FALSE)

proof_loggedin_serverup <- function() {
  proof_loggedin() && rlang::is_list(cromwell_version_safe()$result)
}

proof_serverup <- function() {
  rlang::is_list(cromwell_version_safe()$result)
}

stop_safe <- function(fun, message) if (!fun()) stop(safeError(message))
stop_safe_loggedin_serverup <- function() {
  stop_safe(proof_loggedin, "Not logged in! Please log in")
  stop_safe(proof_serverup, "Your Cromwell server is not up!")
}


loginModal <- function(failed = FALSE, error = "Invalid username or password") {
  modalDialog(
    textInput("username", "Username",
      placeholder = "HutchNet username"
    ),
    passwordInput("password", "Password",
      placeholder = "HutchNet password"
    ),
    if (failed) {
      div(tags$b(error, style = "color: red;"))
    },
    footer = tagList(
      modalButton("Cancel"),
      actionButton("submit", "Submit")
    )
  )
}

cromwellStartModal <- function() {
  modalDialog(
    title = "Start your PROOF Cromwell server",
    br(),
    textInput(
      inputId = "slurmAccount",
      label = div(HTML("Slurm account (optional)")),
      value = NULL
    ),
    footer = tagList(
      modalButton("Cancel"),
      shinyFeedback::loadingButton(
        inputId = "beginCromwell",
        label = "Start",
        class = "btn btn-primary"
      )
    )
  )
}

verifyCromwellDeleteModal <- function(failed = FALSE, error = "Woops, an error! Contact DaSL") {
  modalDialog(
    title = "Delete your PROOF Cromwell server",
    "Permanently delete your PROOF Cromwell server. Although you can't undo this action, you can start up another one anytime!",
    br(),
    br(),
    textInput(
      inputId = "stopCromwell",
      label = div(HTML("To confirm deletion, type <em>delete me</em> into the field."))
    ),
    if (failed) {
      div(tags$b(error, style = "color: red;"))
    },
    footer = tagList(
      modalButton("Cancel"),
      shinyjs::disabled(
        shinyFeedback::loadingButton(
          inputId = "deleteCromwell",
          label = "Delete",
          class = "btn btn-warning"
        )
      )
    )
  )
}

validate_workflowid <- function(x) {
  shiny::validate(
    shiny::need(
      uuid::UUIDvalidate(x),
      "That doesn't look like a workflow ID; check your ID"
    )
  )
}

my.cols <- brewer.pal(6, "RdYlBu")

cookie_expiry <- 7

get_sessions_from_db <- function(conn = db, expiry = cookie_expiry) {
  print(dbReadTable(conn, "sessions"))
  dbReadTable(conn, "sessions") %>%
    mutate(login_time = ymd_hms(login_time)) %>%
    as_tibble() %>%
    filter(login_time > now() - days(expiry))
}
add_session_to_db <- function(user, sessionid, conn = db) {
  tibble(user = user, sessionid = sessionid, login_time = as.character(now())) %>%
    dbWriteTable(conn, "sessions", ., append = TRUE)
}
db <- dbConnect(SQLite(), ":memory:")
dbCreateTable(db, "sessions", c(user = "TEXT", sessionid = "TEXT", login_time = "TEXT"))
dbCreateTable(db, "users", c(user = "TEXT", password = "TEXT"))

loggedInSidebar <- sidebarMenu(
      menuItem("Welcome",
        tabName = "welcome", icon = icon("book-open"),
        badgeLabel = "info", badgeColor = "green",
        selected = TRUE
      ),
      # menuItem(
      #   tabName = "auth",
      #   startExpanded = TRUE,
      #   actionButton(
      #     inputId = "proofAuth",
      #     label = "PROOF Login",
      #     icon = icon("lock")
      #   )
      # ),
      # menuItem(
      #   tabName = "serverConf",
      #   startExpanded = TRUE,
      #   actionButton(
      #     inputId = "getStarted",
      #     label = "Connect to Server",
      #     icon = icon("plug")
      #   )
      # ),
      menuItem("Cromwell Servers",
        tabName = "cromwell", icon = icon("server"),
        badgeLabel = "cromwell", badgeColor = "yellow"
      ),
      menuItem("Validate",
        tabName = "validate", icon = icon("stethoscope"),
        badgeLabel = "check", badgeColor = "orange"
      ),
      menuItem("Submit Jobs",
        tabName = "submission", icon = icon("paper-plane"),
        badgeLabel = "compute", badgeColor = "light-blue"
      ),
      menuItem("Track Jobs",
        tabName = "tracking", icon = icon("binoculars"),
        badgeLabel = "monitor", badgeColor = "purple"
      ),
      menuItem("Troubleshoot",
        tabName = "troubleshoot", icon = icon("wrench"),
        badgeLabel = "troubleshoot", badgeColor = "teal"
      )
    )

loggedInBody <- tabItems(
      tabItem(
        tabName = "welcome",
        fluidRow(
          align = "center",
          box(
            title = "Find Cromwell and WDL Resources at Fred Hutch's GitHub",
            actionButton(
              inputId = "githubLink", label = "What resources are available?",
              icon = icon("retweet"),
              onclick = "window.open('https://github.com/FredHutch?utf8=%E2%9C%93&q=wdl+OR+cromwell&type=&language=', '_blank')"
            )
          ),
          box(
            title = "Learn about Cromwell and WDL at SciWIki",
            actionButton(
              inputId = "sciwikiLink", label = "I want to go read!",
              icon = icon("book-open"),
              onclick = "window.open('https://sciwiki.fredhutch.org/compdemos/Cromwell/', '_blank')"
            )
          )
        ),
        fluidRow(
          align = "center",
          column(
            width = 11, align = "left",
            includeMarkdown("about.md")
          )
        )
      ),
      tabItem(
        tabName = "cromwell",
        fluidRow(h2("Manage Your PROOF Based Cromwell Server"), align = "center"),
        fluidRow(
          align = "left",
          box(
            width = 12, solidHeader = FALSE, status = "info",
            collapsed = FALSE,
            title = "Start/delete your Cromwell Server",
            p(strong("Note"), " stopping your server requires making sure you mean it :)"),
            br(),
            br(),
            actionButton(
              inputId = "cromwellStart",
              label = "Start",
              icon = icon("play"),
              class = "btn-primary btn-lg "
            ),
            actionButton(
              inputId = "cromwellDelete",
              label = "Delete",
              icon = icon("stop"),
              class = "btn-warning btn-lg"
            )
          )
        ),
        fluidRow(
          align = "left",
          box(
            width = 12, solidHeader = FALSE, status = "info",
            collapsed = FALSE,
            title = "Status",
            p("Cromwell server details will show below when your server is running"),
            actionButton(
              inputId = "cromwellStatus",
              label = "Update Status",
              icon = icon("recycle"),
              class = "btn-info"
            ),
            br(),
            br(),
            shinyBS::bsAlert("alert_loggedin"),
            shinyBS::bsAlert("alert_server_status"),
            htmlOutput("proofStatusJobStatus"),
            htmlOutput("proofStatusUrlStr"),
            htmlOutput("proofStatusWorkflowLogDir"),
            htmlOutput("proofStatusScratchDir"),
            htmlOutput("proofStatusSlurmJobId"),
            htmlOutput("proofStatusCromwellDir"),
            htmlOutput("proofStatusServerLogDir"),
            htmlOutput("proofStatusSingularityCacheDir"),
            htmlOutput("proofStatusServerTime"),
            htmlOutput("proofStatusUseAWS"),
            htmlOutput("proofStatusSlurmJobAccount")
          ),
        ),
      ),
      tabItem(
        tabName = "validate",
        fluidRow(h2("Validate a Workflow"), align = "center"),
        fluidRow(
          align = "left",
          ## Validate a Workflow
          box(
            width = 12, solidHeader = FALSE, status = "info",
            collapsible = TRUE, collapsed = FALSE,
            title = "Validate a Workflow",
            p("This tool will check to see if the WDL (and it's input JSON if you choose to upload it)
                                                are both in the correct format required and if not will give you some
                                                hints as to what might be wrong.  If your workflow does not validate, the feedback often hints
                                                at a problem just below where your error actually is. "),
            fileInput(
              inputId = "validatewdlFile", "Upload WDL File (required):",
              accept = ".wdl"
            ),
            fileInput(
              inputId = "validateinputFile", "Upload Consolidated Input JSON (optional):",
              accept = ".json"
            ),
            actionButton(
              inputId = "validateWorkflow",
              label = "Validate Workflow",
              class = "btn-info"
              # icon = icon("question-circle")
            ),
            br(),
            br(),
            actionButton('resetValidate', 'Reset'),
            verbatimTextOutput(outputId = "validationResult")
          )
        )
      ),
      tabItem(
        tabName = "submission",
        fluidRow(h2("Run Workflows on Cromwell"), align = "center"),
        fluidRow(
          box(
            width = 12, solidHeader = FALSE, status = "success",
            collapsible = TRUE, collapsed = FALSE,
            title = "Submit a Workflow",
            p("Here you can submit your workflow to your Cromwell server for execution.  Only a WDL is required. Up to two different input JSONs
                                                 can be uploaded (if variables are specified in both, the second input's variable value will overwrite the first). Workflow options
                                                 can be provided if desired.  Workflow labels are user-defined values you'd like to use to describe your workflows for your own
                                                 future reference. "),
            column(
              width = 6,
              fileInput(
                inputId = "wdlFile", "Upload WDL (required):",
                accept = ".wdl"
              ),
              fileInput(
                inputId = "inputJSON", "Upload First Input JSON (optional):",
                accept = ".json"
              ),
              fileInput(
                inputId = "input2JSON", "Upload Second Input JSON (optional):",
                accept = ".json"
              ),
              actionButton(
                inputId = "submitWorkflow",
                label = "Submit Workflow",
                icon = icon("paper-plane"),
                class = "btn-info"
              ),
              verbatimTextOutput(outputId = "submissionResult"),
              br(),
              actionButton('resetSubmission', 'Reset')
            ),
            column(
              width = 6,
              fileInput(
                inputId = "workOptions", "Upload Workflow Options JSON (optional):",
                accept = ".json"
              ),
              textInput(
                inputId = "labelValue", "Workflow Label (optional)",
                value = "",
                placeholder = "e.g., First Try"
              ),
              textInput(
                inputId = "seclabelValue", "Secondary Workflow Label (optional)",
                value = "",
                placeholder = "e.g., Cohort 2"
              )
            )
          )
        )
      ),
      tabItem(
        tabName = "tracking",
        fluidRow(h2("Cromwell Workflow Tracking"), align = "center"),
        fluidRow(
          box(
            width = 6,
            numericInput("daysToShow", "Days of History to Display:",
              min = 1, max = 21, value = 1, step = 1
            ),
            actionButton(
              inputId = "trackingUpdate",
              label = "Update View",
              icon = icon("refresh")
            )
          ),
          box(
            width = 6,
            textInput("workName", "Filter for workflows with name:",
              value = "",
              placeholder = "myCustomWorkflow"
            ),
            selectInput("workStatus",
              label = "Filter for Workflows with Status(es):",
              choices = c(
                "Submitted", "Running",
                "Succeeded", "Failed", "Aborting",
                "Aborted"
              ), multiple = TRUE
            )
          )
        ),
        fluidRow(
          box(
            width = 12,
            infoBoxOutput("submittedBox", width = 6),
            infoBoxOutput("inprogressBox", width = 6),
            infoBoxOutput("successBox", width = 6),
            infoBoxOutput("failBox", width = 6)
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Workflow Timing",
            collapsible = TRUE, solidHeader = TRUE,
            plotOutput("workflowDuration")
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Workflows Run",
            collapsible = TRUE, solidHeader = TRUE,
            DTOutput("joblistCromwell")
          )
        ),
        fluidRow(h3("Workflow Specific Job Information"),
          align = "center",
          p("Select a row in the above table for a specific workflow id in order to populate the tables below.  "),
          valueBoxOutput("pendingBatch", width = 3),
          infoBoxOutput("runningBatch", width = 3),
          infoBoxOutput("succeededBatch", width = 3),
          infoBoxOutput("failedBatch", width = 3)
        ),
        fluidRow(
          box(
            width = 12,
            title = "Workflow Description",
            DTOutput("workflowDescribe")
          )
        ),
        fluidRow(
          box(
            width = 6,
            title = "Workflow Options",
            DTOutput("workflowOpt")
          ),
          box(
            width = 6,
            title = "Workflow Inputs",
            DTOutput("workflowInp")
          )
        ),
        fluidRow(
          align = "center",
          box(
            width = 12,
            title = "Workflow Call Duration",
            collapsible = TRUE, solidHeader = TRUE,
            plotOutput("workflowTiming")
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Job List",
            collapsible = TRUE, solidHeader = TRUE, collapsed = FALSE,
            downloadButton("downloadJobs", "Download Workflow Jobs Data"),
            DTOutput("tasklistBatch")
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Job Failures",
            p("Specific information for jobs with a status of 'Failed', only available upon request."),
            collapsible = TRUE, solidHeader = TRUE, collapsed = FALSE,
            actionButton(
              inputId = "getFailedData",
              label = "Get/Refresh Failed Job Metadata",
              icon("refresh")
            ),
            downloadButton("downloadFails", "Download Call Failure Data"),
            DTOutput("failurelistBatch")
          )
        ),
        fluidRow(
          align = "center",
          infoBoxOutput("cacheHits", width = 6),
          infoBoxOutput("cacheMisses", width = 6)
        ),
        fluidRow(
          box(
            width = 12,
            title = "Call Caching ",
            p("Only available upon request.  Note: this can be slow for very complex workflows.  "),
            collapsible = TRUE, solidHeader = TRUE, collapsed = FALSE,
            actionButton(
              inputId = "getCacheData",
              label = "Get/Refresh Call Caching Metadata",
              icon("refresh")
            ),
            downloadButton("downloadCache", "Download Call Caching Data"),
            DTOutput("cachingListBatch")
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Get Workflow Outputs",
            p("The specific outputs to the entire workflow itself are listed here only upon request and only if they are all available. "),
            collapsible = TRUE, solidHeader = TRUE, collapsed = FALSE,
            actionButton(
              inputId = "getOutputData",
              label = "Get/Refresh Workflow Output Metadata",
              icon("refresh")
            ),
            downloadButton("downloadOutputs", "Download Workflow Output Data"),
            DTOutput("outputslistBatch")
          )
        )
      ),
      tabItem(
        tabName = "troubleshoot",
        fluidRow(
          align = "left",
          box(
            title = "Abort a Workflow",
            p("Aborting a workflow cannot be undone and can take some time to fully stop all jobs submitted in complex or highly parallelized workflows."),
            collapsible = TRUE, collapsed = FALSE,
            width = 12, solidHeader = FALSE, status = "danger",
            textInput("abortWorkflowID", "Workflow id to abort:",
              value = "",
              placeholder = "577b9aa4-b26b-4fd6-9f17-7fb33780bbd0"
            ),
            actionButton(
              inputId = "abortWorkflow",
              label = "Abort Workflow",
              icon = icon("thumbs-down")
            ),
            verbatimTextOutput(outputId = "abortResult")
          )
        ),
        fluidRow(
          align = "left",
          ## Troubleshoot a workflow via Glob
          box(
            width = 12, solidHeader = FALSE, status = "info",
            collapsible = TRUE, collapsed = FALSE,
            title = "Troubleshoot a Workflow",
            p("When a workflow fails but no jobs were started, or there appears to be no clear reason for a workflow to have failed, this tool can provide you the entire set of workflow metadata Cromwell has for your workflow in it's raw and unprocessed (json) form. For complex workflows, this can be rather large (and ugly!)."),
            textInput("troubleWorkflowID", "Cromwell workflow id to get metadata for:",
              value = "",
              placeholder = "577b9aa4-b26b-4fd6-9f17-7fb33780bbd0"
            ),
            actionButton(
              inputId = "troubleWorkflow",
              label = "Get Complete Workflow Metadata",
              icon = icon("question-circle")
            ),
            verbatimTextOutput(outputId = "troubleResult")
          )
        )
      )
    )

server <- function(input, output, session) {
  # handling login/logout

  # call the logout module with reactive trigger to hide/show
  logout_init <- shinyauthr::logoutServer(
    id = "logout",
    active = reactive(credentials()$user_auth)
  )

  credentials <- shinyauthr::loginServer(
    id = "login",
    user_col = user,
    pwd_col = password,
    cookie_logins = TRUE,
    sessionid_col = sessionid,
    cookie_getter = get_sessions_from_db,
    cookie_setter = add_session_to_db,
    log_out = reactive(logout_init()),
    sqlconn = db
  )

  output$loggedInSidebar <- renderUI({
    req(credentials()$user_auth)

    loggedInSidebar
  })

  output$loggedInBody <- renderUI({
    req(credentials()$user_auth)

    # remove the login tab
    removeTab("tabs", "login")
    # add sidebar
    # appendTab("tabs", loggedInSidebar)
    # add body
    # appendTab("tabs", loggedInBody)
    loggedInBody
  })




  # the app itself
  observeEvent(input$proofAuth, {
    showModal(loginModal())
  })

  observeEvent(input$submit, {
    if (!is.null(input$username) && !is.null(input$password)) {
      try_auth <- tryCatch(
        proof_authenticate(input$username, input$password),
        error = function(e) e
      )
      if (rlang::is_error(try_auth)) {
        showModal(loginModal(failed = TRUE, error = try_auth$message))
      } else {
        cromwell_up <- tryCatch(proof_status()$jobStatus, error = function(e) e)
        print(glue("cromwell_up {cromwell_up}"))
        if (!rlang::is_error(cromwell_up)) {
          if (!is.null(cromwell_up)) {
            cromwell_config(proof_wait_for_up(), verbose = FALSE)
          }
        }
        removeModal()
      }
    } else {
      showModal(loginModal(failed = TRUE))
    }
  })

  # update the icon in the PROOF Login button
  # (FIXME: ideally we change the color of the button too but not sure how to do that yet)
  observeEvent(proof_loggedin_serverup(), {
    updateActionButton(session, "proofAuth", icon = icon("unlock"))
  })

  # observeEvent(input$getStarted, {
  #   inputSweetAlert(
  #     session = session, inputId = "cromwellURL", input = "text",
  #     title = "What's your server node name and port?",
  #     inputPlaceholder = "gizmot32:8000",
  #     btn_labels = "Submit"
  #   )
  # })

  # theBackends <- eventReactive(input$cromwellURL,
  #   {
  #     cromwell_config(paste0("https://", input$cromwellURL), verbose = FALSE)
  #     shiny::validate(
  #       shiny::need(
  #         !proof_status()$canJobStart,
  #         "Your Cromwell server is not running. Go to  the Cromwell servers tab and click Start"
  #       )
  #     )
  #     try(cromwell_backends()$supportedBackends, silent = TRUE)
  #   },
  #   ignoreNULL = TRUE
  # )

  # observeEvent(input$cromwellURL, {
  #   if (inherits(theBackends(), "try-error")) {
  #     sendSweetAlert(
  #       session = session,
  #       title = "Whoops, that didn't work! If your server has only been running for <1 min, wait and try to reconnect again. ",
  #       text = print(theBackends()),
  #       type = "error"
  #     )
  #   } else {
  #     sendSweetAlert(
  #       session = session,
  #       title = "Server address valid.",
  #       text = paste0(
  #         "Available backends: ",
  #         paste0(theBackends(), collapse = ", ")
  #       ),
  #       type = "success"
  #     )
  #   }
  # })

  # output$connectionResult <- renderText(
  #   {
  #     if (inherits(theBackends(), "try-error")) {
  #       print(theBackends())
  #     } else {
  #       paste0(
  #         "Server address valid! \n Available backends: ",
  #         paste0(theBackends(), collapse = ", ")
  #       )
  #     }
  #   },
  #   quoted = FALSE
  # )

  ###### Cromwell servers tab ######
  ## Alert that need to login first
  observe({
    if (!proof_loggedin()) {
      shinyBS::createAlert(session,
        "alert_loggedin",
        title = "Heads up",
        content = HTML("You aren't logged in. Click the <strong>Proof Login</strong> button to the left"),
        style = "warning",
        append = FALSE
      )
    }
  })

  # Hide or show start and stop buttons
  observe({
    if (!proof_loggedin()) shinyjs::disable(id = "cromwellDelete")
    if (!proof_serverup()) shinyjs::disable(id = "cromwellDelete")
    if (proof_loggedin_serverup()) {
      if (proof_status()$canJobStart) {
        shinyjs::disable(id = "cromwellDelete")
      } else {
        shinyjs::disable(id = "cromwellStart")
      }
    }
  })

  # Start button handling
  observeEvent(input$cromwellStart, {
    showModal(cromwellStartModal())
  })

  observeEvent(input$beginCromwell, {
    if (proof_loggedin()) {
      # fail out early if already running
      if (!proof_status()$canJobStart) {
        stop(safeError("Your Cromwell server is already running"))
      }

      # start cromwell server
      proof_start(slurm_account = input$slurmAccount)

      # set cromwell server url
      cromwell_config(proof_wait_for_up(), verbose = FALSE)
      shiny::validate(
        shiny::need(
          !proof_status()$canJobStart,
          "Your Cromwell server is not running. Go to  the Cromwell servers tab and click Start"
        )
      )

      # reset loading spinner
      print("resetting loading spinner")
      shinyFeedback::resetLoadingButton("beginCromwell")

      print("running removeModal() for Start")
      removeModal()
      shinyjs::enable(id = "cromwellDelete")
      shinyjs::disable(id = "cromwellStart")
    }
  })

  # output$cromwellURI <- renderText({
  #   cromwell_url_display()
  # })

  output$cromwellinfo <- renderUI({
    req(credentials()$user_auth)

    tagList(
      dropdownBlock(
        id = "mydropdown",
        title = textOutput(outputId = cromwell_url_display()),
        badgeStatus = NULL
      )
    )
  })

  # Disable or enable the Delete button for deleting proof server
  observeEvent(input$cromwellDelete, {
    showModal(verifyCromwellDeleteModal())
  })

  # For the button WITHIN the Delete modal
  observe({
    shinyjs::toggleState("deleteCromwell", input$stopCromwell == "delete me")
  })

  observeEvent(input$deleteCromwell, {
    if (input$stopCromwell == "delete me") {
      try_delete <- tryCatch(proof_cancel(), error = function(e) e)
      if (rlang::is_error(try_delete)) {
        showModal(verifyCromwellDeleteModal(failed = TRUE, error = try_delete$message))
      }

      # wait for server to go down
      proof_wait_for_down()

      # reset loading spinner
      print("resetting loading spinner")
      shinyFeedback::resetLoadingButton("deleteCromwell")

      print("running removeModal() for Delete")
      removeModal()
      shinyjs::disable(id = "cromwellDelete")
      shinyjs::enable(id = "cromwellStart")
    } else {
      showModal(verifyCromwellDeleteModal(failed = TRUE))
    }
  })


  # Gather/show PROOF server status metadata when logged in
  # OR when user clicks "Update Status" button
  cromwellProofStatusData <- reactivePoll(1000, session,
    checkFunc = function() {
      if (proof_loggedin()) proof_status()$jobStatus
    },
    valueFunc = function() {
      proof_status()
    }
  )

  proofStatusTextGenerator <- function(name, list_index, value_if_null = NULL) {
    if (proof_loggedin()) {
      renderText(
        paste0(
          strong(glue("{name}: ")),
          purrr::flatten(cromwellProofStatusData())[[list_index]] %||% value_if_null
        )
      )
    }
  }

  output$proofStatusJobStatus <- proofStatusTextGenerator('Job status', 'jobStatus', "Stopped")
  output$proofStatusUrlStr <- if (proof_loggedin()) {
    renderText(
      paste0(
        strong("Cromwell URL: "),
        a(
          cromwellProofStatusData()$cromwellUrl,
          target = "_blank",
          href = cromwellProofStatusData()$cromwellUrl
        )
      )
    )
  }
  output$proofStatusWorkflowLogDir <- proofStatusTextGenerator('Workflow log directory', 'WORKFLOWLOGDIR')
  output$proofStatusScratchDir <- proofStatusTextGenerator('Scratch directory', 'SCRATCHDIR')
  output$proofStatusSlurmJobId <- proofStatusTextGenerator('Slurm job ID', 'SLURM_JOB_ID')
  output$proofStatusCromwellDir <- proofStatusTextGenerator('Cromwell directory', 'CROMWELL_DIR')
  output$proofStatusServerLogDir <- proofStatusTextGenerator('Server log directory', 'SERVERLOGDIR')
  output$proofStatusSingularityCacheDir <- proofStatusTextGenerator('Singlarity cache directory', 'SINGULARITY_CACHEDIR')
  output$proofStatusServerTime <- proofStatusTextGenerator('Server time', 'SERVERTIME')
  output$proofStatusUseAWS <- proofStatusTextGenerator('Use AWS?', 'USE_AWS')
  output$proofStatusSlurmJobAccount <- proofStatusTextGenerator('Slurm job account', 'SLURM_JOB_ACCOUNT')

  ###### Cromwell Validate tab ######
  ## Validate a possible workflow
  validateWorkflow <- eventReactive(input$validateWorkflow,
    {
      stop_safe_loggedin_serverup()
      cromwell_validate(
        wdl = input$validatewdlFile$datapath,
        all_inputs = input$validateinputFile$datapath
      )
    },
    ignoreNULL = TRUE
  )
  ## Show the validation result in a box
  output$validationResult <- renderPrint(validateWorkflow())

  # reset
  observeEvent(input$resetValidate, {
    purrr::map(
      c('validatewdlFile', 'validateinputFile'),
      shinyjs::reset
    )
    output$validationResult <- renderText({})
  })




  ###### Cromwell Submit tab ######
  ## Submit a workflow
  submitWorkflowJob <- eventReactive(input$submitWorkflow,
    {
      stop_safe_loggedin_serverup()
      cromwell_submit_batch(
        wdl = input$wdlFile$datapath,
        params = input$inputJSON$datapath,
        batch = input$input2JSON$datapath,
        options = input$workOptions$datapath,
        labels = data.frame(
          "workflowType" = "AppSubmission",
          "Label" = input$labelValue,
          "secondaryLabel" = input$seclabelValue
        )
      )
    },
    ignoreNULL = TRUE
  )
  ## Show the workflow submission result in a box
  output$submissionResult <- renderPrint(submitWorkflowJob())

  ## Troubleshoot a workflow
  troubleWorkflowJob <- eventReactive(input$troubleWorkflow,
    {
      stop_safe_loggedin_serverup()
      validate_workflowid(input$troubleWorkflowID)
      cromwell_glob(workflow_id = input$troubleWorkflowID)
    },
    ignoreNULL = TRUE
  )
  ## Show the abort workflow result in a box
  output$troubleResult <- renderPrint(troubleWorkflowJob())

  ## Abort a workflow
  abortWorkflowJob <- eventReactive(input$abortWorkflow,
    {
      stop_safe_loggedin_serverup()
      validate_workflowid(input$abortWorkflowID)
      cromwell_abort(workflow_id = input$abortWorkflowID)
    },
    ignoreNULL = TRUE
  )
  ## Show the abort workflow result in a box
  output$abortResult <- renderPrint(abortWorkflowJob())

  # reset
  observeEvent(input$resetSubmission, {
    purrr::map(c(
      'wdlFile', 'inputJSON', 'input2JSON',
      'workOptions', 'labelValue', 'seclabelValue'),
      shinyjs::reset
    )
    output$submissionResult <- renderText({})
    print(input$wdlFile)
  })


  ############ CROMWELL Tracking Tab  ############

  workflowUpdate <- eventReactive(input$trackingUpdate,
    {
      stop_safe_loggedin_serverup()
      if (input$workName == "") {
        cromTable <- cromwell_jobs(days = input$daysToShow, workflow_status = input$workStatus)
      } else {
        cromTable <- cromwell_jobs(
          days = input$daysToShow, workflow_status = input$workStatus,
          workflow_name = input$workName
        )
      }
      print("workflowUpdate(); Requesting Crowmell Job info")
      if ("workflow_id" %in% colnames(cromTable)) {
        workflowDat <- cromTable %>% select(one_of(
          "workflow_name", "workflow_id", "status", "submission", "start",
          "end", "workflowDuration"
        ), everything())
      } else {
        workflowDat <- data.frame(
          workflow_name = character(0), workflow_id = character(0),
          status = character(0), submission = character(0), start = character(0),
          end = character(0), workflowDuration = integer(0)
        )
      }
      workflowDat
    },
    ignoreNULL = TRUE
  )


  callDurationUpdate <- eventReactive(input$trackingUpdate,
    {
      stop_safe_loggedin_serverup()
      print("callDurationUpdate")
      if (nrow(workflowUpdate()) == 1 & is.na(workflowUpdate()$workflow_id[1])) {
        callDuration <- data.frame("noCalls" = "No workflows with calls were submitted, please choose a different time period. ")
      } else {
        callDuration <- purrr::map_dfr(workflowUpdate()$workflow_id, cromwell_call) %>%
          dplyr::select(workflow_id, callName, executionStatus, callDuration, jobId)
      }

      callDuration
    },
    ignoreNULL = TRUE
  )


  output$workflowDuration <- renderPlot({
    if ("workflow_name" %in% colnames(workflowUpdate())) {
      print("inside workflowDuration ...")
      ggplot(workflowUpdate(), aes(x = as.factor(workflow_name), y = as.numeric(workflowDuration))) +
        geom_point(aes(color = status), width = 0.05, size = 4) +
        coord_flip() +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_color_manual(values = my.cols) +
        ylab("Workflow Duration (mins)") +
        xlab("Workflow Name")
    } else {
      ggplot() +
        geom_blank()
    }
  })

  ## Render some info boxes
  output$submittedBox <- renderInfoBox({
    infoBox(
      "Total \nSubmitted",
      workflowUpdate() %>%
        filter(!is.na(workflow_id)) %>%
        summarize(n_distinct(workflow_id)),
      icon = icon("list"),
      color = "purple", width = 3
    )
  })
  output$successBox <- renderInfoBox({
    infoBox(
      "Success Rate", paste0(round(nrow(workflowUpdate()[workflowUpdate()$status == "Succeeded", ]) / nrow(workflowUpdate()) * 100, 0), " %"),
      icon = icon("grin"),
      color = "yellow", width = 3
    )
  })
  output$successBox <- renderInfoBox({
    infoBox(
      "Successful", if (is.na(workflowUpdate()$workflow_id[1])) {
        0
      } else {
        workflowUpdate() %>%
          filter(status == "Succeeded") %>%
          summarise(n_distinct(workflow_id))
      },
      icon = icon("grin"),
      color = "yellow", width = 3
    )
  })
  output$failBox <- renderInfoBox({
    infoBox(
      "Failed", if (is.na(workflowUpdate()$workflow_id[1])) {
        0
      } else {
        workflowUpdate() %>%
          filter(status == "Failed") %>%
          summarise(n_distinct(workflow_id))
      },
      icon = icon("sad-tear"),
      color = "red", width = 3
    )
  })
  output$inprogressBox <- renderInfoBox({
    infoBox(
      "In Progress", if (is.na(workflowUpdate()$workflow_id[1])) {
        0
      } else {
        workflowUpdate() %>%
          filter(status == "Running") %>%
          summarise(n_distinct(workflow_id))
      },
      icon = icon("sync"),
      color = "green", width = 3
    )
  })
  ## Get a table of workflow labels
  workflowLabels <- eventReactive(input$joblistCromwell_rows_selected, {
    print("find Labels")
    data <- workflowUpdate()
    focusID <- data[input$joblistCromwell_rows_selected, ]$workflow_id
    workflow <- cromwell_workflow(focusID)
    if ("workflow_name" %in% colnames(workflow)) {
      workflowDat <- workflow %>% select(-one_of("options", "workflow", "metadataSource", "inputs"))
    } else {
      workflowDat <<- workflow %>% mutate(workflow_name = "NA")
    }
    suppressWarnings(workflowDat %>% select(one_of("workflow_name", "workflowRoot", "submission", "start", "end", "status", "workflowDuration"), everything()))
  })
  output$workflowDescribe <- renderDT(
    data <- workflowLabels(),
    class = "compact",
    filter = "top",
    options = list(scrollX = TRUE), selection = "single", rownames = FALSE
  )
  ## Get a table of workflow options
  workflowOptions <- eventReactive(input$joblistCromwell_rows_selected, {
    print("find options")
    data <- workflowUpdate()
    focusID <- data[input$joblistCromwell_rows_selected, ]$workflow_id
    as.data.frame(jsonlite::fromJSON(cromwell_workflow(focusID)$options))
  })
  output$workflowOpt <- renderDT(
    data <- workflowOptions(),
    class = "compact",
    filter = "top",
    options = list(scrollX = TRUE), selection = "single", rownames = FALSE
  )
  ## Get a table of workflow inputs
  workflowInputs <- eventReactive(input$joblistCromwell_rows_selected, {
    print("find inputs")
    data <- workflowUpdate()
    focusID <- data[input$joblistCromwell_rows_selected, ]$workflow_id
    as.data.frame(jsonlite::fromJSON(cromwell_workflow(focusID)$inputs))
  })
  output$workflowInp <- renderDT(
    data <- workflowInputs(),
    class = "compact",
    filter = "top",
    options = list(scrollX = TRUE), selection = "single", rownames = FALSE
  )
  ## Render a list of jobs in a table for a workflow
  output$joblistCromwell <- renderDT(
    data <- workflowUpdate(),
    class = "compact",
    filter = "top",
    options = list(scrollX = TRUE), selection = "single", rownames = FALSE
  )


  #### Call Data
  callsUpdate <- eventReactive(
    input$joblistCromwell_rows_selected,
    {
      data <- workflowUpdate()
      focusID <<- data[input$joblistCromwell_rows_selected, ]$workflow_id
      print("callsUpdate(); Querying cromwell for metadata for calls.")
      theseCalls <- cromwell_call(focusID)
      if ("executionStatus" %in% colnames(theseCalls)) {
        callDat <<- theseCalls
      } else {
        callDat <<- theseCalls %>% mutate(executionStatus = "NA")
      }
      suppressWarnings(callDat %>% select(one_of("workflow_name", "detailedSubName", "callName", "executionStatus", "shardIndex", "callRoot", "start", "end", "callDuration", "docker", "modules"), everything()))
    },
    ignoreNULL = TRUE
  )

  output$workflowTiming <- renderPlot({
    if ("callName" %in% colnames(callsUpdate())) {
      print("in render plot ...")
      ggplot(callsUpdate(), aes(x = as.factor(callName), y = callDuration)) +
        geom_point(aes(color = executionStatus), size = 3) + # coord_flip() +
        theme_minimal() +
        theme(axis.text.x = element_text(hjust = 1, angle = 25)) +
        scale_color_manual(values = my.cols) +
        ylab("Call Duration (mins)") +
        xlab("Call Name")
    } else {
      ggplot() +
        geom_blank()
    }
  })

  ## Render some info boxes
  output$pendingBatch <- renderValueBox({
    infoBox(
      "Pending",
      value = nrow(callsUpdate() %>% filter(executionStatus %in% c("Starting", "QueuedInCromwell"))),
      icon = icon("clock"),
      color = "yellow", width = 6
    )
  })
  output$runningBatch <- renderInfoBox({
    infoBox(
      "Running",
      value = nrow(callsUpdate() %>% filter(executionStatus == "Running")),
      icon = icon("sync"),
      color = "teal", width = 6
    )
  })
  output$failedBatch <- renderInfoBox({
    infoBox(
      "Failed",
      value = nrow(callsUpdate() %>% filter(executionStatus == "Failed")),
      icon = icon("thumbs-down"),
      color = "maroon", width = 6
    )
  })
  output$succeededBatch <- renderInfoBox({
    infoBox(
      "Succeeded",
      value = nrow(callsUpdate() %>% filter(executionStatus == "Done")),
      icon = icon("thumbs-up"),
      color = "green", width = 6
    )
  })
  ## Jobs Lists
  output$tasklistBatch <- renderDT(
    data <- callsUpdate(),
    class = "compact",
    filter = "top",
    options = list(scrollX = TRUE),
    rownames = FALSE
  )

  output$downloadJobs <- downloadHandler(
    filename = function() {
      paste0(unique(callsUpdate()$workflow_id), "-workflowJobData.csv")
    },
    content = function(file) {
      write.csv(callsUpdate(), file, row.names = FALSE)
    }
  )

  ## Failure data
  failsUpdate <- eventReactive(input$getFailedData,
    {
      data <- workflowUpdate()
      focusID <- data[input$joblistCromwell_rows_selected, ]$workflow_id
      print("failsUpdate(); Querying cromwell for metadata for failures.")
      suppressWarnings(failDat <- cromwell_failures(focusID) %>%
        select(one_of(
          "callName", "jobId", "workflow_id", "detailedSubName", "shardIndex", "attempt",
          "failures.message", "failures.causedBy.message"
        ), everything()) %>% unique())
      return(failDat)
    },
    ignoreNULL = TRUE
  )

  output$failurelistBatch <- renderDT(
    data <- failsUpdate(),
    class = "compact",
    filter = "top",
    options = list(scrollX = TRUE),
    rownames = FALSE
  )

  output$downloadFails <- downloadHandler(
    filename = function() {
      paste0(unique(failsUpdate()$workflow_id), "-callFailureData.csv")
    },
    content = function(file) {
      write.csv(failsUpdate(), file, row.names = FALSE)
    }
  )

  ### Call Caching data
  cacheUpdate <- eventReactive(input$getCacheData,
    {
      data <- workflowUpdate()
      focusID <<- data[input$joblistCromwell_rows_selected, ]$workflow_id
      print("cacheUpdate(); Querying cromwell for metadata for call caching.")
      theseCache <- cromwell_cache(focusID)
      if ("callCaching.effectiveCallCachingMode" %in% colnames(theseCache)) {
        cacheDat <- theseCache
      } else {
        cacheDat <- theseCache %>% mutate(callCaching.effectiveCallCachingMode = "NA")
      }
      cacheDat
    },
    ignoreNULL = TRUE
  )

  output$cachingListBatch <- renderDT(
    data <- cacheUpdate() %>% select(workflow_name, workflow_id, callName, shardIndex, executionStatus, everything()) %>% unique(),
    class = "compact",
    filter = "top",
    options = list(scrollX = TRUE),
    rownames = FALSE
  )


  output$downloadCache <- downloadHandler(
    filename = function() {
      paste0(unique(cacheUpdate()$workflow_id), "-callCachingData.csv")
    },
    content = function(file) {
      write.csv(cacheUpdate(), file, row.names = FALSE)
    }
  )
  ## Render some info boxes
  output$cacheHits <- renderInfoBox({
    infoBox(
      "Cache Hits",
      value = if ("callCaching.hit" %in% colnames(cacheUpdate())) {
        nrow(cacheUpdate() %>% filter(callCaching.hit))
      } else {
        0
      },
      icon = icon("grin-tongue"),
      color = "aqua", width = 6
    )
  })
  output$cacheMisses <- renderInfoBox({
    infoBox(
      "Cache Misses",
      value = if ("callCaching.hit" %in% colnames(cacheUpdate())) {
        nrow(cacheUpdate() %>% filter(!callCaching.hit))
      } else {
        0
      },
      icon = icon("meh"),
      color = "orange", width = 6
    )
  })

  ## Outputs Data
  ### Go get the output data for the selected workflow
  outputsUpdate <- eventReactive(input$getOutputData,
    {
      data <- workflowUpdate()
      focusID <<- data[input$joblistCromwell_rows_selected, ]$workflow_id
      print("outputsUpdate(); Querying cromwell for a list of workflow outputs.")
      outDat <<- try(cromwell_outputs(focusID), silent = TRUE)
      if (!is.data.frame(outDat)) {
        outDat <- dplyr::tibble("workflow_id" = "No outputs are available for this workflow yet.")
      }
      outDat
    },
    ignoreNULL = TRUE
  )
  ## render outputs list to a table
  output$outputslistBatch <- renderDT(
    data <- outputsUpdate(),
    class = "compact",
    filter = "top",
    options = list(scrollX = TRUE),
    rownames = FALSE
  )
  ## Prep outputs table for download
  output$downloadOutputs <- downloadHandler(
    filename = function() {
      paste0(unique(outputsUpdate()$workflow_id), "-workflowOutputData.csv")
    },
    content = function(file) {
      write.csv(outputsUpdate(), file, row.names = FALSE)
    }
  )
}
