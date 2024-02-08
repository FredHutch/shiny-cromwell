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
library(cookies)
library(dplyr)
library(RSQLite)
library(DBI)

SANITIZE_ERRORS <- FALSE
PROOF_TIMEOUT <- 10
COOKIE_EXPIRY_DAYS <- 1
DB_LOCATION <- ":memory:"

db <- dbConnect(RSQLite::SQLite(), DB_LOCATION)
db_columns <- c(user = "TEXT", proof_token = "TEXT", cromwell_url = "TEXT", login_time = "TEXT")
if (!dbExistsTable(db, "users")) dbCreateTable(db, "users", db_columns)

user_from_db <- function(user, conn = db, expiry = COOKIE_EXPIRY_DAYS) {
  dbReadTable(conn, "users") %>%
    as_tibble() %>%
    filter(
      user == user,
      login_time > now() - days(expiry)
    ) %>%
    arrange(desc(login_time))
}
user_to_db <- function(user, token, url, conn = db) {
  tibble(
    user = user,
    proof_token = token,
    cromwell_url = url,
    login_time = as.character(now())
  ) %>%
    dbWriteTable(conn, "users", ., append = TRUE)
}
user_drop_from_db <- function(user, conn = db) {
  sql_delete <- glue::glue_sql("
    DELETE from users
    WHERE user = {user}
  ", .con = db)
  dbSendQuery(db, sql_delete)
}

focusID <- 1

# FIXME: maybe remove later, was running into some timeouts during testing
proof_timeout(sec = PROOF_TIMEOUT)

# sanitize errors - note that some actual errors will still happen
options(shiny.sanitize.errors = SANITIZE_ERRORS)

cromwell_url_display <- function(url) {
  paste0("Cromwell URL: ", url %||% "No Cromwell Server found")
}

proof_wait_for_up <- function(token) {
  not_up <- TRUE
  while (not_up) {
    cromwell_url <- proof_status(token = token)$cromwellUrl
    if (!is.null(cromwell_url)) not_up <- FALSE
  }
  cromwell_url
}

proof_wait_for_down <- function(token) {
  up <- TRUE
  while (up) {
    cromwell_url <- proof_status(token = token)$cromwellUrl
    if (is.null(cromwell_url)) up <- FALSE
  }
}

proof_loggedin <- function(token) {
  is.character(token) && nzchar(token)
}

cromwell_version_safe <- function(url, token) {
  tmp <- tryCatch(
    cromwell_version(url = url, token = token),
    error = function(e) e
  )
  list(
    result = if (rlang::is_error(tmp)) FALSE else tmp,
    error = if (rlang::is_error(tmp)) tmp$message else NULL
  )
}

proof_loggedin_serverup <- function(url, token) {
  proof_loggedin(token) && rlang::is_list(cromwell_version_safe(url, token)$result)
}

proof_serverup <- function(url, token) {
  rlang::is_list(cromwell_version_safe(url, token)$result)
}

stop_safe <- function(fun, ..., message) if (!fun(...)) stop(safeError(message))
stop_safe_loggedin_serverup <- function(url, token) {
  stop_safe(proof_loggedin, token = token, message = "Not logged in! Please log in")
  stop_safe(proof_serverup, url = url, token = token, message = "Your Cromwell server is not up!")
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
    ),
    easyClose = TRUE
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

logInButton <-
  actionButton(
    inputId = "proofAuth",
    label = "PROOF Login",
    icon = icon("lock")
  )
logOutButton <-
  actionButton(
    inputId = "proofAuthLogout",
    label = "Log out",
    class = "btn-danger",
    style = "color: white;"
  )

server <- function(input, output, session) {
  r_url <- shiny::reactiveVal("")
  r_token <- shiny::reactiveVal("")
  r_user <- shiny::reactiveVal()

  observeEvent(input$proofAuth, {
    showModal(loginModal())
  })

  observeEvent(
    cookies::get_cookie("user"), {

    r_user(cookies::get_cookie("user"))
    user_df <- user_from_db(r_user()) %>% top_n(1)
    if (nrow(user_df)) {
      r_url(user_df$cromwell_url)
      r_token(user_df$proof_token)
    }
  })

  output$loggedInOut <- renderUI({
    if (proof_loggedin(r_token())) {
      logOutButton
    } else {
      logInButton
    }
  })

  observeEvent(input$proofAuthLogout, {
    cookies::remove_cookie("user")
    session$reload()
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
        r_token(try_auth)
        r_user(input$username)

        cookies::set_cookie(
          cookie_name = "user",
          cookie_value = r_user(),
          expiration = COOKIE_EXPIRY_DAYS,
          secure_only = TRUE,
          same_site = "strict"
        )

        cromwell_up <- tryCatch(
          proof_status(token = r_token())$jobStatus,
          error = function(e) e
        )
        print(glue("cromwell_up {cromwell_up}"))
        if (!rlang::is_error(cromwell_up)) {
          if (!is.null(cromwell_up)) {
            cromwell_config(verbose = FALSE)
            r_url(proof_wait_for_up(r_token()))
          }
        }
        user_to_db(r_user(), r_token(), r_url())
        removeModal()
        session$reload()
      }
    } else {
      showModal(loginModal(failed = TRUE))
    }
  })

  # output$url <- renderText({
  #   r_url()
  # })
  # output$token <- renderText({
  #   r_token()
  # })

  # update the icon in the PROOF Login button
  # (FIXME: ideally we change the color of the button too but not sure how to do that yet)
  observeEvent(proof_loggedin_serverup(r_url(), r_token()), {
    updateActionButton(session, "proofAuth", icon = icon("unlock"))
  })

  ###### Cromwell servers tab ######
  ## Alert that need to login first
  observe({
    if (!proof_loggedin(r_token())) {
      shinyBS::createAlert(session,
        "alert_loggedin",
        title = "Heads up",
        content = HTML("You aren't logged in. Click the <strong>Proof Login</strong> button to the left"),
        style = "warning",
        append = FALSE
      )
    } else {
      shinyBS::closeAlert(session, "alert_loggedin")
    }
  })

  # Hide or show start and stop buttons
  observe({
    if (!proof_loggedin(r_token())) {
      shinyjs::disable(id = "cromwellStart")
      shinyjs::disable(id = "cromwellDelete")
    }
    if (!proof_serverup(r_url(), r_token())) shinyjs::disable(id = "cromwellDelete")
    if (proof_loggedin_serverup(r_url(), r_token())) {
      if (proof_status(token = r_token())$canJobStart) {
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
    if (proof_loggedin(r_token())) {
      # fail out early if already running
      if (!proof_status(token = r_token())$canJobStart) {
        stop(safeError("Your Cromwell server is already running"))
      }

      # start cromwell server
      proof_start(slurm_account = input$slurmAccount, token = r_token())

      # set cromwell server url
      # cromwell_config(proof_wait_for_up(r_token()), verbose = FALSE)
      cromwell_config(verbose = FALSE)
      r_url(proof_wait_for_up(r_token()))
      shiny::validate(
        shiny::need(
          !proof_status(token = r_token())$canJobStart,
          "Your Cromwell server is not running. Go to  the Cromwell servers tab and click Start"
        )
      )
      user_drop_from_db(r_user())
      user_to_db(r_user(), r_token(), r_url())

      # reset loading spinner
      shinyFeedback::resetLoadingButton("beginCromwell")

      removeModal()
      shinyjs::enable(id = "cromwellDelete")
      shinyjs::disable(id = "cromwellStart")
    }
  })

  # output$cromwellURI <- renderText({
  #   cromwell_url_display(r_url())
  # })

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
      try_delete <- tryCatch(proof_cancel(token = r_token()), error = function(e) e)
      if (rlang::is_error(try_delete)) {
        showModal(verifyCromwellDeleteModal(failed = TRUE, error = try_delete$message))
      }

      # wait for server to go down
      proof_wait_for_down(r_token())

      # reset loading spinner
      shinyFeedback::resetLoadingButton("deleteCromwell")

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
      if (proof_loggedin(r_token())) proof_status(token = r_token())$jobStatus
    },
    valueFunc = function() {
      proof_status(token = r_token())
    }
  )

  proofStatusTextGenerator <- function(name, list_index, value_if_null = NULL) {
      renderText(
        if (proof_loggedin(r_token())) {
          paste0(
            strong(glue("{name}: ")),
            purrr::flatten(cromwellProofStatusData())[[list_index]] %||% value_if_null
          )
        }
      )
  }

  output$proofStatusJobStatus <- proofStatusTextGenerator('Job status', 'jobStatus', "Stopped")
  output$proofStatusUrlStr <- renderText(
    if (proof_loggedin(r_token())) {
      paste0(
        strong("Cromwell URL: "),
        a(
          cromwellProofStatusData()$cromwellUrl,
          target = "_blank",
          href = cromwellProofStatusData()$cromwellUrl
        )
      )
    }
  )
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
      stop_safe_loggedin_serverup(r_url(), r_token())
      cromwell_validate(
        wdl = input$validatewdlFile$datapath,
        all_inputs = input$validateinputFile$datapath,
        url = r_url(),
        token = r_token()
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
      stop_safe_loggedin_serverup(r_url(), r_token())
      cromwell_submit_batch(
        wdl = input$wdlFile$datapath,
        params = input$inputJSON$datapath,
        batch = input$input2JSON$datapath,
        options = input$workOptions$datapath,
        labels = data.frame(
          "workflowType" = "AppSubmission",
          "Label" = input$labelValue,
          "secondaryLabel" = input$seclabelValue
        ),
        url = r_url(),
        token = r_token()
      )
    },
    ignoreNULL = TRUE
  )
  ## Show the workflow submission result in a box
  output$submissionResult <- renderPrint(submitWorkflowJob())

  ## Troubleshoot a workflow
  troubleWorkflowJob <- eventReactive(input$troubleWorkflow,
    {
      stop_safe_loggedin_serverup(r_url(), r_token())
      validate_workflowid(input$troubleWorkflowID)
      cromwell_glob(
        workflow_id = input$troubleWorkflowID,
        url = r_url(),
        token = r_token()
      )
    },
    ignoreNULL = TRUE
  )
  ## Show the abort workflow result in a box
  output$troubleResult <- renderPrint(troubleWorkflowJob())

  ## Abort a workflow
  abortWorkflowJob <- eventReactive(input$abortWorkflow,
    {
      stop_safe_loggedin_serverup(r_url(), r_token())
      validate_workflowid(input$abortWorkflowID)
      cromwell_abort(
        workflow_id = input$abortWorkflowID,
        url = r_url(),
        token = r_token()
      )
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
      stop_safe_loggedin_serverup(r_url(), r_token())
      if (input$workName == "") {
        cromTable <- cromwell_jobs(
          days = input$daysToShow,
          workflow_status = input$workStatus,
          url = r_url(),
          token = r_token()
        )
      } else {
        cromTable <- cromwell_jobs(
          days = input$daysToShow,
          workflow_status = input$workStatus,
          workflow_name = input$workName,
          url = r_url(),
          token = r_token()
        )
      }
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
      stop_safe_loggedin_serverup(r_url(), r_token())
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
    workflow <- cromwell_workflow(focusID,
      url = r_url(),
      token = r_token()
    )
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
    as.data.frame(jsonlite::fromJSON(
      cromwell_workflow(focusID,
        url = r_url(),
        token = r_token()
      )$options))
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
    as.data.frame(jsonlite::fromJSON(
      cromwell_workflow(focusID,
        url = r_url(),
        token = r_token())$inputs))
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
      theseCalls <- cromwell_call(focusID,
        url = r_url(),
        token = r_token()
      )
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
      suppressWarnings(failDat <- cromwell_failures(focusID,
        url = r_url(),
        token = r_token()) %>%
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
      theseCache <- cromwell_cache(focusID,
        url = r_url(),
        token = r_token())
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
      outDat <<- try(cromwell_outputs(focusID,
        url = r_url(),
        token = r_token()), silent = TRUE)
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
