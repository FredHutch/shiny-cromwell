library(shiny)
library(shinyBS)
library(shinyjs)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyFeedback)
library(shinyWidgets)
library(shinyvalidate)

library(DT)
library(glue)
library(jsonlite)
library(rlang)

library(lubridate)
library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(tibble)
library(magrittr)

library(uuid)
library(httr)

library(proofr)
library(rcromwell)

library(rclipboard)

source("sidebar.R")
source("modals.R")
source("proof.R")
source("buttons.R")
source("utils.R")
source("tab-servers.R")
source("tab-welcome.R")
source("validators.R")

SANITIZE_ERRORS <- FALSE
PROOF_TIMEOUT <- 20
FOCUS_ID <- 1

# FIXME: maybe remove later, was running into some timeouts during testing
proof_timeout(sec = PROOF_TIMEOUT)

# sanitize errors - note that some actual errors will still happen
options(shiny.sanitize.errors = SANITIZE_ERRORS)

myCols <- brewer.pal(6, "RdYlBu")

server <- function(input, output, session) {
  session$allowReconnect(TRUE)

  # Upper right github icon for source code
  output$gitHtml <- renderText({
    glue('<b>Code</b>: <a href="https://github.com/FredHutch/shiny-cromwell/tree/dev" target="_blank">FredHutch/shiny-cromwell</a>
                    <br>
                    <b>Built from</b>: <a href="https://github.com/FredHutch/shiny-cromwell/tree/{git_sha()}" target="_blank">{substring(git_sha(), 1, 7)}</a>
          ')
  })

  rv <- reactiveValues(token = "", url = "", own = FALSE)

  # Login and UI component handling
  observeEvent(input$proofAuth, {
    showModal(loginModal())
  })

  output$userName <- renderText({
    input$username
  })

  observe({
    if (nzchar(rv$token)) {
      updateBox("boxServers", action = "update", options = list(width = 4))
      updateBox("boxValidate", action = "update", options = list(width = 4))
      updateBox("boxSubmit", action = "update", options = list(width = 4))
    } else {
      updateBox("boxValidate", action = "update", options = list(width = 6))
      updateBox("boxSubmit", action = "update", options = list(width = 6))
    }
  })

  output$uiSideBar <- renderMenu({
    if (nzchar(rv$token)) {
      proofSidebar()
    } else {
      nonProofSidebar()
    }
  })

  output$toggleServersBox <- renderUI({
    if (nzchar(rv$token)) {
      welcome_servers_box
    } else {
      NULL
    }
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
        rv$token <- try_auth
        cromwell_up <- tryCatch(
          proof_status(token = rv$token)$jobStatus,
          error = function(e) e
        )
        print(glue("cromwell_status {cromwell_up}"))
        if (!rlang::is_error(cromwell_up)) {
          if (!is.null(cromwell_up)) {
            cromwell_config(verbose = FALSE)
            rv$url <- proof_wait_for_up(rv$token)
          }
        }
        removeModal()
      }
    } else {
      showModal(loginModal(failed = TRUE))
    }
  })

  output$loggedInOut <- renderUI({
    if (proof_loggedin(rv$token)) {
      logOutButton
    } else {
      logInButton
    }
  })

  # Render bring your own Cromwell Server button
  output$ownCromwell <- renderUI({
    if (rv$own) {
      logOutCromwellButton
    } else {
      logInCromwellButton
    }
  })

  observeEvent(input$ownCrom, {
    if (proof_loggedin(rv$token)) session$reload()
  })

  observeEvent(input$ownCrom, {
    showModal(myCromwellModal())
  })

  iv <- validatorOwnCromwell()
  observeEvent(input$submitOwnCromwell, {
    if (iv$is_valid()) {
      print(input$ownCromwellURL)
      cromwell_config(verbose = FALSE)
      rv$url <- input$ownCromwellURL
      rv$own <- TRUE
      removeModal()
    } else {
      iv$enable()
    }
  })

  # Hiding login buttons
  observe({
    if (proof_loggedin(rv$token)) {
      shinyjs::hide("ownCrom")
    }
  })

  observe({
    if (rv$own) {
      shinyjs::hide("loggedInOut")
    }
  })

  # Handle logout for both buttons
  observeEvent(input$proofAuthLogout, {
    session$reload()
  })

  observeEvent(input$proofCromwellLogout, {
    session$reload()
  })


  ###### Cromwell servers tab ######
  # Start button handling
  observeEvent(input$cromwellStart, {
    showModal(cromwellStartModal())
  })

  observeEvent(input$beginCromwell, {
    if (proof_loggedin(rv$token)) {
      # fail out early if already running
      if (!proof_status(token = rv$token)$canJobStart) {
        # stop(safeError("Your Cromwell server is already running"))
        showModal(cromwellStartModal(failed = TRUE, error = "Your Cromwell server is already running"))
      } else {
        # start cromwell server
        try_start <- tryCatch(
          proof_start(slurm_account = input$slurmAccount, token = rv$token),
          error = function(e) e
        )

        if (rlang::is_error(try_start)) {
          showModal(cromwellStartModal(failed = TRUE, error = try_start$message))
        } else {
          cromwell_config(verbose = FALSE)
          rv$url <- proof_wait_for_up(rv$token)
          shiny::validate(
            shiny::need(
              !proof_status(token = rv$token)$canJobStart,
              "Your Cromwell server is not running. Go to  the Cromwell servers tab and click Start"
            )
          )

          # reset loading spinner
          shinyFeedback::resetLoadingButton("beginCromwell")

          removeModal()
          shinyjs::enable(id = "cromwellDelete")
          shinyjs::disable(id = "cromwellStart")
        }
      }
    } else {
      showModal(cromwellStartModal(failed = TRUE, error = "You're not logged in"))
    }
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
    if (proof_loggedin(rv$token)) {
      if (input$stopCromwell == "delete me") {
        try_delete <- tryCatch(proof_cancel(token = rv$token), error = function(e) e)
        if (rlang::is_error(try_delete)) {
          showModal(verifyCromwellDeleteModal(failed = TRUE, error = try_delete$message))
        }

        # wait for server to go down
        proof_wait_for_down(rv$token)

        # reset loading spinner
        shinyFeedback::resetLoadingButton("deleteCromwell")

        removeModal()
        shinyjs::disable(id = "cromwellDelete")
        shinyjs::enable(id = "cromwellStart")
      } else {
        showModal(verifyCromwellDeleteModal(failed = TRUE))
      }
    } else {
      showModal(verifyCromwellDeleteModal(failed = TRUE, error = "You're not logged in"))
    }
  })


  # Gather/show PROOF server status metadata when logged in
  cromwellProofStatusData <- reactivePoll(2000, session,
    checkFunc = function() {
      if (proof_loggedin(rv$token)) {
        proof_status(token = rv$token)$jobInfo$SCRATCHDIR
      } else {
        NULL
      }
    },
    valueFunc = function() {
      proof_status(token = rv$token)
    }
  )

  proofStatusTextGenerator <- function(name, list_index, tip = "", value_if_null = NULL) {
    renderUI({
      if (proof_loggedin(rv$token)) {
        tags$span(
          shinyBS::tipify(
            icon("question-circle"),
            tip,
            placement = "right"
          ),
          HTML(paste0(
            strong(glue("{name}: ")),
            purrr::flatten(cromwellProofStatusData())[[list_index]] %||% value_if_null
          ))
        )
      }
    })
  }

  output$proofStatusJobStatus <- proofStatusTextGenerator("Job status", "jobStatus", "PROOF server job status", value_if_null = "Stopped")
  output$proofStatusUrlStr <- proofStatusTextGenerator("Cromwell URL", "cromwellUrl")
  output$proofStatusWorkflowLogDir <- proofStatusTextGenerator("Workflow log directory", "WORKFLOWLOGDIR")
  output$proofStatusScratchDir <- proofStatusTextGenerator("Scratch directory", "SCRATCHDIR", "Working directory on Scratch")
  output$proofStatusSlurmJobId <- proofStatusTextGenerator("Slurm job ID", "SLURM_JOB_ID", "PROOF server SLURM job id")
  output$proofStatusCromwellDir <- proofStatusTextGenerator("Cromwell directory", "CROMWELL_DIR")
  output$proofStatusServerLogDir <- proofStatusTextGenerator("Server log directory", "SERVERLOGDIR", "PROOF server log directory")
  output$proofStatusSingularityCacheDir <- proofStatusTextGenerator("Singlarity cache directory", "SINGULARITYCACHEDIR")
  output$proofStatusServerTime <- proofStatusTextGenerator("Server time", "SERVERTIME", "PROOF server job lifetime")
  output$proofStatusUseAWS <- proofStatusTextGenerator("Use AWS?", "USE_AWS", "AWS credentials found?")
  output$proofStatusSlurmJobAccount <- proofStatusTextGenerator("Slurm job account", "SLURM_JOB_ACCOUNT", "Designated Gizmo PI account")

  ###### Cromwell Validate tab ######
  ## Validate a possible workflow
  validateWorkflow <- eventReactive(input$validateWorkflow,
    {
      stop_safe_loggedin_serverup(rv$url, rv$token, rv$own)
      cromwell_validate(
        wdl = input$validatewdlFile$datapath,
        all_inputs = input$validateinputFile$datapath,
        url = rv$url,
        token = rv$token
      )
    },
    ignoreNULL = TRUE
  )
  ## Show the validation result in a box
  output$validationResult <- renderPrint(validateWorkflow())

  # reset validate
  observeEvent(input$resetValidate, {
    purrr::map(
      c("validatewdlFile", "validateinputFile"),
      shinyjs::reset
    )
    output$validationResult <- renderText({})
  })




  ###### Cromwell Submit tab ######
  ## Submit a workflow
  submitWorkflowJob <- eventReactive(input$submitWorkflow,
    {
      stop_safe_loggedin_serverup(rv$url, rv$token, rv$own)
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
        url = rv$url,
        token = rv$token
      )
    },
    ignoreNULL = TRUE
  )
  ## Show the workflow submission result in a box
  output$submissionResult <- renderPrint(submitWorkflowJob())

  # reset
  observeEvent(input$resetSubmission, {
    purrr::map(
      c(
        "wdlFile", "inputJSON", "input2JSON",
        "workOptions", "labelValue", "seclabelValue"
      ),
      shinyjs::reset
    )
    output$submissionResult <- renderText({})
    print(input$wdlFile)
  })



  ###### Troubleshoot tab ######
  ## Abort a workflow
  abortWorkflowJob <- eventReactive(input$abortWorkflow,
    {
      validate_workflowid(input$abortWorkflowID)
      stop_safe_loggedin_serverup(rv$url, rv$token, rv$own)
      cromwell_abort(
        workflow_id = input$abortWorkflowID,
        url = rv$url,
        token = rv$token
      )
    },
    ignoreNULL = TRUE
  )
  ## Show the abort workflow result in a box
  output$abortResult <- renderPrint(abortWorkflowJob())

  ## reset abort
  observeEvent(input$resetAbort, {
    shinyjs::reset("abortWorkflowID")
    output$abortResult <- renderText({})
  })


  ## Troubleshoot a workflow
  troubleWorkflowJob <- eventReactive(input$troubleWorkflow,
    {
      validate_workflowid(input$troubleWorkflowID)
      stop_safe_loggedin_serverup(rv$url, rv$token, rv$own)
      cromwell_glob(
        workflow_id = input$troubleWorkflowID,
        url = rv$url,
        token = rv$token
      )
    },
    ignoreNULL = TRUE
  )
  ## Show the troubleshoot workflow result in a box
  output$troubleResult <- renderPrint(troubleWorkflowJob())

  ## reset trouble
  observeEvent(input$resetTrouble, {
    shinyjs::reset("troubleWorkflowID")
    output$troubleResult <- renderText({})
  })



  ############ CROMWELL Tracking Tab  ############

  workflowUpdate <- eventReactive(input$trackingUpdate,
    {
      print(rv$url)
      print(rv$own)
      stop_safe_loggedin_serverup(rv$url, rv$token, rv$own)
      if (input$workName == "") {
        cromTable <- cromwell_jobs(
          days = input$daysToShow,
          workflow_status = input$workStatus,
          url = rv$url,
          token = rv$token
        )
      } else {
        cromTable <- cromwell_jobs(
          days = input$daysToShow,
          workflow_status = input$workStatus,
          workflow_name = input$workName,
          url = rv$url,
          token = rv$token
        )
      }

      if ("workflow_id" %in% colnames(cromTable)) {
        workflowDat <- cromTable %>% select(one_of(
          "workflow_name", "workflow_id", "status", "submission", "start",
          "end", "workflowDuration"
        ), everything())

        if (NCOL(workflowDat) > 1) {
          # add copy to clipboard buttons
          workflowDat[["copyId"]] <- vapply(1L:nrow(workflowDat), function(i) {
            as.character(
              rclipButton(
                paste0("clipbtn_", i),
                label = "",
                clipText = workflowDat[i, "workflow_id"],
                icon = icon("copy"),
                class = "btn-secondary btn-sm",
                tooltip = "Click to copy the Workflow ID to the left",
                options = list(delay = list(show = 800, hide = 100), trigger = "hover")
              )
            )
          }, character(1L))

          # change date formats
          workflowDat <- dplyr::mutate(
            workflowDat,
            dplyr::across(
              c(submission, start, end),
              as_pt
            )
          )

          # reorder columns
          workflowDat <- dplyr::relocate(workflowDat, copyId, .after = workflow_id)
        }
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
      stop_safe_loggedin_serverup(rv$url, rv$token, rv$own)
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
        scale_color_manual(values = myCols) +
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
    FOCUS_ID <- data[input$joblistCromwell_rows_selected, ]$workflow_id
    workflow <- cromwell_workflow(FOCUS_ID,
      url = rv$url,
      token = rv$token
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
    FOCUS_ID <- data[input$joblistCromwell_rows_selected, ]$workflow_id
    as.data.frame(jsonlite::fromJSON(
      cromwell_workflow(FOCUS_ID,
        url = rv$url,
        token = rv$token
      )$options
    ))
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
    FOCUS_ID <- data[input$joblistCromwell_rows_selected, ]$workflow_id
    as.data.frame(jsonlite::fromJSON(
      cromwell_workflow(FOCUS_ID,
        url = rv$url,
        token = rv$token
      )$inputs
    ))
  })
  output$workflowInp <- renderDT(
    data <- workflowInputs(),
    class = "compact",
    filter = "top",
    options = list(scrollX = TRUE), selection = "single", rownames = FALSE
  )
  ## Render a list of jobs in a table for a workflow
  output$joblistCromwell <- renderDT({
    datatable(
      workflowUpdate(),
      escape = FALSE,
      selection = "single",
      rownames = FALSE,
      filter = "top",
      options = list(scrollX = TRUE)
    )
  })


  #### Call Data
  callsUpdate <- eventReactive(
    input$joblistCromwell_rows_selected,
    {
      data <- workflowUpdate()
      FOCUS_ID <<- data[input$joblistCromwell_rows_selected, ]$workflow_id
      print("callsUpdate(); Querying cromwell for metadata for calls.")
      theseCalls <- cromwell_call(FOCUS_ID,
        url = rv$url,
        token = rv$token
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
        scale_color_manual(values = myCols) +
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
      FOCUS_ID <- data[input$joblistCromwell_rows_selected, ]$workflow_id
      print("failsUpdate(); Querying cromwell for metadata for failures.")
      suppressWarnings(failDat <- cromwell_failures(FOCUS_ID,
        url = rv$url,
        token = rv$token
      ) %>%
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
      FOCUS_ID <<- data[input$joblistCromwell_rows_selected, ]$workflow_id
      print("cacheUpdate(); Querying cromwell for metadata for call caching.")
      theseCache <- cromwell_cache(FOCUS_ID,
        url = rv$url,
        token = rv$token
      )
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
      FOCUS_ID <<- data[input$joblistCromwell_rows_selected, ]$workflow_id
      print("outputsUpdate(); Querying cromwell for a list of workflow outputs.")
      outDat <<- try(cromwell_outputs(FOCUS_ID,
        url = rv$url,
        token = rv$token
      ), silent = TRUE)
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
