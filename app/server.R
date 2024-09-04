library(shiny)
library(shinyBS)
library(shinyjs)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyFeedback)
library(shinyWidgets)
library(shinyvalidate)
library(shinylogs)

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

library(cookies)
library(listviewer)

library(parsedate)

source("sidebar.R")
source("modals.R")
source("proof.R")
source("buttons.R")
source("utils.R")
source("tab-servers.R")
source("tab-welcome.R")
source("validators.R")
source("inputs_utils.R")
source("cookies-db.R")

SANITIZE_ERRORS <- FALSE
PROOF_TIMEOUT <- 20
FOCUS_ID <- 1
SHINY_LOGGING <- as.logical(Sys.getenv("SHINY_LOG", FALSE))

# FIXME: maybe remove later, was running into some timeouts during testing
proof_timeout(sec = PROOF_TIMEOUT)

# sanitize errors - note that some actual errors will still happen
options(shiny.sanitize.errors = SANITIZE_ERRORS)

myCols <- brewer.pal(6, "RdYlBu")

server <- function(input, output, session) {
  if (SHINY_LOGGING) track_usage(storage_mode = store_null())

  session$allowReconnect(TRUE)

  # Upper right github icon for source code
  output$gitHtml <- renderText({
    glue('<b>Code</b>: <a href="https://github.com/FredHutch/shiny-cromwell/tree/{COMMIT_BRANCH}" target="_blank">FredHutch/shiny-cromwell</a>
                    <br>
                    <b>Built from</b>: <a href="https://github.com/FredHutch/shiny-cromwell/tree/{COMMIT_SHA}" target="_blank">{substring(COMMIT_SHORT_SHA, 1, 7)}</a>
                    <br>
                    <b>Last built on</b>: {stamp("Mar 1, 1999", quiet = TRUE)(ymd_hms(COMMIT_TIMESTAMP))}
          ')
  })

  rv <- reactiveValues(token = "", url = "", validateFilepath="", own = FALSE, user = "")

  rv_file <- reactiveValues(
    validatewdlFile_state = NULL,
    validateinputFile_state = NULL,
    wdlFile_state = NULL,
    inputJSON_state = NULL,
    input2JSON_state = NULL,
    workOptions_state = NULL,
    abortWorkflowID_state = NULL,
    troubleWorkflowID_state = NULL
  )

  # Login and UI component handling
  observeEvent(input$proofAuth, {
    showModal(loginModal())
  })

  observeEvent(cookies::get_cookie("user"), {
    rv$user <- cookies::get_cookie("user")
    # print(glue("in observeEvent(cookies::get_cookie(user) -----> {rv$user}"))
    if (!is.null(rv$user)) {
      user_df <- user_from_db(rv$user) %>% top_n(1)
      if (nrow(user_df)) {
        # print(user_df)
        rv$url <- from_base64(user_df$cromwell_url)
        rv$token <- from_base64(user_df$proof_token)
      } else {
        # force reload b/c no data from the user in the DB, so need to re-login
        # cookies::remove_cookie("user")
        # session$reload()
      }
    }
  })

  output$userName <- renderText({
    if (is.null(input$username)) {
      rv$user
    } else {
      input$username
    }
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
        rv$user <- input$username

        cromwell_up <- tryCatch(
          proof_status(token = rv$token)$jobStatus,
          error = function(e) e
        )
        if (!rlang::is_error(cromwell_up)) {
          if (!is.null(cromwell_up)) {
            cromwell_config(verbose = FALSE)
            rv$url <- proof_wait_for_up(rv$token)
          }
        }

        user_to_db(
          user = rv$user,
          token = to_base64(rv$token),
          url = to_base64(rv$url),
          drop_existing = TRUE
        )

        cookies::set_cookie(
          cookie_name = "user",
          cookie_value = rv$user,
          expiration = COOKIE_EXPIRY_DAYS,
          secure_only = TRUE,
          same_site = "strict"
        )

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
    user_drop_from_db(rv$user)
    cookies::remove_cookie("user")
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
              "Your Cromwell server is not running. Go to the Cromwell servers tab and click Start"
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
      if (!is.null(input$tabs)) {
        if (input$tabs != "cromwell") return(NULL)
      }
      if (proof_loggedin(rv$token)) {
        tmp <- proof_status(token = rv$token)
        paste0(tmp$jobStatus, tmp$jobInfo$SCRATCHDIR)
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
        if (nzchar(tip)) {
          tags$span(
            bslib::tooltip(
              icon("question-circle"),
              tip,
              placement = "right"
            ),
            HTML(paste0(
              strong(glue("{name}: ")),
              purrr::flatten(cromwellProofStatusData())[[list_index]] %||% value_if_null
            ))
          )
        } else {
          tags$span(
            icon("question-circle"),
            HTML(paste0(
              strong(glue("{name}: ")),
              purrr::flatten(cromwellProofStatusData())[[list_index]] %||% value_if_null
            ))
          )
        }
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
  output$proofStatusServerStartTime <- proofStatusTextGenerator("PROOF Server Start Time", "jobStartTime")

  ###### Cromwell Validate tab ######
  ## Validate a possible workflow
  file_validatewdlFile <- reactive({
    reactiveInput(
      rv_file$validatewdlFile_state,
      input$validatewdlFile$datapath
    )
  })
  file_validateinputFile <- reactive({
    reactiveInput(
      rv_file$validateinputFile_state,
      input$validateinputFile$datapath
    )
  })

  observeEvent(input$validatewdlFile, {
    rv_file$validatewdlFile_state <- 'loaded'
  })
  observeEvent(input$validateinputFile, {
    rv_file$validateinputFile_state <- 'loaded'
  })

  observeEvent(input$validateWorkflow, {
    output$validationResult <- renderPrint({
      stop_safe_loggedin_serverup(rv$url, rv$token, rv$own)
      cromwell_validate(
        wdl = isolate(file_validatewdlFile()),
        all_inputs = isolate(file_validateinputFile()),
        url = rv$url,
        token = rv$token
      )
    })
  })
      
  # reset
  observeEvent(input$resetValidate, {
    reset_inputs(c("validatewdlFile", "validateinputFile"))
    rv_file$validatewdlFile_state <- 'reset'
    rv_file$validateinputFile_state <- 'reset'
    output$validationResult <- renderText({})
  })




  ###### Cromwell Submit tab ######
  ## Submit a workflow
  file_wdlFile <- reactive({
    reactiveInput(rv_file$wdlFile_state, input$wdlFile$datapath)
  })
  file_inputJSON <- reactive({
    reactiveInput(rv_file$inputJSON_state, input$inputJSON$datapath)
  })
  file_input2JSON <- reactive({
    reactiveInput(rv_file$input2JSON_state, input$input2JSON$datapath)
  })
  file_workOptions <- reactive({
    reactiveInput(rv_file$workOptions_state, input$workOptions$datapath)
  })

  observeEvent(input$wdlFile, {
    rv_file$wdlFile_state <- 'loaded'
  })
  observeEvent(input$inputJSON, {
    rv_file$inputJSON_state <- 'loaded'
  })
  observeEvent(input$input2JSON, {
    rv_file$input2JSON_state <- 'loaded'
  })
  observeEvent(input$workOptions, {
    rv_file$workOptions_state <- 'loaded'
  })

  observeEvent(input$submitWorkflow, {
    output$submissionResult <- renderUI({
      stop_safe_loggedin_serverup(rv$url, rv$token, rv$own)
      df <- cromwell_submit_batch(
        wdl = isolate(file_wdlFile()),
        params = isolate(file_inputJSON()),
        batch = isolate(file_input2JSON()),
        options = isolate(file_workOptions()),
        labels = data.frame(
          "workflowType" = "AppSubmission",
          "Label" = isolate(input$labelValue),
          "secondaryLabel" = isolate(input$seclabelValue)
        ),
        url = rv$url,
        token = rv$token
      )
      HTML(glue('
        <br>
        <ul>
          <li><strong>Workflow ID:</strong> {df$id}</li>
          <li><strong>Status:</strong> {df$status}</li>
        </ul>
      '))
    })
  })

  # reset
  observeEvent(input$resetSubmission, {
    reset_inputs(c(
      "wdlFile", "inputJSON", "input2JSON",
      "workOptions", "labelValue", "seclabelValue"
    ))
    rv_file$wdlFile_state <- 'reset'
    rv_file$inputJSON_state <- 'reset'
    rv_file$input2JSON_state <- 'reset'
    rv_file$workOptions_state <- 'reset'
    output$submissionResult <- renderText({})
  })



  ###### Troubleshoot tab ######
  ## Abort a workflow
  input_abortWorkflowID <- reactive({
    reactiveInput(rv_file$abortWorkflowID_state, input$abortWorkflowID)
  })
  observeEvent(input$abortWorkflowID, {
    rv_file$abortWorkflowID_state <- 'loaded'
  })

  observeEvent(input$abortWorkflow, {
    output$abortResult <- renderPrint({
      validate_workflowid(isolate(input$abortWorkflowID))
      stop_safe_loggedin_serverup(rv$url, rv$token, rv$own)
      cromwell_abort(
        workflow_id = isolate(input_abortWorkflowID()),
        url = rv$url,
        token = rv$token
      )
    })
  })

  ## reset abort
  observeEvent(input$resetAbort, {
    reset_inputs("abortWorkflowID")
    rv_file$abortWorkflowID_state <- 'reset'
    output$abortResult <- renderText({})
  })


  ## Troubleshoot a workflow
  input_troubleWorkflowID <- reactive({
    reactiveInput(rv_file$troubleWorkflowID_state, input$troubleWorkflowID)
  })
  observeEvent(input$troubleWorkflowID, {
    rv_file$troubleWorkflowID_state <- 'loaded'
  })

  observeEvent(input$troubleWorkflow, {
    output$troubleResult <- renderPrint({
      validate_workflowid(isolate(input$troubleWorkflowID))
      stop_safe_loggedin_serverup(rv$url, rv$token, rv$own)
      cromwell_glob(
        workflow_id = isolate(input_troubleWorkflowID()),
        url = rv$url,
        token = rv$token
      )
    })
  })

  ## reset trouble
  observeEvent(input$resetTrouble, {
    reset_inputs("troubleWorkflowID")
    rv_file$troubleWorkflowID_state <- 'reset'
    output$troubleResult <- renderText({})
  })



  ############ CROMWELL Tracking Tab  ############

  workflowUpdate <- eventReactive(input$trackingUpdate, {
      stop_safe_loggedin_serverup(rv$url, rv$token, rv$own)
      # if (input$workName == "") {
      #   cromTable <- cromwell_jobs(
      #     days = input$daysToShow,
      #     days = 60,
      #     workflow_status = input$workStatus,
      #     url = rv$url,
      #     token = rv$token
      #   )
      # } else {
      #   cromTable <- cromwell_jobs(
      #     days = input$daysToShow,
      #     workflow_status = input$workStatus,
      #     workflow_name = input$workName,
      #     url = rv$url,
      #     token = rv$token
      #   )
      # }

      cromTable <- cromwell_jobs(
        days = 60,
        url = rv$url,
        token = rv$token
      )
    
      if ("workflow_id" %in% colnames(cromTable)) {
        workflowDat <- cromTable %>% select(one_of(
          "workflow_name", "workflow_id", "status", "submission", "start",
          "end", "workflowDuration"
        ), everything())

        if (NCOL(workflowDat) > 1) {
          workflowDat <- workflowDat %>%
            rowwise() %>%
            mutate(
              copyId = make_copybtn(workflow_id, "clipbtn_", "Copy Workflow ID")
            ) %>%
            ungroup()

          # change date formats
          workflowDat <- dplyr::mutate(
            workflowDat,
            dplyr::across(
              matches(c("submission", "start", "end")),
              as_pt
            )
          )

          # Add go to WDL viewer button
          workflowDat <- workflowDat %>%
            rowwise() %>%
            mutate(
              wdl = make_wdlbtn(workflow_id)
            ) %>%
            ungroup()

          # Add workflow labels
          ## Get labels data
          labels_df <- lapply(workflowDat$workflow_id, \(x) {
            as_tibble_row(cromwell_labels(x, url = rv$url, token = rv$token)) %>%
              mutate(workflow_id = sub("cromwell-", "", workflow_id))
          }) %>%
            bind_rows()
          workflowDat <- left_join(workflowDat, labels_df, by = "workflow_id")
          ## Then reorder columns
          workflowDat <- dplyr::relocate(workflowDat, wdl, .after = workflow_id)
          workflowDat <- dplyr::relocate(workflowDat, copyId, .after = wdl)
          workflowDat <- dplyr::relocate(workflowDat, Label, .after = copyId)
          workflowDat <- dplyr::relocate(workflowDat, secondaryLabel, .after = Label)
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
    # ignoreNULL = FALSE so that data for the tracking page loads when the user logs in
    # ignoreNULL = TRUE would mean the data will only load after user first clicks
    # the "Refresh data button"
    ignoreNULL = FALSE
  )

  observeEvent(input$selectedWorkflowId, {
    mermaid_file <- wdl_to_file(
      workflow_id = input$selectedWorkflowId,
      url = rv$url,
      token = rv$token
    )
    mermaid_str <- wdl2mermaid(mermaid_file)
    output$mermaid_diagram <- renderUI({
      mermaid_container(mermaid_str)
    })
  })

  ### go back to tracking tab from details tab
  observeEvent(input$linkToTrackingTab_from_workflow_inputs, {
    nav_select("proof", "Track")
  })
  observeEvent(input$linkToTrackingTab_from_mermaid, {
    nav_select("proof", "Track")
  })

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
        theme_minimal(base_size=16) +
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
  is_workflow_empty <- function() {
    NROW(workflowUpdate()) == 0 || NCOL(workflowUpdate()) == 1
  }
  output$submittedBoxValue <- renderText({
    if (is_workflow_empty()) {
      return(0)
    } else {
      workflowUpdate() %>%
        filter(!is.na(workflow_id)) %>%
        summarize(n_distinct(workflow_id)) %>%
        pull(1)
    }
  })
  output$successBoxValue <- renderText({
    if (is_workflow_empty()) {
      return(0)
    } else {
      workflowUpdate() %>%
        filter(status == "Succeeded") %>%
        summarise(n_distinct(workflow_id)) %>%
        pull(1)
    }
  })
  output$failBoxValue <- renderText({
    if (is_workflow_empty()) {
      return(0)
    } else {
      workflowUpdate() %>%
        filter(status == "Failed") %>%
        summarise(n_distinct(workflow_id)) %>%
        pull(1)
    }
  })
  output$inprogressBoxValue <- renderText({
    if (is_workflow_empty()) {
      return(0)
    } else {
      workflowUpdate() %>%
        filter(status == "Running") %>%
        summarise(n_distinct(workflow_id)) %>%
        pull(1)
    }
  })

  # Data for cards out of workflowUpdate data
  output$workflows_cards <- renderUI({
    dflst <- apply(workflowUpdate(), 1, as.list)
    dat <- lapply(dflst, function(w) {
      list(
       data = w,
       card = card(
          id = glue("job_card_{w$workflow_id}"),
          class = "border border-secondary",
          card_header(
            w$workflow_id,
            actionButton(
              "goToWorkflowDetails",
              label = "Details", 
              icon = icon("rectangle-list"),
              class = "btn-secondary btn-sm",
              onclick = glue('Shiny.setInputValue(\"selectedWorkflowId\", \"{w$workflow_id}\")')
            ),
            class = "d-flex justify-content-between gap-1",
            # class = "bg-secondary"
          ),
          card_body(
            class = "d-flex align-items-left justify-content-between gap-1",
            fillable = FALSE,
            tags$button(
              w$status,
              class = glue("btn btn-{card_header_color(w$status)} btn-sm col-2")
            ),
            span(bsicons::bs_icon("send"), w$submission),
            span(bsicons::bs_icon("clock-history"), w$workflowDuration)
          ),
          card_body(
            fillable = FALSE,
            span(bsicons::bs_icon("person-badge"), w$workflow_name),
            span(bsicons::bs_icon("tag-fill"), w$Label),
            span(bsicons::bs_icon("tag"), w$secondaryLabel)
          )
        )
      )
    })
    # Filter by date
    dat <- Filter(\(w) {
      parse_date(w$data$submission) >= parse_date(input$runs_date[1]) && 
        parse_date(w$data$submission) <= parse_date(input$runs_date[2])
    }, dat)
    # Filter by status
    if (!is.null(input$workStatus)) {
      dat <- Filter(\(w) {
        w$data$status %in% input$workStatus
      }, dat)
    }
    # Filter by workflow name
    if (nzchar(input$workName)) {
      dat <- Filter(\(w) {
        w$data$workflow_name == input$workName
      }, dat)
    }
    # return cards
    purrr::map(dat, "card")
  })

  observeEvent(input$goToWorkflowDetails, {
    print(input$goToWorkflowDetails)
    nav_select("proof", "Workflow Details")
  })

  ## reset trouble
  observeEvent(input$resetTrackingFilters, {
    reset_inputs("workName")
    reset_inputs("workStatus")
    reset_inputs("runs_date")
  })

  output$selectedWorkflowUI <- renderUI({
    if (!is.null(input$selectedWorkflowId)) {
      h4(bsicons::bs_icon("caret-right"), input$selectedWorkflowId)
    }
  })

  ## Get a table of workflow labels
  workflowLabels <- eventReactive(input$selectedWorkflowId, {
    print("find Labels")
    data <- workflowUpdate()
    FOCUS_ID <- input$selectedWorkflowId
    workflow <- cromwell_workflow(FOCUS_ID,
      url = rv$url,
      token = rv$token
    )
    if ("workflow_name" %in% colnames(workflow)) {
      workflowDat <- workflow %>% select(-one_of("options", "workflow", "metadataSource", "inputs"))
    } else {
      workflowDat <- workflow %>% mutate(workflow_name = "NA")
    }
    if (NROW(workflowDat) > 0) {
      workflowDat <- workflowDat %>%
        rowwise() %>%
        mutate(
          workflow = make_copybtn(workflow, "clipbtn_wflow_", "Copy workflow text"),
          inputs = make_copybtn(inputs, "clipbtn_inputs_", "Copy inputs text")
        ) %>%
        ungroup()

      # change date formats
      workflowDat <- dplyr::mutate(
        workflowDat,
        dplyr::across(
          matches(c("submission", "start", "end")),
          as_pt
        )
      )
    }

    suppressWarnings(
      workflowDat %>%
        select(
          one_of(
            "workflowName", "workflowRoot", "submission", "start",
            "end", "status", "workflowDuration"
          ),
          everything()
        )
      )
    })
  output$workflowDescribe <- renderDT({
    datatable(
      workflowLabels(),
      escape = FALSE,
      selection = "single",
      rownames = FALSE,
      filter = "top",
      options = list(scrollX = TRUE)
    )
  })
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
  workflowInputs <- eventReactive(input$selectedWorkflowId, {
    print("find inputs")
    data <- workflowUpdate()

    FOCUS_ID <- input$selectedWorkflowId
    output$currentWorkflowId <- renderText({
      paste("Workflow ID: ", FOCUS_ID)
    })

    cromwell_workflow(FOCUS_ID,
      url = rv$url,
      token = rv$token
    )$inputs
  })
  ### inputs json javascript viewer
  output$workflowInp <- renderReactjson({
    reactjson(workflowInputs())
  })
  ### edit json viewer
  observeEvent(input$workflowInp_edit, {
    str(input$workflowInp_edit, max.level=2)
  })
  ### set workflow id display in viewer tab back to none
  ### when nothing selected in the Workflows Run table
  observeEvent(input$joblistCromwell_rows_selected, {
    output$currentWorkflowId <- renderText({"Workflow ID: "})
  }, ignoreNULL = FALSE)

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
    input$selectedWorkflowId,
    {
      data <- workflowUpdate()
      FOCUS_ID <<- input$selectedWorkflowId
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
  output$tasklistBatch <- renderDT({
    datatable(
      {
        callsUpdate() %>%
          dplyr::mutate(dplyr::across(matches(c("start", "end")), as_pt))
      },
      escape = FALSE,
      selection = "single",
      rownames = FALSE,
      filter = "top",
      options = list(
        scrollX = TRUE,
        columnDefs = list(
          list(
            targets = "_all",
            render = JS(
              "function(data, type, row, meta) {",
                "if (data === null) {",
                "return data;",
                "} else {",
                "return type === 'display' && data.length > 150 ?",
                "'<span title=\"' + data + '\">' + data.substr(0, 150) + '...</span>' : data;",
                "}",
              "}"
            )
          )
        )
      )
    )
  })

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
      FOCUS_ID <- input$selectedWorkflowId
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
      FOCUS_ID <<- input$selectedWorkflowId
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
    data <- cacheUpdate() %>%
      select(
        any_of(
          c("workflow_name", "workflow_id", "callName", "shardIndex", "executionStatus")),
        everything()
      ) %>%
      unique(),
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
        nrow(cacheUpdate() %>% filter(as.logical(callCaching.hit)))
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
        nrow(cacheUpdate() %>% filter(!as.logical(callCaching.hit)))
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
      FOCUS_ID <<- input$selectedWorkflowId 
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
