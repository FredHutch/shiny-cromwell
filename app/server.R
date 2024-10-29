library(shiny)
library(shinyBS)
library(shinyjs)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyFeedback)
library(shinyWidgets)
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

library(httr)

library(proofr)
library(rcromwell)

library(cookies)
library(listviewer)
library(rclipboard)

library(ids)

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
SHINY_LOGGING <- as.logical(Sys.getenv("SHINY_LOG", FALSE))

# FIXME: maybe remove later, was running into some timeouts during testing
proofr::proof_timeout(sec = PROOF_TIMEOUT)

# sanitize errors - note that some actual errors will still happen
options(shiny.sanitize.errors = SANITIZE_ERRORS)

myCols <- RColorBrewer::brewer.pal(6, "RdYlBu")

server <- function(input, output, session) {
  if (SHINY_LOGGING) shinylogs::track_usage(storage_mode = shinylogs::store_null())

  session$allowReconnect(TRUE)

  # For the Help page
  output$gitHtml <- renderUI({
    div(
      tags$span(
        tags$b("Code:"),
        tags$a(
          "FredHutch/shiny-cromwell",
          href = glue("https://github.com/FredHutch/shiny-cromwell/tree/{COMMIT_BRANCH}"),
          target="_blank"
        )
      ),
      tags$br(),
      tags$span(
        tags$b("Built from:"),
        tags$a(
          glue("{substring(COMMIT_SHORT_SHA, 1, 7)}"),
          href = glue("https://github.com/FredHutch/shiny-cromwell/tree/{COMMIT_SHA}"),
          target="_blank"
        )
      ),
      tags$br(),
      tags$span(
        tags$b('Last built on:', style = "display:inline;"),
        tags$p(
          glue('Last built on: {stamp("Mar 1, 1999", quiet = TRUE)(ymd_hms(COMMIT_TIMESTAMP))}'),
          style = "display:inline;"
        )
      )
    )
  })

  rv <- reactiveValues(token = "", url = "", validateFilepath="", own = FALSE, user = "")

  rv_file <- reactiveValues(
    validatewdlFile_state = NULL,
    validateinputFile_state = NULL,
    wdlFile_state = NULL,
    inputJSON_state = NULL,
    input2JSON_state = NULL,
    workOptions_state = NULL,
    abortWorkflowID_state = NULL
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

  output$userName <- renderUI({
    the_name <- if (is.null(input$username)) {
      rv$user
    } else {
      input$username
    }
    if (nzchar(the_name)) {
      span(icon("user"), the_name)
    } else {
      span()
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
      shinyjs::hide("ownCromwell")
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

          # update records in cookies DB
          user_to_db(
            user = rv$user,
            token = to_base64(rv$token),
            url = to_base64(rv$url),
            drop_existing = TRUE
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

  observe({
    if (proof_loggedin_serverup(rv$url, rv$token)) {
      shinyjs::toggleState("cromwellStart",
        proof_status(token = rv$token)$jobStatus != "RUNNING"
      )
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

        # update records in cookies DB
        user_to_db(
          user = rv$user,
          token = to_base64(rv$token),
          url = "",
          drop_existing = TRUE
        )

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
        if (input$proof != "Server") return(NULL)
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
        dat <- cromwellProofStatusData()
        if (nzchar(tip)) {
          tags$span(
            bslib::tooltip(
              icon("question-circle"),
              tip,
              placement = "right"
            ),
            HTML(paste0(
              strong(glue("{name}: ")),
              purrr::flatten(dat)[[list_index]] %||% value_if_null
            ))
          )
        } else {
          tags$span(
            icon("question-circle"),
            HTML(paste0(
              strong(glue("{name}: ")),
              purrr::flatten(dat)[[list_index]] %||% value_if_null
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
          "Label" = ifelse(nzchar(isolate(input$labelValue)), isolate(input$labelValue), ids::adjective_animal(style = "Pascal")),
          "secondaryLabel" = ifelse(nzchar(isolate(input$seclabelValue)), isolate(input$seclabelValue), ids::adjective_animal(style = "Pascal"))
        ),
        url = rv$url,
        token = rv$token
      )
      shinyjs::disable("submitWorkflow")
      HTML(glue('
        <br>
        <ul>
          <li><strong>Workflow ID:</strong> {df$id}</li>
          <li><strong>Status:</strong> {df$status}</li>
        </ul>
      '))
    })
  })

  observe({
    shinyjs::toggleState("submitWorkflow",
      !rlang::is_empty(input$wdlFile$datapath)
    )
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
    shinyjs::disable("submitWorkflow")
  })

  ###### Troubleshoot tab ######
  observeEvent(input$selectedWorkflowId, {
    output$troubleResult <- renderPrint({
      stop_safe_loggedin_serverup(rv$url, rv$token, rv$own)
      cromwell_glob(
        workflow_id = input$selectedWorkflowId,
        url = rv$url,
        token = rv$token
      )
    })
  })

  ############ CROMWELL Tracking Tab  ############

  workflowUpdate <- eventReactive(input$trackingUpdate, {
      stop_safe_loggedin_serverup(rv$url, rv$token, rv$own)

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

  ### Links to various tabs from the welcome page
  observeEvent(input$linkToServerTab, {
    nav_select("proof", "Server")
  })
  observeEvent(input$linkToValidateTab, {
    nav_select("proof", "Validate")
  })
  observeEvent(input$linkToSubmitTab, {
    nav_select("proof", "Submit")
  })
  observeEvent(input$linkTrackingTab, {
    nav_select("proof", "Track workflows")
  })
  observeEvent(input$linkToWorkflowDetailsTab, {
    nav_select("proof", "Workflow Details")
  })
  observeEvent(input$linkToHelpTab, {
    nav_select("proof", "Help")
  })

  ### go back to tracking tab from details tab
  observeEvent(input$linkToTrackingTab_from_workflow_inputs, {
    nav_select("proof", "Track workflows")
  })
  observeEvent(input$linkToTrackingTab_from_mermaid, {
    nav_select("proof", "Track workflows")
  })

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

  ## Compute totals for each Cromwell status
  is_workflow_empty <- function() {
    NROW(workflowUpdate()) == 0 || NCOL(workflowUpdate()) == 1
  }
  status_text <- function(status) {
    if (is_workflow_empty()) {
      0
    } else {
      df <- workflowUpdate()
      if (status == "Submitted") {
        df <- filter(df, !is.na(workflow_id))
      } else {
        # !! needed to get the value of the variable
        df <- filter(df, status == !!status)
      }
      df %>%
        summarize(n_distinct(workflow_id)) %>%
        pull(1)
    }
  }
  submittedText <- reactive({ status_text(status = "Submitted") })
  succeededText <- reactive({ status_text(status = "Succeeded") })
  pendingText <- reactive({ status_text(status = "Pending") })
  failedText <- reactive({ status_text(status = "Failed") })
  runningText <- reactive({ status_text(status = "Running") })
  abortedText <- reactive({ status_text(status = "Aborted") })

  output$trackingSummaryStats <- renderUI({
    tagList(
      tags$span(paste("Submitted: ", submittedText()), class = "text-primary fw-bold", style = "display:inline"),
      HTML("&nbsp;-&nbsp;"),
      tags$span(paste("Pending: ", pendingText()), class = "text-info fw-bold", style = "display:inline"),
      HTML("&nbsp;-&nbsp;"),
      tags$span(paste("Running: ", runningText()), class = "text-warning fw-bold", style = "display:inline"),
      HTML("&nbsp;-&nbsp;"),
      tags$span(paste("Succeeded: ", succeededText()), class = "text-success fw-bold", style = "display:inline"),
      HTML("&nbsp;-&nbsp;"),
      tags$span(paste("Failed: ", failedText()), class = "text-danger fw-bold", style = "display:inline"),
      HTML("&nbsp;-&nbsp;"),
      tags$span(paste("Aborted: ", abortedText()), class = "text-secondary fw-bold", style = "display:inline")
    )
  })

  # Data for cards out of workflowUpdate data
  workflowDetailsId <- function(workflow_id) {
    paste0("goToWorkflowDetails-", workflow_id)
  }

  output$workflows_cards <- renderUI({
    dflst <- apply(workflowUpdate(), 1, as.list)
    dat <- lapply(dflst, function(w) {
      list(
       data = w,
       card = card(
          id = glue("job_card_{w$workflow_id}"),
          class = "border border-secondary",
          card_header(
            div(
              span(bsicons::bs_icon("person-badge"), w$workflow_name),
              span("(", bsicons::bs_icon("tag-fill"), w$Label),
              span(bsicons::bs_icon("tag"), w$secondaryLabel, ")")
            ),
            proofLoadingButton(
              inputId = workflowDetailsId(w$workflow_id),
              label = "Workflow Details",
              class = "btn btn-secondary btn-sm",
              onclick = glue('
                Shiny.setInputValue(\"selectedWorkflowId\", \"{w$workflow_id}\");
                Shiny.setInputValue(\"selectedWorkflowName\", \"{w$workflow_name}\");
                Shiny.setInputValue(\"selectedWorkflowLabel\", \"{w$Label}\");
                Shiny.setInputValue(\"selectedWorkflowSecLabel\", \"{w$secondaryLabel}\")
              ')
            ),
            class = "d-flex justify-content-between gap-1",
          ),
          card_body(
            class = "d-flex align-items-left justify-content-between gap-1",
            fillable = FALSE,
            tags$span(
              w$status,
              class = glue("text-{card_header_color(w$status)} fw-bold")
            ),
            span(bsicons::bs_icon("send"), w$submission),
            span(bsicons::bs_icon("clock-history"), w$workflowDuration)
          ),
          card_body(
            class = "d-flex justify-content-between gap-1",
            fillable = FALSE,
            w$workflow_id,
            actionButton(
              inputId = "abortWorkflow",
              label = "Abort Workflow",
              icon = icon("eject"),
              class = "btn-sm",
              onclick = glue('Shiny.setInputValue(\"selectedWorkflowId\", \"{w$workflow_id}\")'),
              disabled = !w$status %in% c("Submitted", "Running")
            )
          )
        )
      )
    })
    # Filter by date
    dat <- Filter(\(w) {
      parse_date_tz(w$data$submission) >= parse_date_tz(paste(input$runs_date[1], "00:00:00")) &&
      parse_date_tz(w$data$submission) <= parse_date_tz(paste(input$runs_date[2], "23:59:00"))
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

  ## Abort a workflow with the abort button on each card
  observeEvent(input$abortWorkflow, {
    validate_workflowid(isolate(input$selectedWorkflowId))
    stop_safe_loggedin_serverup(rv$url, rv$token, rv$own)
    aborted <- tryCatch(
      {
        cromwell_abort(
          workflow_id = isolate(input$selectedWorkflowId),
          url = rv$url,
          token = rv$token
        )
      },
      error = function(e) e
    )
    if (rlang::is_error(aborted)) {
      print(glue("In Abort Workflow button: {aborted$message}"))
      default_abort_msg <- "Try refreshing data"
      msg <- aborted$message
      if (grepl("404", msg)) {
        msg <- "Not found"
      } else {
        msg <- default_abort_msg
      }
    } else {
      msg <- "Workflow aborted!"
    }
    updateActionButton(
      inputId = "abortWorkflow",
      label = msg,
      icon = icon("eject")
    )
  })

  reactive_buttons <- reactive({
    button_ids <- names(input)
    button_ids[grepl("goToWorkflowDetails", button_ids)]
  })

  observe({
    matching_ids <- reactive_buttons()
    lapply(matching_ids, function(id) {
      observeEvent(input[[id]], {
        nav_select("proof", "Workflow Details")
        shinyFeedback::resetLoadingButton(id)
      })
    })
  })

  ## reset trouble
  observeEvent(input$resetTrackingFilters, {
    reset_inputs("workName")
    reset_inputs("workStatus")
    reset_inputs("runs_date")
  })

  output$selectedWorkflowUI <- renderUI({
    if (!is.null(input$selectedWorkflowId)) {
      htmltools::tagList(
        htmltools::tags$span(
          h4(input$selectedWorkflowName, style = "display:inline"),
          h5(
            " (",
            span(bsicons::bs_icon("tag-fill"), input$selectedWorkflowLabel),
            span(bsicons::bs_icon("tag"), input$selectedWorkflowSecLabel),
            ")",
            style = "display:inline"
          )
        ),
        htmltools::tags$div(
          rclipButton(
            inputId = "clipbtn",
            label = "",
            clipText = input$selectedWorkflowId,
            icon = icon("clipboard"),
            tooltip = "Copy workflow ID",
            placement = "left",
            options = list(delay = list(show = 800, hide = 100), trigger = "hover"),
            class = "btn-secondary btn-sm"
          ),
          input$selectedWorkflowId
        )
      )
    }
  })

  ## Get a table of workflow labels
  workflowLabels <- eventReactive(input$selectedWorkflowId, {
    print("find Labels")
    workflow <- cromwell_workflow(
      workflow_id = input$selectedWorkflowId,
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

  output$workflowDescribe <- renderUI({
    wl <- purrr::discard_at(workflowLabels(), c("workflow", "inputs"))
    workflowLabelsLst <- lapply(wl, as.list)
    tags$ul(
      Map(function(x, y) {
        # print(y[[1]])
        tags$li(
          span(
            strong(x)
          ),
          ifelse(grepl("clipbtn", as.character(y[[1]])), HTML(y[[1]]), y)
        )
      }, names(workflowLabelsLst), unname(workflowLabelsLst))
    )
  })

  ## Workflow options
  workflowOptions <- eventReactive(input$selectedWorkflowId, {
    as.data.frame(jsonlite::fromJSON(
      cromwell_workflow(input$selectedWorkflowId,
        url = rv$url,
        token = rv$token
      )$options
    ))
  })

  output$workflowOpt <- renderUI({
    if (NROW(workflowOptions()) > 0) {
      renderDT(
        expr = workflowOptions(),
        class = "compact",
        filter = "top",
        options = list(scrollX = TRUE),
        selection = "single",
        rownames = FALSE
      )
    } else {
      div(
        "No options data found",
        class = "alert alert-primary",
        role = "alert"
      )
    }
  })

  ## Get a table of workflow inputs
  workflowInputs <- eventReactive(input$selectedWorkflowId, {
    cromwell_workflow(
      workflow_id = input$selectedWorkflowId,
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
  callsUpdate <- eventReactive(input$selectedWorkflowId,
    {
      theseCalls <- cromwell_call(
        workflow_id = input$selectedWorkflowId,
        url = rv$url,
        token = rv$token
      )
      if ("executionStatus" %in% colnames(theseCalls)) {
        callDat <<- theseCalls
      } else {
        callDat <<- theseCalls %>% mutate(executionStatus = "NA")
      }
      suppressWarnings(
        callDat %>%
        select(
          one_of("workflow_name", "detailedSubName", "callName",
            "executionStatus", "shardIndex", "callRoot", "start",
            "end", "callDuration", "docker", "modules"),
          everything()
        )
      )
    },
    ignoreNULL = TRUE
  )

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
      suppressWarnings(cromwell_failures(
        workflow_id = input$selectedWorkflowId,
        url = rv$url,
        token = rv$token
      ) %>%
        select(one_of(
          "callName", "jobId", "workflow_id", "detailedSubName", "shardIndex", "attempt",
          "failures.message", "failures.causedBy.message"
        ), everything()) %>%
        unique())
    },
    ignoreNULL = TRUE
  )

  output$failurelistBatch <- renderDT(
    expr = failsUpdate(),
    class = "compact",
    filter = "top",
    options = list(scrollX = TRUE),
    rownames = FALSE
  )

  output$failurelistBatch <- renderUI({
    if (NROW(failsUpdate()) > 0) {
      renderDT(
        expr = failsUpdate(),
        class = "compact",
        filter = "top",
        options = list(scrollX = TRUE),
        rownames = FALSE
      )
    } else {
      alert("No failures data found")
    }
  })

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
      theseCache <- cromwell_cache(
        workflow_id = input$selectedWorkflowId,
        url = rv$url,
        token = rv$token
      )
      if ("callCaching.effectiveCallCachingMode" %in% colnames(theseCache)) {
        theseCache
      } else {
        theseCache %>%
          mutate(callCaching.effectiveCallCachingMode = "NA")
      }
    },
    ignoreNULL = TRUE
  )

  output$cachingListBatch <- renderUI({
    if (NROW(cacheUpdate()) > 0) {
      renderDT(
        expr = cacheUpdate() %>%
          select(
            any_of(
              c("workflow_name", "workflow_id", "callName",
                "shardIndex", "executionStatus")),
            everything()
          ) %>%
          unique(),
        class = "compact",
        filter = "top",
        options = list(scrollX = TRUE),
        rownames = FALSE
      )
    } else {
      alert("No call caching data found")
    }
  })

  output$downloadCache <- downloadHandler(
    filename = function() {
      paste0(unique(cacheUpdate()$workflow_id), "-callCachingData.csv")
    },
    content = function(file) {
      write.csv(cacheUpdate(), file, row.names = FALSE)
    }
  )

  ## Outputs Data
  ### Go get the output data for the selected workflow
  outputsUpdate <- eventReactive(input$getOutputData,
    {
      cromwell_outputs(
        workflow_id = input$selectedWorkflowId,
        url = rv$url,
        token = rv$token
      )
    },
    ignoreNULL = TRUE
  )
  ## render outputs list to a table
  output$outputslistBatch <- renderUI({
    if (NROW(outputsUpdate()) > 0) {
      renderDT(
        expr = outputsUpdate(),
        class = "compact",
        filter = "top",
        options = list(scrollX = TRUE),
        rownames = FALSE
      )
    } else {
      alert("No output data found")
    }
  })
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
