## remotes::install_github('getwilds/rcromwell')')
library(shiny)
library(shinydashboard)
library(data.table)
library(DT)
library(tidyverse)
library(RColorBrewer)
library(rcromwell)
# for rendering the About page:
library(markdown)
library(shinyWidgets)
library(jsonlite)
library(lubridate)
focusID <- 1

my.cols <- brewer.pal(6, "RdYlBu")
server <- function(input, output, session) {
  observeEvent(input$getStarted, {
    inputSweetAlert(
      session = session, inputId = "cromwellURL", input = "text",
      title = "What's your server node name and port?",
      inputPlaceholder = "gizmot32:8000",
      btn_labels = "Submit"
    )
  })

  theBackends <- eventReactive(input$cromwellURL,
    {
      cromwell_config(paste0("http://", input$cromwellURL), verbose = FALSE)
      try(cromwell_backends()$supportedBackends, silent = TRUE)
    },
    ignoreNULL = TRUE
  )


  observeEvent(input$cromwellURL, {
    if (class(theBackends()) == "try-error") {
      sendSweetAlert(
        session = session,
        title = "Whoops, that didn't work! If your server has only been running for <1 min, wait and try to reconnect again. ",
        text = print(theBackends()),
        type = "error"
      )
    } else {
      sendSweetAlert(
        session = session,
        title = "Server address valid.",
        text = paste0(
          "Available backends: ",
          paste0(theBackends(), collapse = ", ")
        ),
        type = "success"
      )
    }
  })

  output$connectionResult <- renderText(
    {
      if (class(theBackends()) == "try-error") {
        print(theBackends())
      } else {
        paste0(
          "Server address valid! \n Available backends: ",
          paste0(theBackends(), collapse = ", ")
        )
      }
    },
    quoted = FALSE
  )
  ###### Cromwell Submit tab ######
  ## Validate a possible workflow
  validateWorkflow <- eventReactive(input$validateWorkflow,
    {
      cromwell_validate(
        wdl = input$validatewdlFile$datapath,
        all_inputs = input$validateinputFile$datapath
      )
    },
    ignoreNULL = TRUE
  )
  ## Show the validation result in a box
  output$validationResult <- renderPrint(validateWorkflow())

  ## Submit a workflow
  submitWorkflowJob <- eventReactive(input$submitWorkflow,
    {
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
      cromwell_glob(workflow_id = input$troubleWorkflowID)
    },
    ignoreNULL = TRUE
  )
  ## Show the abort workflow result in a box
  output$troubleResult <- renderPrint(troubleWorkflowJob())


  ## Abort a workflow
  abortWorkflowJob <- eventReactive(input$abortWorkflow,
    {
      cromwell_abort(workflow_id = input$abortWorkflowID)
    },
    ignoreNULL = TRUE
  )
  ## Show the abort workflow result in a box
  output$abortResult <- renderPrint(abortWorkflowJob())




  ############ CROMWELL Tracking Tab  ############

  workflowUpdate <- eventReactive(input$trackingUpdate,
    {
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
          "workflowName", "workflow_id", "status", "submission", "start",
          "end", "workflowDuration"
        ), everything())
      } else {
        workflowDat <- data.frame(
          workflowName = character(0), workflow_id = character(0),
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
    if ("workflowName" %in% colnames(workflowUpdate())) {
      ggplot(workflowUpdate(), aes(x = as.factor(workflowName), y = as.numeric(workflowDuration))) +
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
    if ("workflowName" %in% colnames(workflow)) {
      workflowDat <- workflow %>% select(-one_of("options", "workflow", "metadataSource", "inputs"))
    } else {
      workflowDat <<- workflow %>% mutate(workflowName = "NA")
    }
    suppressWarnings(workflowDat %>% select(one_of("workflowName", "workflowRoot", "submission", "start", "end", "status", "workflowDuration"), everything()))
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
      suppressWarnings(callDat %>% select(one_of("workflowName", "detailedSubName", "callName", "executionStatus", "shardIndex", "callRoot", "start", "end", "callDuration", "docker", "modules"), everything()))
    },
    ignoreNULL = TRUE
  )

  output$workflowTiming <- renderPlot({
    if ("callName" %in% colnames(callsUpdate())) {
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
    data <- cacheUpdate() %>% select(workflowName, workflow_id, callName, shardIndex, executionStatus, everything()) %>% unique(),
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
