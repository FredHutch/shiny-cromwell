#require(devtools)
#devtools::install_github('FredHutch/tgR@v0.1.1') # Minimum version v0.1.1
library(shiny); library(shinydashboard);
library(data.table); library(DT); library(tidyverse);
library(RColorBrewer)
library(fh.wdlR)

focusID <- 1

server <- function(input, output, session) {

###### Cromwell
  updateServer <- eventReactive(input$trackingUpdate, {
    Sys.setenv("CROMWELLURL" = paste0("http://", input$submitCromwellURL))
  }, ignoreNULL = FALSE)

  workflowUpdate <- eventReactive(input$trackingUpdate, {
    Sys.setenv("CROMWELLURL" = paste0("http://", input$currentCromwellURL))
    cromTable <- cromwellJobs(days = input$daysToShow)
    print("workflowUpdate")
    if(nrow(cromTable) == 1 & is.na(cromTable$workflow_id[1]) == T){workflowDat <- cromTable } else {
      workflowDat <- purrr::map_dfr(cromTable$workflow_id, cromwellWorkflow) %>% arrange(desc(submission)) %>%
        select(-c("workflow", "workflowUrl", "inputs")) # select(one_of("workflow_id", "workflowName", "actualWorkflowLanguage",
      #  "status", "submission", "workflowDuration"))
      cat("the value of cromwellUpdate is", input$cromwellUpdate)
    }
    workflowDat
  }, ignoreNULL = TRUE)


  output$workflowStatus <- renderPlot({
    if ("workflowName" %in% colnames(workflowUpdate())){
      ggplot(workflowUpdate(), aes(workflowName, fill=status)) +
        geom_bar(position = "stack") + coord_flip() +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        scale_fill_brewer(palette = "Spectral") + ylab("Number of Workflow Submissions") +
        xlab("Workflow Name")
    } else {
      ggplot() + geom_blank()
    }
  })

  callDurationUpdate <- eventReactive(input$trackingUpdate, {
    print("callDurationUpdate")
    if(nrow(workflowUpdate()) == 1 & is.na(workflowUpdate()$workflow_id[1]) == T) {
      callDuration <- data.frame("noCalls" = "No workflows with calls were submitted, please choose a different time period. ")
    } else {
      callDuration <- purrr::map_dfr(workflowUpdate()$workflow_id, cromwellCall) %>%
        dplyr::select(workflow_id, callName, executionStatus, callDuration, jobId)
    }

    callDuration
  }, ignoreNULL = TRUE)


  output$workflowDuration <- renderPlot({
    if ("workflowName" %in% colnames(workflowUpdate())){
      ggplot(workflowUpdate(), aes(workflowName, workflowDuration)) +
        geom_point(aes(fill = status)) + coord_flip() +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_fill_brewer(type = "qualitative", palette = "Set1") +
        ylab("Workflow Duration (hrs)") +
        xlab("Workflow Name")
    } else {ggplot() + geom_blank()}
  })

  output$submittedBox <- renderInfoBox({
    infoBox(
      "Submitted", workflowUpdate() %>% filter(is.na(workflow_id) == F) %>% summarize(n_distinct(workflow_id)),
      icon = icon("paper-plane"),
      color = "purple", width = 3
    )
  })
  output$successBox <- renderInfoBox({
    infoBox(
      "Successful", if(is.na(workflowUpdate()$workflow_id[1]) == T) {0} else {
        workflowUpdate() %>% filter(status == "Succeeded") %>% summarise(n_distinct(workflow_id))},
      icon = icon("grin"),
      color = "yellow", width = 3
    )
  })
  output$failBox <- renderInfoBox({
    infoBox(
      "Failed", if(is.na(workflowUpdate()$workflow_id[1]) == T) {0} else {
        workflowUpdate() %>% filter(status == "Failed") %>% summarise(n_distinct(workflow_id))},
      icon = icon("sad-tear"),
      color = "red", width = 3
    )
  })
  output$inprogressBox <- renderInfoBox({
    infoBox(
      "In Progress", if(is.na(workflowUpdate()$workflow_id[1]) == T) {0} else {
        workflowUpdate() %>% filter(status == "Running") %>% summarise(n_distinct(workflow_id))},
      icon = icon("sync"),
      color = "green", width = 3
    )
  })

  output$joblistCromwell <- renderDT(
    data <- workflowUpdate(),
    class = "compact",
    filter = "top",
    options = list(scrollX = TRUE), selection = "single", rownames= FALSE)


  #### Call Data
  callsUpdate <- eventReactive(
    input$joblistCromwell_rows_selected,{
      data <- workflowUpdate()
      focusID <<- data[input$joblistCromwell_rows_selected,]$workflow_id
      print("cromwellCall(); Querying cromwell for metadata for calls.")
      print(input$joblistCromwell_rows_selected)
      theseCalls <- cromwellCall(focusID)
      if ("executionStatus" %in% colnames(theseCalls)) {
        callDat <<- theseCalls } else {
          callDat <<- theseCalls %>% mutate(executionStatus = "NA")
        }
      cat("the value of tracking update is", input$trackingupdate)
      callDat
    }, ignoreNULL = TRUE)

  output$workflowTiming <- renderPlot({
    if ("callName" %in% colnames(callsUpdate())){
      ggplot(callsUpdate(), aes(callName, callDuration)) +
        geom_boxplot(aes(fill = executionStatus)) + coord_flip() +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_fill_brewer(type = "qualitative", palette = "Set1") +
        ylab("Call Duration (mins)") +
        xlab("Call Name")
    } else {ggplot() + geom_blank()}
  })

  output$pendingBatch <- renderValueBox({
    infoBox(
      "Pending",value = nrow(callsUpdate()  %>% filter(executionStatus %in% c("Starting"))),
      icon = icon("clock"),
      color = "yellow", width = 6
    )
  })
  output$runningBatch <- renderInfoBox({
    infoBox(
      "Running", value = nrow(callsUpdate() %>% filter(executionStatus == "Running")),
      icon = icon("sync"),
      color = "teal", width = 6
    )
  })
  output$failedBatch <- renderInfoBox({
    infoBox(
      "Failed", value = nrow(callsUpdate() %>% filter(executionStatus == "Failed")),
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
    rownames= FALSE)

  output$downloadJobs<- downloadHandler(
    filename = function() {
      paste0(unique(callsUpdate()$workflow_id), "-workflowJobData.csv")
    },
    content = function(file) {
      write.csv(callsUpdate(), file, row.names = FALSE)
    }
  )


  ## Failure data
  failsUpdate <- eventReactive(input$joblistCromwell_rows_selected,{
    data <- workflowUpdate()
    focusID <<- data[input$joblistCromwell_rows_selected,]$workflow_id
    print("cromwellFailures(); Querying cromwell for metadata for failures.")
    failDat <<- cromwellFailures(focusID) %>%
      select(one_of("callName" ,"jobId", "workflow_id", "shardIndex", 'attempt',
                    "failures.message", "failures.causedBy.message")) %>% unique()
    cat("the value of trackingupdate is", input$trackingupdate)
    failDat
  }, ignoreNULL = TRUE)

  output$failurelistBatch <- renderDT(
    data <- failsUpdate(),
    class = "compact",
    filter = "top",
    options = list(scrollX = TRUE),
    rownames= FALSE)

  output$downloadFails<- downloadHandler(
    filename = function() {
      paste0(unique(failsUpdate()$workflow_id), "-callFailureData.csv")
    },
    content = function(file) {
      write.csv(failsUpdate(), file, row.names = FALSE)
    }
  )

  ### Call Caching data
  cacheUpdate <- eventReactive(input$joblistCromwell_rows_selected,{
    data <- workflowUpdate()
    focusID <<- data[input$joblistCromwell_rows_selected,]$workflow_id
    print("cromwellCache(); Querying cromwell for metadata for call caching.")
    theseCache <- cromwellCache(focusID)
    if ("callCaching.effectiveCallCachingMode" %in% colnames(theseCache)) {
      cacheDat <<- theseCache %>% filter(callCaching.effectiveCallCachingMode %in% c("ReadAndWriteCache", "WriteCache"))} else {
        cacheDat <<- theseCache %>% mutate(callCaching.effectiveCallCachingMode = "NA")
      }

    cat("the value of trackingupdate is", input$trackingupdate)
    cacheDat
  }, ignoreNULL = TRUE)

  output$cachingListBatch <- renderDT(
    data <- cacheUpdate() %>% unique(),
    class = "compact",
    filter = "top",
    options = list(scrollX = TRUE),
    rownames= FALSE)


  output$downloadCache <- downloadHandler(
    filename = function() {
      paste0(unique(cacheUpdate()$workflow_id), "-callCachingData.csv")
    },
    content = function(file) {
      write.csv(cacheUpdate(), file, row.names = FALSE)
    }
  )
  output$cacheHits <- renderInfoBox({
    infoBox(
      "Cache Hits", value = if("callCaching.hit" %in% colnames(cacheUpdate())) {nrow(cacheUpdate() %>% filter(callCaching.hit == TRUE))} else {0},
      icon = icon("grin-tongue"),
      color = "aqua", width = 6
    )
  })
  output$cacheMisses <- renderInfoBox({
    infoBox(
      "Cache Misses",
      value = if("callCaching.hit" %in% colnames(cacheUpdate())) {nrow(cacheUpdate() %>% filter(callCaching.hit == FALSE))} else {0},
      icon = icon("meh"),
      color = "orange", width = 6
    )
  })
}

