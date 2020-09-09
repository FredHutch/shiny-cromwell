## remotes::install_github('FredHutch/fh.wdlR') @v0.1.1')
library(shiny); library(shinydashboard);
library(data.table); library(DT); library(tidyverse);
library(RColorBrewer)
library(fh.wdlR)
# for rendering the About page:
library(markdown)
library(shinyWidgets)
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

  theBackends <- eventReactive(input$cromwellURL, {
    try(cromwellBackends(cromURL = paste0("http://", input$cromwellURL))$supportedBackends, silent = TRUE)}, 
    ignoreNULL = TRUE)
 
  
  observeEvent(input$cromwellURL, {
    if(class(theBackends()) == "try-error") {
        sendSweetAlert(
            session = session,
            title = "Whoops, that didn't work!",
            text = print(theBackends()),
            type = "error")}
        else sendSweetAlert(
                session = session,
                title = "Server address valid.",
                text = paste0("Available backends: ",
                              paste0(theBackends(), collapse = ", ")),
                type = "success")
  })

  output$connectionResult <- renderText({
    if(class(theBackends()) == "try-error") print(theBackends())
       else paste0("Server address valid! \n Available backends: ", 
                   paste0(theBackends(), collapse = ", "))}, 
    quoted = FALSE)
  ###### Cromwell Submit tab ######
  ## Validate a possible workflow
  validateWorkflow <- eventReactive(input$validateWorkflow, {
    womtoolValidate(WDL = input$validatewdlFile$datapath, 
                    allInputs = input$validateinputFile$datapath, 
                    cromURL = paste0("http://", input$cromwellURL))
    
  }, ignoreNULL = TRUE)
  ## Show the validation result in a box
  output$validationResult <- renderPrint(validateWorkflow())
  
  ## Submit a workflow
  submitWorkflowJob <- eventReactive(input$submitWorkflow, {
    cromwellSubmitBatch(WDL = input$wdlFile$datapath,
                        Params = input$inputJSON$datapath,
                        Batch = input$input2JSON$datapath,
                        Options = input$workOptions$datapath,
                        Labels = data.frame("workflowType" = "AppSubmission"),
                        cromURL = paste0("http://", input$cromwellURL))
  }, ignoreNULL = TRUE)
  ## Show the workflow submission result in a box
  output$submissionResult <- renderPrint(submitWorkflowJob())
  
  ## Troubleshoot a workflow
  troubleWorkflowJob <- eventReactive(input$troubleWorkflow, {
    cromwellGlob(workflow_id = input$troubleWorkflowID,cromURL = paste0("http://", input$cromwellURL))
  }, ignoreNULL = TRUE)
  ## Show the abort workflow result in a box
  output$troubleResult <- renderPrint(troubleWorkflowJob())
  
  
  ## Abort a workflow
  abortWorkflowJob <- eventReactive(input$abortWorkflow, {
    cromwellAbort(workflow_id = input$abortWorkflowID, cromURL = paste0("http://", input$cromwellURL))
  }, ignoreNULL = TRUE)
  ## Show the abort workflow result in a box
  output$abortResult <- renderPrint(abortWorkflowJob())
  
  
  
  
  ############ CROMWELL Tracking Tab  ############

  workflowUpdate <- eventReactive(input$trackingUpdate, {
    if(input$workName == ""){ 
      cromTable <- cromwellJobs(days = input$daysToShow, workflowStatus = input$workStatus, 
                                cromURL = paste0("http://", input$cromwellURL))}
    else {
    cromTable <- cromwellJobs(days = input$daysToShow, workflowStatus = input$workStatus,
                              workflowName = input$workName,
                              cromURL = paste0("http://", input$cromwellURL))}
    print("workflowUpdate(); Requesting Crowmell Job info")
    # if(nrow(cromTable) == 1 & is.na(cromTable$workflow_id[1]) == T){workflowDat <- cromTable } else {
    #   workflowDat <- purrr::map_dfr(cromTable$workflow_id, cromwellWorkflow, cromURL = paste0("http://", input$cromwellURL)) %>% arrange(desc(submission)) %>%
    #     select(-c("workflow", "workflowUrl", "inputs", "metadataSource"))
    # }
    workflowDat <- cromTable %>% select("workflowName", "workflow_id", "status", "submission","start",
                                        "end", "workflowDuration",  everything())
    workflowDat
  }, ignoreNULL = TRUE)
  
  
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
      ggplot(workflowUpdate(), aes(x = as.factor(workflowName), y = workflowDuration)) +
        geom_jitter(aes(color = status), width = 0.05, size = 4) + coord_flip() +
        theme_minimal() + 
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_color_manual(values = my.cols) +
        ylab("Workflow Duration (mins)") +
        xlab("Workflow Name")
    } else {ggplot() + geom_blank()}
  })
  
  ## Render some info boxes
  output$submittedBox <- renderInfoBox({
    infoBox(
      "Total \nSubmitted", workflowUpdate() %>% filter(is.na(workflow_id) == F) %>% summarize(n_distinct(workflow_id)),
      icon = icon("list"),
      color = "purple", width = 3
    )
  })
  output$successBox <- renderInfoBox({
    infoBox(
      "Success Rate", paste0(round(nrow(workflowUpdate()[workflowUpdate()$status == "Succeeded",])/nrow(workflowUpdate())*100, 0), " %"),
      icon = icon("grin"),
      color = "yellow", width = 3
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
      "In Progress", if(is.na(workflowUpdate()$workflow_id[1]) == T) {0} else { workflowUpdate() %>% filter(status == "Running") %>% summarise(n_distinct(workflow_id))},
      icon = icon("sync"),
      color = "green", width = 3
    )
  })
  ## Render a list of jobs in a table for a workflow
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
      print("callsUpdate(); Querying cromwell for metadata for calls.")
      theseCalls <- cromwellCall(focusID,cromURL = paste0("http://", input$cromwellURL))
      if ("executionStatus" %in% colnames(theseCalls)) {
        callDat <<- theseCalls } else {
          callDat <<- theseCalls %>% mutate(executionStatus = "NA")
        }
      suppressWarnings(callDat %>% select(one_of("workflowName", "callName", "executionStatus", "shardIndex", "callRoot", "start", "end", "callDuration", "docker", "modules"), everything())) 
    }, ignoreNULL = TRUE)
  
  output$workflowTiming <- renderPlot({
    if ("callName" %in% colnames(callsUpdate())){
      ggplot(callsUpdate(), aes(x = as.factor(callName), y = callDuration)) +
        geom_jitter(aes(color = executionStatus), width = 0.1, size = 3) + #coord_flip() +
        theme_minimal() + 
        theme(axis.text.x = element_text( hjust = 1, angle = 25)) +
        scale_color_manual(values = my.cols) +
        ylab("Call Duration (mins)") +
        xlab("Call Name")
    } else {ggplot() + geom_blank()}
  })
  
  ## Render some info boxes
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
  failsUpdate <- eventReactive(input$getFailedData,{
    data <- workflowUpdate()
    focusID <- data[input$joblistCromwell_rows_selected,]$workflow_id
    print("failsUpdate(); Querying cromwell for metadata for failures.")
    suppressWarnings(failDat <- cromwellFailures(focusID, cromURL = paste0("http://", input$cromwellURL)) %>%
      select(one_of("callName" ,"jobId", "workflow_id", "shardIndex", 'attempt',
                    "failures.message", "failures.causedBy.message"), everything()) %>% unique())
    return(failDat)
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
  cacheUpdate <- eventReactive(input$getCacheData,{
    data <- workflowUpdate()
    focusID <<- data[input$joblistCromwell_rows_selected,]$workflow_id
    print("cacheUpdate(); Querying cromwell for metadata for call caching.")
    theseCache <- cromwellCache(focusID, cromURL = paste0("http://", input$cromwellURL))
    if ("callCaching.effectiveCallCachingMode" %in% colnames(theseCache)) {
      cacheDat <- theseCache %>% filter(callCaching.effectiveCallCachingMode %in% c("ReadAndWriteCache", "WriteCache"))} else {
        cacheDat <- theseCache %>% mutate(callCaching.effectiveCallCachingMode = "NA")
      }
    cacheDat
  }, ignoreNULL = TRUE)
  
  output$cachingListBatch <- renderDT(
    data <- cacheUpdate() %>% select(workflowName, workflow_id, callName, shardIndex, executionStatus, everything()) %>% unique(),
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
  ## Render some info boxes
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
  
  ## Outputs Data 
  ### Go get the output data for the selected workflow
  outputsUpdate <- eventReactive(input$getOutputData,{
    data <- workflowUpdate()
    focusID <<- data[input$joblistCromwell_rows_selected,]$workflow_id
    print("outputsUpdate(); Querying cromwell for a list of workflow outputs.")
    outDat <<- try(cromwellOutputs(focusID, cromURL = paste0("http://", input$cromwellURL)), silent = TRUE)
    if (is.data.frame(outDat)==F) {
      outDat <- data.frame("workflow_id" = "No outputs are available for this workflow yet.",  stringsAsFactors = F)
    } 
    outDat
    
  }, ignoreNULL = TRUE)
  ## render outputs list to a table
  output$outputslistBatch <- renderDT(
    data <- outputsUpdate(),
    class = "compact",
    filter = "top",
    options = list(scrollX = TRUE),
    rownames= FALSE)
  ## Prep outputs table for download
  output$downloadOutputs<- downloadHandler(
    filename = function() {
      paste0(unique(outputsUpdate()$workflow_id), "-workflowOutputData.csv")
    },
    content = function(file) {
      write.csv(outputsUpdate(), file, row.names = FALSE)
    }
  )
}

