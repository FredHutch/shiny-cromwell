## remotes::install_github('FredHutch/fh.wdlR') @v0.1.1')
library(shiny); library(shinydashboard);
library(data.table); library(DT); library(tidyverse);
library(RColorBrewer)
library(fh.wdlR)
# for rendering the About page:
library(markdown)

focusID <- 1

server <- function(input, output, session) {
  ###### Cromwell Submit tab ######
  ## Validate a possible workflow
  validateWorkflow <- eventReactive(input$validateWorkflow, {
    Sys.setenv("CROMWELLURL" = paste0("http://", input$submitCromwellURL))
    womtoolValidate(WDL = input$validatewdlFile$datapath, 
                    allInputs = input$validateinputFile$datapath)
    
  }, ignoreNULL = TRUE)
  ## Show the validation result in a box
  output$validationResult <- renderPrint(validateWorkflow())
  
  ## Submit a workflow
  submitWorkflowJob <- eventReactive(input$submitWorkflow, {
    Sys.setenv("CROMWELLURL" = paste0("http://", input$submitCromwellURL))
    cromwellSubmitBatch(WDL = input$wdlFile$datapath,
                        Params = input$inputJSON$datapath,
                        Batch = input$input2JSON$datapath,
                        Options = input$workOptions$datapath,
                        Labels = data.frame("workflowType" = "AppSubmission"))
  }, ignoreNULL = TRUE)
  ## Show the workflow submission result in a box
  output$submissionResult <- renderPrint(submitWorkflowJob())
  
  ## Troubleshoot a workflow
  troubleWorkflowJob <- eventReactive(input$troubleWorkflow, {
    Sys.setenv("CROMWELLURL" = paste0("http://", input$submitCromwellURL))
    cromwellGlob(workflow_id = input$troubleWorkflowID)
  }, ignoreNULL = TRUE)
  ## Show the abort workflow result in a box
  output$troubleResult <- renderPrint(troubleWorkflowJob())
  
  
  ## Abort a workflow
  abortWorkflowJob <- eventReactive(input$abortWorkflow, {
    Sys.setenv("CROMWELLURL" = paste0("http://", input$submitCromwellURL))
    cromwellAbort(workflow_id = input$abortWorkflowID)
  }, ignoreNULL = TRUE)
  ## Show the abort workflow result in a box
  output$abortResult <- renderPrint(abortWorkflowJob())
  
  
  
  
  ############ CROMWELL Tracking Tab  ############
  updateServer <- eventReactive(input$trackingUpdate, {
    Sys.setenv("CROMWELLURL" = paste0("http://", input$submitCromwellURL))
  }, ignoreNULL = FALSE)
  
  workflowUpdate <- eventReactive(input$trackingUpdate, {
    Sys.setenv("CROMWELLURL" = paste0("http://", input$currentCromwellURL))
    if(input$workName == ""){ 
      cromTable <- cromwellJobs(days = input$daysToShow, workflowStatus = input$workStatus)}
    else {
    cromTable <- cromwellJobs(days = input$daysToShow, workflowStatus = input$workStatus,
                              workflowName = input$workName)}
    print("workflowUpdate")
    if(nrow(cromTable) == 1 & is.na(cromTable$workflow_id[1]) == T){workflowDat <- cromTable } else {
      workflowDat <- purrr::map_dfr(cromTable$workflow_id, cromwellWorkflow) %>% arrange(desc(submission)) %>% 
        select(-c("workflow", "workflowUrl", "inputs", "metadataSource")) %>% 
        select("workflowName", "workflow_id", "status", "submission","start", 
               "end", "workflowDuration", "workflowRoot", everything()) 
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
        geom_boxplot(aes(fill = status)) + coord_flip() +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_fill_brewer(type = "qualitative", palette = "Set1") +
        ylab("Workflow Duration (hrs)") +
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
      print("cromwellCall(); Querying cromwell for metadata for calls.")
      print(input$joblistCromwell_rows_selected)
      theseCalls <- cromwellCall(focusID)
      if ("executionStatus" %in% colnames(theseCalls)) {
        callDat <<- theseCalls } else {
          callDat <<- theseCalls %>% mutate(executionStatus = "NA")
        }
      callDat %>% select(one_of("workflowName", "callName", "executionStatus", "shardIndex", "callRoot", "start", "end", "callDuration", "docker", "modules"), everything()) 
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
    print("cromwellFailures(); Querying cromwell for metadata for failures.")
    failDat <- cromwellFailures(focusID) %>%
      select(one_of("callName" ,"jobId", "workflow_id", "shardIndex", 'attempt',
                    "failures.message", "failures.causedBy.message")) %>% unique()
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
    print("cromwellCache(); Querying cromwell for metadata for call caching.")
    theseCache <- cromwellCache(focusID)
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
    print("cromwellOutputs(); Querying cromwell for a list of workflow outputs.")
    outDat <<- try(cromwellOutputs(focusID), silent = TRUE)
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

