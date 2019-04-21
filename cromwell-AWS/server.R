library(shiny); library(shinydashboard);
library(data.table); library(DT);
library(tgR); library(tidyverse); 
library(RColorBrewer) 

focusID <- 1
server <- function(input, output, session) { 

###### Cromwell/AWS Batch
  observe({
    daysOfHistory <- input$daysToShow
  })
  
  workflowUpdate <- eventReactive(input$trackingUpdate, {
    cromTable <- cromwellJobs(days = daysOfHistory)
    print("cromwellWorkflow(); Querying cromwell for workflows(s).")
    workflowDat <- purrr::map_dfr(cromTable$workflow_id, function(x) cromwellWorkflow(x))
    cat("the value of trackingUpdate is", input$trackingUpdate)
    workflowDat
  }, ignoreNULL = FALSE)
  

  output$workflowStatus <- renderPlot({
    ggplot(workflowUpdate(), aes(workflowName, fill=status)) + 
      geom_bar(position = "stack") + 
      theme_minimal() + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      scale_fill_brewer(palette = "BuPu") + ylab("Number of Workflows") +
      xlab("Workflow Name")
    
  })
  # output$workflowDuration <- renderPlot({
  #   ggplot(workflowUpdate(), aes(workflowName, as.numeric(workflowDuration))) +
  #     geom_point(aes(color = status)) +
  #     theme_minimal() +
  #     theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  #     scale_color_brewer(palette = "BuPu") + ylab("Workflow Duration (mins)") +
  #     xlab("Workflow Name")
  # 
  # })
  output$submittedBox <- renderInfoBox({
    infoBox(
      "Total \nSubmitted", nrow(workflowUpdate()), 
      icon = icon("list"),
      color = "purple"
    )
  })
  # output$successBox <- renderInfoBox({
  #   infoBox(
  #     "Success Rate", paste0(round(nrow(workflowUpdate()[workflowUpdate()$status == "Succeeded",])/nrow(workflowUpdate())*100, 0), " %"), 
  #     icon = icon("grin"),
  #     color = "yellow"
  #   )
  # })
  output$inprogressBox <- renderInfoBox({
    infoBox(
      "In Progress", nrow(workflowUpdate()[workflowUpdate()$status == "Running",]), 
      icon = icon("sync"),
      color = "green"
    )
  })
  
  output$joblistCromwell <- renderDT(
    data <- workflowUpdate() %>% select(one_of("workflow_id", "workflowName", "actualWorkflowLanguage", 
        "status", "submission", "workflowDuration", "submissionType", "user")) %>% arrange(desc(submission)),
    class = "compact",
    filter = "top",
    options = list(scrollX = TRUE), selection = "single")
  
  callsUpdate <- eventReactive(
    input$joblistCromwell_rows_selected,{
      data <- workflowUpdate() %>% select(one_of("workflow_id", "workflowName", "actualWorkflowLanguage", 
                                                 "status", "submission", "workflowDuration", "submissionType", "user")) %>% arrange(desc(submission))
      focusID <<- data[input$joblistCromwell_rows_selected,]$workflow_id
    print("cromwellCall(); Querying cromwell for metadata for calls.")
    print(input$joblistCromwell_rows_selected)
    callDat <<- cromwellCall(focusID)
    cat("the value of trackingUpdate is", input$trackingUpdate)
    callDat
  }, ignoreNULL = TRUE)
  
  failsUpdate <- eventReactive(input$joblistCromwell_rows_selected,{
    data <- workflowUpdate() %>% select(one_of("workflow_id", "workflowName", "actualWorkflowLanguage", 
                                               "status", "submission", "workflowDuration", "submissionType", "user")) %>% arrange(desc(submission))
    focusID <<- data[input$joblistCromwell_rows_selected,]$workflow_id
    print("cromwellFailures(); Querying cromwell for metadata for failures.")
    failDat <<- cromwellFailures(focusID)
    cat("the value of trackingUpdate is", input$trackingUpdate)
    failDat
  }, ignoreNULL = TRUE)
  

  output$pendingBatch <- renderValueBox({
    infoBox(
      "Pending",value = nrow(callsUpdate()  %>% filter(executionStatus %in% c("Starting"))), 
      icon = icon("clock"),
      color = "yellow"
    )
  })
  output$runningBatch <- renderInfoBox({
    infoBox(
      "Running", value = nrow(callsUpdate() %>% filter(executionStatus == "Running")),
      icon = icon("sync"),
      color = "teal"
    )
  })
  output$failedBatch <- renderInfoBox({
    infoBox(
      "Failed", value = nrow(callsUpdate() %>% filter(executionStatus == "Failed")), 
      icon = icon("thumbs-down"),
      color = "maroon"
    )
  })
  output$succeededBatch <- renderInfoBox({
    infoBox(
      "Succeeded", 
      value = nrow(callsUpdate() %>% filter(executionStatus == "Done")), 
      icon = icon("thumbs-up"),
      color = "green"
    )
  })

output$tasklistBatch <- renderDT(
  data <- callsUpdate() %>% select(one_of("callName", "jobId", "workflow_id", "executionStatus", "backendStatus", 
            "shardIndex", "docker", "maxRetries", "cpu", "noAddress", "memory", 
            "allowResultReuse", "hit", "result", "effectiveCallCachingMode",
            "attempt", "log", "start", "jobDuration", "dockerImageUsed", "cpuMin", "retryableFailure")) %>% unique() %>% arrange(desc(start)),
  class = "compact",
  filter = "top",
  options = list(scrollX = TRUE))



output$failurelistBatch <- renderDT(
  data <- failsUpdate() %>% 
    select(one_of("jobId", "callName" ,"workflow_id", "shardIndex", 'attempt', 
                  "failures.message", "failures.causedBy.message")) %>% 
    unique(), 
  class = "compact",
  filter = "top",
  options = list(scrollX = TRUE))
}

