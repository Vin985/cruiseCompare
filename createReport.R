
REPORT_OUTPUT_DIR <- "output"


TYPE_COMPARE <- "compare"
TYPE_CREATE <- "create"

###################
### Initialize
##################


createReport <- function(input, output, session, userInfo) {
  createReportObserver(input, output, session, userInfo)
  createReportRender(input, output, session, userInfo)
}


############
### Utils
###########


generateReport <- function(subsetId, userInfo) {
  author <- "Author" #needed in markdown
  today <- Sys.Date() #needed in markdown
  
  browser()
  subsetData <- getSubsetData(subsetId, userInfo, as.df = TRUE)
  if (is.null(subsetData)) {
    subset <- getSubset(subsetId, userInfo)
    subsetData <- filterData(subset, userInfo)
    if (!is.null(subsetData)) {
      subsetData <- subsetData@data
    }
  }
  
  Observation.df <- droplevels(subsetData)
  
  outputFile <- paste0("report_", subsetId,".docx")
  outputDir <- "output"
  
  # testmd(Observation.df)
  
  rmarkdown::render(
    "Shiny_cruise_report2.Rmd",
    output_file = outputFile,
    output_dir = outputDir
  )
  
}

generateReports <- function(input, output, session, userInfo) {
  lapply(input$subsetReport, generateReport, userInfo)
}

compareReports <- function(input, output, session, userInfo) {
  
}

################
### Observers
###############


createReportObserver <- function(input, output, session, userInfo) {
  observeEvent(input$createReport, {
    showModal(createReportModal(input, output, session, userInfo))
  })
  
  
  ## Create a new subset
  observeEvent(input$generateReport, {
    if (input$reportType == TYPE_CREATE) {
      generateReports(input, output, session, userInfo)
    } else {
      compareReports(input, output, session, userInfo)
    }
    removeModal(session)
  })
  
  observeEvent(input$reportType, {loginfo("type")})
  
}

##############
### Renders
#############


createReportRender <- function(input, output, session, userInfo) {
  output$reportButtons <- renderUI({
    tagList(div(
      class = "reportButtons",
      actionButton(
        class = "reportButton",
        "createReport",
        geti18nValue("create.report", userInfo$lang)
      )
    ))
  })
  
  output$reportOptions <- renderUI({
    if (input$reportType == TYPE_CREATE) {
      uiOutput("createReportOptions")
    } else {
      uiOutput("compareReportOptions")
    }
  })
  
  
  output$createReportOptions <- renderUI({
    loginfo("Displaying list of subsets...")
    isolate({
      # List all subsets
      subsets <- getSubsets(userInfo)
      subsetChoices <- names(subsets)
      names(subsetChoices) <- getSubsetsLabels(subsets)
      
    })
    tagList(selectizeInput(
      "subsetReport",
      geti18nValue("filter.choices.subset", userInfo$lang),
      choices = subsetChoices
    ))
    
  })

  
  output$compareReportOptions <- renderUI({
    
  })
  
}


createReportModal <- function(input, output, session, userInfo) {
  reportType <- list(TYPE_CREATE, TYPE_COMPARE)
  names(reportType) <-
    c(
      geti18nValue("report.type.create", userInfo$lang),
      geti18nValue("report.type.compare", userInfo$lang)
    )
  modalDialog(
    tagList(
      radioButtons(
        "reportType",
        label = geti18nValue("report.type.label", userInfo$lang),
        choices = reportType,
        inline = TRUE
      ),
      uiOutput("reportOptions"),
      div(
        style = "text-align: right;",
        actionButton(
          "generateReport",
          geti18nValue("report.generate", userInfo$lang)
        ),
        actionButton(
          "cancelSubset",
          geti18nValue("button.cancel", userInfo$lang)
        )
      )
    ),
    footer = NULL,
    easyClose = TRUE
  )
}

