
REPORT_OUTPUT_DIR <- "output"


TYPE_COMPARE <- "compare"
TYPE_CREATE <- "create"

DEFAULT_AUTHOR_NAME <- "Author"
DEFAULT_FILE_NAME <- "report"

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


generateReport <- function(subsetId, input, userInfo) {
  #needed in markdown
  author <- if (!is.null(input$reportAuthor)) {
    input$reportAuthor
    } else {
      DEFAULT_AUTHOR_NAME
    }
  today <- Sys.Date() #needed in markdown
  
  subsetData <- getSubsetData(subsetId, userInfo, as.df = TRUE)
  if (is.null(subsetData)) {
    subset <- getSubset(subsetId, userInfo)
    subsetData <- filterData(subset, userInfo)
    if (!is.null(subsetData)) {
      subsetData <- subsetData@data
    }
  }
  
  Observation.df <- droplevels(subsetData)
  
  # If there is more than one to generate, suffix the subset id
  addSubsetId <- ifelse(length(input$subsetReport) > 1, paste0("_", subsetId), "")
  # Use the user provided name or the default one if it doesn't exists
  fileName <- ifelse(is.empty(input$reportFileName), DEFAULT_FILE_NAME, input$reportFileName)
  
  outputFile <- paste0(fileName, addSubsetId,".docx")
  
  
  loginfo("output File name: %s", outputFile)
  
  outputDir <- "output"
  
  # testmd(Observation.df)
  
  rmarkdown::render(
    "Shiny_cruise_report2.Rmd",
    output_file = outputFile,
    output_dir = outputDir
  )
  
}

generateReports <- function(input, output, session, userInfo) {
  lapply(input$subsetReport, generateReport, input, userInfo)
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
      choices = subsetChoices,
      multiple = TRUE
    ),
    textInput("reportAuthor", geti18nValue("report.author.name", userInfo$lang),
              placeholder = geti18nValue("report.author.name.placeholder", userInfo$lang)),
    textInput("reportFileName", geti18nValue("report.file.name", userInfo$lang),
              placeholder = geti18nValue("report.file.name.placeholder", userInfo$lang)),
    checkboxInput("askDownloadDest", geti18nValue("report.ask.dest", userInfo$lang)),
    fileInput("reportDownload", geti18nValue("report.save.dest", userInfo$lang))
    )
    
  })

  output$reportActionButtons <- renderUI({
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
      uiOutput("reportActionButtons")
    ),
    footer = NULL,
    easyClose = TRUE
  )
}

