


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


generateReport <-
  function(subsetId, input, userInfo, fileDest = NULL) {
    loginfo("inside report generation with subsetId: %s", subsetId)
    #needed in markdown
    author <- if (!is.empty(input$reportAuthor)) {
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
    addSubsetId <-
      ifelse(length(input$subsetReport) > 1, paste0("_", subsetId), "")
    # Use the user provided name or the default one if it doesn't exists
    
    outputFile <- if (is.null(fileDest)) {
      fileName <- ifelse(is.empty(input$reportFileName),
                         DEFAULT_FILE_NAME,
                         input$reportFileName)
      paste0(fileName, addSubsetId, ".docx")
    } else {
      basename(fileDest)
    }
    
    outputDir <-
      ifelse(is.null(fileDest), "output", dirname(fileDest))
    
    loginfo("output File name: %s and dir: %s", outputFile, outputDir)
    
    # testmd(Observation.df)
    
    rmarkdown::render("Shiny_cruise_report2.Rmd",
                      output_file = outputFile,
                      output_dir = outputDir)
    # list(name = outputFile, path = out, subset = subsetId)
  }

generateReports <-
  function(subsets, input, userInfo, fileDest = NULL) {
    lapply(subsets, generateReport, input, userInfo)
  }

compareReports <- function(input, output, session, userInfo) {
  
}


################
### Observers
###############

downloadZippedReports <- function(subsetIds, input, userInfo) {
  downloadHandler(
    filename = function() {
      outputFile <- paste0(DEFAULT_FILE_NAME, "s_", Sys.Date(), ".zip")
    },
    content = function(file) {
      out <- generateReports(subsetIds, input, userInfo)
      zip(zipfile = file,
          files = unlist(out),
          extras = "-j")
    },
    contentType = "application/zip, application/octet-stream"
  )
}


generateReportDownloadObservers <-
  function(input, output, userInfo) {
    print("generate download observers")
    # Iterate on each subset to generate a download handler
    lapply(getSubsets(userInfo), function(subset, input, output) {
      subsetId <- getId(subset)
      # get label and replace any space by underscore
      subsetLabel <- gsub("\\s", "_", getLabel(subset))
      output[[paste0("downloadReport", subsetId)]] <-
        downloadHandler(
          filename = function() {
            fileName <-
              ifelse(is.empty(input$reportFileName),
                     DEFAULT_FILE_NAME,
                     input$reportFileName)
            outputFile <-
              paste0(fileName, "_", subsetLabel, ".docx")
          },
          content = function(file) {
            generateReport(subsetId, input, userInfo, file)
          },
          contentType = "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
        )
    }, input, output)
    # Generate a handler to download them all
    output$downloadReportAll <-
      downloadZippedReports(getSubsets(userInfo), input, userInfo)
    output$downloadReportSelection <-
      downloadZippedReports(input$reportSubset, input, userInfo)
  }


createReportObserver <- function(input, output, session, userInfo) {
  observeEvent(input$showReportModal, {
    userInfo$reports <- NULL
    # Create observers to handle download
    generateReportDownloadObservers(input, output, userInfo)
    showModal(createReportModal(input, output, session, userInfo))
  })
  
  
  observeEvent(input$generateReport, {
    req(input$subsetReport)
    if (input$downloadManually) {
      generateReportDownloadObservers(input, output, userInfo)
    } else {
      # userInfo$reports <- if (input$reportType == TYPE_CREATE) {
      #   generateReports(input, output, session, userInfo)
      # } else {
      #   compareReports(input, output, session, userInfo)
      # }
    }
  })
  
  observeEvent(input$exitReportModal, {
    removeModal(session)
  })
  
}

##############
### Renders
#############


createReportRender <- function(input, output, session, userInfo) {
  output$createReport <- renderUI({
    tagList(div(
      class = "reportButtons",
      actionButton(
        class = "reportButton",
        "showReportModal",
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
    tagList(
      textInput(
        "reportAuthor",
        geti18nValue("report.author.name", userInfo$lang),
        placeholder = geti18nValue("report.author.name.placeholder", userInfo$lang)
      ),
      textInput(
        "reportFileName",
        geti18nValue("report.file.name", userInfo$lang),
        placeholder = geti18nValue("report.file.name.placeholder", userInfo$lang)
      ),
      fluidRow(style = "margin-bottom: 10px;",
        column(6, renderText(
          i18nText("report.download.manually", userInfo$lang)
        )),
        column(
          6,
          textOutput2(
            content = toupper(i18nText("text.or", userInfo$lang)),
            inline = TRUE,
            style = "font-weight: bold;"
          ),
          textOutput2(
            content = i18nText("report.select.download", userInfo$lang),
            inline = TRUE
          )
        )
      ),
      fluidRow(column(6,
                      uiOutput(
                        "reportDownloadList"
                      )),
               column(
                 6,
                 tagList(
                   selectizeInput(
                     "subsetReport",
                     geti18nValue("filter.choices.subset", userInfo$lang),
                     choices = subsetChoices,
                     multiple = TRUE
                   ),
                   downloadButton(
                     "downloadReportSelection",
                     geti18nValue("report.download.select", userInfo$lang)
                   )
                 )
               ))
    )
    
  })
  
  output$reportDownloadList <- renderUI({
    tagList(div(
      class = "downloadReportButtons",
      lapply(getSubsets(userInfo), function(subset) {
        downloadButton(paste0("downloadReport", getId(subset)),
                       getLabel(subset),
                       class = "downloadButton")
      }),
      downloadButton(
        "downloadReportAll",
        geti18nValue("report.download.all", userInfo$lang),
        class = "downloadButton"
      )
    ))
  })
  
  output$reportActionButtons <- renderUI({
    div(style = "text-align: right; margin-top:20px;",
        # actionButton(
        #   "generateReport",
        #   geti18nValue("report.generate", userInfo$lang)
        # ),
        actionButton(
          "exitReportModal",
          geti18nValue("button.exit", userInfo$lang)
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
      uiOutput("reportActionButtons")
    ),
    footer = NULL,
    easyClose = TRUE
  )
}
