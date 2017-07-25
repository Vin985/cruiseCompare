
TYPE_COMPARE <- "compare"
TYPE_CREATE <- "create"

DEFAULT_AUTHOR_NAME <- "Author"
DEFAULT_FILE_NAME <- "report"
DEFAULT_COMPARE_FILE_NAME <- "compareReport"

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


    models <- generateDensityModel(subsetId, input, userInfo)

    # If there is more than one to generate, suffix the subset id
    addSubsetId <-
      ifelse(length(input$subsetReport) > 1, paste0("_", subsetId), "")

    # Gridsize for use in report
    gridSize <- isolate(input$gridSize)
    if (is.null(gridSize)) {
      gridSize <- DEFAULT_GRIDSIZE
    }

    # Use the user provided name or the default one if it doesn't exists
    outputFile <- if (is.null(fileDest)) {
      fileName <- ifelse(is.empty(input$reportFileName),
                         DEFAULT_FILE_NAME,
                         input$reportFileName)
      paste0(fileName, addSubsetId, ".docx")
    } else {
      basename(fileDest)
    }


    # Data for use in report
    reportData <- list(models = models, gridSize = gridSize, subsets = subsetId)

    outputDir <-
      ifelse(is.null(fileDest), REPORT_OUTPUT_DIR, dirname(fileDest))

    loginfo("output File name: %s and dir: %s", outputFile, outputDir)


    rmarkdown::render(
      file.path(REPORTS_DIR, sprintf("cruise_report_%s.Rmd", userInfo$lang)),
      output_file = outputFile,
      output_dir = outputDir,
      params = list(lang = userInfo$lang),
      encoding = "UTF-8"
    )


  }

generateReports <-
  function(subsets, input, userInfo, fileDest = NULL) {
    lapply(subsets, generateReport, input, userInfo)
  }

generateCompareReport <- function(input, userInfo, fileDest = NULL) {
  loginfo("inside comparison report generation")
  #needed in markdown
  author <- if (!is.empty(input$reportAuthor)) {
    input$reportAuthor
  } else {
    DEFAULT_AUTHOR_NAME
  }

  sub1 <- input$selectCompareSubsetReport1
  sub2 <- input$selectCompareSubsetReport2

  compareModel <- compareModels(c(sub1, sub2), input, userInfo)

  # Gridsize for use in report
  gridSize <- isolate(input$gridSize)
  if (is.null(gridSize)) {
    gridSize <- DEFAULT_GRIDSIZE
  }

  # Data for use in report
  reportData <- list(comparison = compareModel, gridSize = gridSize)


  # Use the user provided name or the default one if it doesn't exists
  outputFile <- if (is.null(fileDest)) {
    fileName <- ifelse(is.empty(input$reportFileName),
                       DEFAULT_COMPARE_FILE_NAME,
                       input$reportFileName)
    paste0(fileName, ".docx")
  } else {
    basename(fileDest)
  }

  outputDir <-
    ifelse(is.null(fileDest), REPORT_OUTPUT_DIR, dirname(fileDest))

  loginfo("output File name: %s and dir: %s", outputFile, outputDir)


  rmarkdown::render(
    file.path(REPORTS_DIR, sprintf("compare_report_%s.Rmd", userInfo$lang)),
    output_file = outputFile,
    output_dir = outputDir,
    params = list(lang = userInfo$lang),
    encoding = "UTF-8"
  )

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


  observeEvent(input$exitReportModal, {
    removeModal(session)
  })

}

##############
### Renders
#############


createReportRender <- function(input, output, session, userInfo) {
  output$reportOptions <- renderUI({
    tagList(uiOutput("commonReportOptions"),
            if (input$reportType == TYPE_CREATE) {
              uiOutput("createReportOptions")
            } else {
              uiOutput("compareReportOptions")
            })
  })

  output$commonReportOptions <- renderUI({
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
      fluidRow(column(10, div(class = "gridSize", style = "margin-bottom:20px;",
                              numericInput("densityGridSize",
                                           label = geti18nValue("grid.size", userInfo$lang),
                                           value = DEFAULT_GRIDSIZE),
                              span("km")),
                      i18nTextOutput("warning.grid.size", userInfo$lang,
                                     style = "color: blue; font-size: 12px;")))
    )
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
      fluidRow(
        style = "margin-bottom: 10px;",
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
        actionButton(
          "exitReportModal",
          geti18nValue("button.exit", userInfo$lang)
        ))
  })

  ## Download Handler for report comparison
  output$downloadCompareReport <-
    downloadHandler(
      filename = function() {
        fileName <-
          ifelse(is.empty(input$reportFileName),
                 DEFAULT_COMPARE_FILE_NAME,
                 input$reportFileName)
        outputFile <- paste0(fileName, ".docx")
      },
      content = function(file) {
        generateCompareReport(input, userInfo, file)
      },
      # contentType = "application/pdf"
      contentType = "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
    )


  output$compareReportOptions <- renderUI({
    # List all subsets
    subsets <- getSubsets(userInfo, isolate = FALSE)
    subsetChoices <- names(subsets)
    names(subsetChoices) <- getSubsetsLabels(subsets)

    validate(need((length(subsetChoices) > 1),
                  geti18nValue("need.more.subsets", userInfo$lang)
    )
    , errorClass = "error")

    tagList(
      fluidRow(
        class = "selectSubsetsCompareReport",
        selectSubsetCompare(1, subsetChoices,
                            input, output, userInfo, "Report"),
        selectSubsetCompare(2, subsetChoices,
                            input, output, userInfo, "Report")
      ),
      fluidRow(
        column(5, offset = 7, downloadButton("downloadCompareReport",
                                              geti18nValue("download.compare.report", userInfo$lang),
                                              class = "downloadButton"))
      )
    )
  })

}

selectSubsetCompareReport <-
  function(idx,
           subsetChoices,
           input,
           output,
           userInfo,
           report) {
    selectInputId <- paste0("selectCompareSubset", idx)
    infoOutputId <- paste0("subsetInfoCompare", idx)

    ## Output
    output[[infoOutputId]] <- renderUI({
      div(displaySubsetInfo(input[[selectInputId]], userInfo))
    })

    column(4,
           tagList(
             selectizeInput(
               selectInputId,
               geti18nValue(paste0("compare.choices.subset", idx), userInfo$lang),
               choices = subsetChoices,
               selected = subsetChoices[idx],
               options = list(maxItems = 1)
             ),
             uiOutput(infoOutputId)
           ))
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
      div(
        style = "margin-bottom : 20px;",
        labelWithHelp("create.report", userInfo$lang, textclass = "title2")
      ),
      div(
      radioButtons(
        "reportType",
        label = geti18nValue("report.type.label", userInfo$lang),
        choices = reportType,
        inline = TRUE
      )),
      uiOutput("reportOptions"),
      uiOutput("reportActionButtons")
    ),
    footer = NULL,
    easyClose = TRUE
  )
}
