




## Page Id
VIEW_DATA_PAGE <- "viewData"

DATA_TYPE_TOTAL <- "total"
DATA_TYPE_BIRDS <- "birds"
DATA_TYPE_RAW <- "raw"

DATA_TABLE <- "table"
DENSITY_MAP <- "density"

DATA_ACTION_VIEW <- "view"
DATA_ACTION_COMPARE <- "compare"

ROUND_COLUMNS <- list(
  birds = c("sd", "cv", "meanFlock"),
  total = c("WatchLenKm", "Densities")
)

getDTcolnames <- function(dt, lang) {
  cols <- colnames(dt)
  res <-
    unlist(lapply(cols, function(name, lang) {
      geti18nValue(paste0("dt.col.", name), lang)
    }, lang))
}

viewDataPage <- function(input, output, session, userInfo) {
  # Change Page
  observeEvent(input$selectionPageAction, {
    changePage(SELECTION_PAGE, userInfo)
  }, ignoreInit = TRUE)

  densityMap(input, output, session, userInfo)

  viewDT <- reactive({
    # Get a dependency on current subsetId
    subsetId  <- getCurrentSubsetId(userInfo, isolate = FALSE)
    type <- input$dataType
    if (type == DATA_TYPE_RAW) {
      dt <-
        getSubsetData(subsetId,
                      userInfo,
                      as.df = TRUE,
                      isolate = FALSE)
      cols <- colnames(dt)
    }
    else {
      dt <-
        getAnalyzedData(subsetId, userInfo, isolate = FALSE)[[input$dataType]]
      cols <- getDTcolnames(dt, userInfo$lang)
    }

    DT::datatable(
      dt,
      filter = "top",
      colnames = cols,
      extensions = c("Buttons", "ColReorder"),
      options = list(
        sScrollX = "100%",
        orderClasses = TRUE,
        lengthMenu = c(10, 20, 50, 100),
        pageLength = 10,
        dom = 'Bfrtip',
        buttons = I('colvis')
      )
    ) %>% formatRound(ROUND_COLUMNS[[input$dataType]])
  })

  output$dataTable <-  DT::renderDataTable(viewDT())

  viewDataUI(input, output, session, userInfo)
}


viewDataUI <- function(input, output, session, userInfo) {
  output$viewDataPage <- renderUI({
    tagList(column(
      12,
      div(class = "description viewDataDesc", i18nText("view.data.desc", userInfo$lang)),
      fluidRow(column(8, uiOutput("subsetInfo", style = "margin-bottom: 15px;")),
               column(4, uiOutput(
                 "selectDataAction"
               ))),
      fluidRow(uiOutput("dataContent")),
      fluidRow(uiOutput("viewDataActionButtons"))
    ))
  })

  # Information about the subset filters
  output$subsetInfo <- renderUI({
    action <- input$dataAction
    if (is.null(action) || action == DATA_ACTION_VIEW) {
      displaySubsetInfo(getCurrentSubsetId(userInfo, isolate = FALSE), userInfo)
    } else {
      uiOutput("selectCompareSubsets")
    }
  })

  # Action buttons: view data or compare subsets
  output$selectDataAction <- renderUI({
    # Only display if two or more subsets
    if (length(getSubsets(userInfo, isolate = FALSE)) > 1) {
      # types of action
      actionType <- list(DATA_ACTION_VIEW, DATA_ACTION_COMPARE)
      names(actionType) <-
        c(
          geti18nValue("data.view", userInfo$lang),
          geti18nValue("data.compare", userInfo$lang)
        )
      tagList(
        div(
          style = "margin-bottom : 20px;",
          labelWithHelp("view.select.action", userInfo$lang, textclass = "title2")
        ),
        div(
          class = "dataAction",
          radioButtons(
            "dataAction",
            label = NULL,
            choices = actionType,
            inline = TRUE
          )
        )
      )
    }
  })

  # Main content: chosse between view or compare data
  output$dataContent <- renderUI({
    # if just one subset, never compare
    if (length(getSubsets(userInfo)) < 2 |
        (!is.empty(input$dataAction) &&
         input$dataAction == DATA_ACTION_VIEW)) {
      uiOutput("viewData")
    } else {
      uiOutput("compareData")
    }
  })

  # View data
  output$viewData <- renderUI({
    # Choose between table or density map
    dataType <- list(DATA_TABLE, DENSITY_MAP)
    names(dataType) <-
      c(
        geti18nValue("data.show.table", userInfo$lang),
        geti18nValue("data.density.map", userInfo$lang)
      )
    tagList(div(
      class = "dataAction dataTabs",
      radioButtons(
        "showDataType",
        label = NULL,
        choices = dataType,
        inline = TRUE
      )
    ),
    uiOutput("showData"))
  })

  # Show data
  output$showData <- renderUI({
    if (input$showDataType == DATA_TABLE) {
      uiOutput("showDataTable")
    } else {
      loginfo("show data UI")
      uiOutput("showDensityMap")
    }
  })

  # Show observations table
  output$showDataTable <- renderUI({
    # Select type of data to display
    dataType <-
      list(DATA_TYPE_TOTAL, DATA_TYPE_BIRDS, DATA_TYPE_RAW)
    names(dataType) <-
      c(
        geti18nValue("data.view.total", userInfo$lang),
        geti18nValue("data.view.birds", userInfo$lang),
        geti18nValue("data.view.raw", userInfo$lang)
      )
    tagList(
      labelWithHelp("view.data.dataset", userInfo$lang, textclass = "title2"),
      radioButtons(
        "dataType",
        label = geti18nValue("data.view.label", userInfo$lang),
        choices = dataType,
        inline = TRUE
      ),
      dataTableOutput("dataTable")
    )
  })

  # Density map
  output$showDensityMap <- renderUI({
    loginfo("show Density map UI")
    tagList(fluidRow(uiOutput("densityMapOptions")),
            fluidRow(column(
              6, offset = 3, plotOutput("densityMap", height = "550px")
            )))
  })

  # Compare subsets
  output$compareData <- renderUI({
    loginfo("compare data UI")
    tagList(uiOutput("showDensityMap"))
  })

  # Select subsets to compare
  output$selectCompareSubsets <- renderUI({
    isolate({
      # List all subsets
      subsets <- getSubsets(userInfo)
      subsetChoices <- names(subsets)
      names(subsetChoices) <- getSubsetsLabels(subsets)
    })
    tagList(
      fluidRow(
        class = "selectSubsetsCompare",
        compareSelectInput(1, subsetChoices, input, output, userInfo),
        compareSelectInput(2, subsetChoices, input, output, userInfo)
      )
    )
  })


  # Action buttons: change selection or create report
  output$viewDataActionButtons <- renderUI({
    tagList(div(
      class = "actionButtons",
      actionButton(
        class = "actionButton",
        "selectionPageAction",
        geti18nValue("select.data", userInfo$lang)
      ),
      actionButton(
        class = "actionButton",
        "showReportModal",
        geti18nValue("create.report", userInfo$lang)
      )
    ))
  })


}

getCompareInputId <- function(id, idx, isReport = FALSE) {
  report <- if (isReport) {
    "Report"
  } else {
    ""
  }
  selectInputId <- paste0(id, report, idx)
}

compareSelectInput <-
  function(idx,
           subsetChoices,
           input,
           output,
           userInfo,
           isReport = FALSE) {
    selectInputId <- getCompareInputId("selectCompareSubset", idx, isReport)
    infoOutputId <- getCompareInputId("subsetInfoCompare", idx, isReport)

    # selectInputId <- paste0("selectCompareSubset", reportId, idx)
    # infoOutputId <- paste0("subsetInfoCompare", reportId, idx)

    ## Output
    output[[infoOutputId]] <- renderUI({
      div(displaySubsetInfo(input[[selectInputId]], userInfo))
    })
    sel <- idx
    columnSize <- 4
    if (isReport) {
      selection <- input[[paste0("selectCompareSubset", idx)]]
      if (!is.empty(selection)) {
        sel <- which(subsetChoices == selection)
      }
      columnSize <- 6
    }

    column(columnSize,
           tagList(
             selectizeInput(
               selectInputId,
               geti18nValue(paste0("compare.choices.subset", idx), userInfo$lang),
               choices = subsetChoices,
               selected = subsetChoices[sel],
               options = list(maxItems = 1)
             ),
             uiOutput(infoOutputId)
           ))
  }

displaySubsetInfo <- function(subsetId, userInfo) {
  # Take a dependency on subset data
  getSubsetData(subsetId, userInfo, isolate = FALSE)
  values <- getFilterValues(subsetId, userInfo)
  tagList(
    labelWithHelp("subset.info", userInfo$lang, textclass = "title2"),
    if (is.empty(values)) {
      textOutput2(content = geti18nValue("filter.none", userInfo$lang), inline = FALSE)
    } else {
      lapply(names(values), function(id) {
        value <- values[id]
        div(
          textOutput2(
            content = paste0(geti18nValue(paste0("filter.", id), userInfo$lang), ": "),
            inline = TRUE
          ),
          textOutput2(
            content = paste0(value, collapse = "; "),
            inline = TRUE
          )
        )
      })
    }

  )
}
