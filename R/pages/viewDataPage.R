


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
  birds = c("mean", "sd", "cv", "meanFlock"),
  total = c("WatchLenKm", "Densities")
)


viewDataPage <- function(input, output, session, userInfo) {
  observeEvent(input$selectionAction, {
    changePage(SELECTION_PAGE, userInfo)
  })
  
  observeEvent(input$displayDensityMap, {
    subsetId <- getCurrentSubsetId(userInfo)
    userInfo$subsetData[[subsetId]]$distance <- getDistanceAnalysis(subsetId, userInfo)
  })
  
  # Get Selected subsets
  distanceSubsets <- reactive({
    subsetIds <- isolate(input$subsetDensityCompare)
    if (is.null(subsetIds)) {
      subsetIds <- getCurrentSubsetId(userInfo, isolate = FALSE)
    }
    subsetIds
  })
  
  # get datafor distance analysis
  distanceData <- reactive({
    # Get selected subsets
    subsetIds <- distanceSubsets()
    # create one table for all subsets
    data <- do.call(rbind, lapply(subsetIds, function(id, userInfo){
      d <- getSubsetData(id, userInfo, as.df = TRUE)
      # add a subset column
      d$subset <- id
      d
    }, userInfo))
    # convert to data.table
    data <- data.table(data)
  })
  
  distanceTransects <- reactive({
    getTransects(distanceData())
  })
  
  # generate distance models for all subsets
  distanceModels <- reactive({
    data <- distanceData()
    transects <- distanceTransects()
    subsetIds <- distanceSubsets()
    grid <- createGrid(transects)
    # generate distance model for each subset
    models <- lapply(subsetIds, function(id, data, grid, userInfo){
      d <- data[subset == id]
      detectionModel <- getDetectionModel(data)
      densityModel <- getDensityModel(data, grid, detectionModel$estimator)
      list(detection = detectionModel, density = densityModel)
    }, data, grid, userInfo)
    names(models) <- subsetIds
    models
  })
  
  output$densityMap <- renderPlot({
    if (input$showDataType == DENSITY_MAP) {
      withProgress(message = 'Calculation in progress',
                   detail = 'This may take a while...',
                   style = "old",
                   value = 0,
                   {
                     subsetIds <- distanceSubsets()
                     if (length(subsetIds == 1)) {
                       model <- distanceModels()[[1]]
                       plotDensityMap(model$density)
                     }
                   })
    }
  })
  
  viewDT <- reactive({
    # Get a dependency on current subsetId
    subsetId  <- getCurrentSubsetId(userInfo, isolate = FALSE)
    type <- input$dataType
    if (type == DATA_TYPE_RAW) {
      dt <- getSubsetData(subsetId, userInfo, as.df = TRUE)
    }
    else {
      dt <- getAnalyzedData(subsetId, userInfo)[[input$dataType]]
    }
    DT::datatable(
      dt,
      filter = "top",
      extensions = c("Buttons", "ColReorder"),
      options = list(
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
      fluidRow(column(8, uiOutput("subsetInfo")),
               column(4, uiOutput(class = "dataAction",
                 "selectDataAction"
               ))),
      fluidRow(uiOutput("dataContent")),
      fluidRow(uiOutput("viewDataActionButtons"))
    ))
  })
  
  output$subsetInfo <- renderUI({
    userInfo$event
    values <- getFilterValues(input$subsetFilterChoices, userInfo)
    tagList(h5(geti18nValue("subset.info", userInfo$lang)),
    lapply(names(values), function(id) {
    value <- values[id]
    div(textOutput2(content = paste0(geti18nValue(paste0("filter.", id), userInfo$lang), ": "), inline = TRUE),
        textOutput2(content = paste0(value, collapse = "; "), inline = TRUE))
    }))
  })
  
  output$selectDataAction <- renderUI({
    if (length(getSubsets(userInfo, isolate = FALSE)) > 1) {
      actionType <- list(DATA_ACTION_VIEW, DATA_ACTION_COMPARE)
      names(actionType) <-
        c(
          geti18nValue("data.view", userInfo$lang),
          geti18nValue("data.compare", userInfo$lang)
        )
      tagList(h5(geti18nValue("view.select.action", userInfo$lang)),
      radioButtons(
        "dataAction",
        label = NULL, #geti18nValue("data.action", userInfo$lang),
        choices = actionType,
        inline = TRUE
      ))
    }
  })
  
  output$showData <- renderUI({
    if (input$showDataType == DATA_TABLE) {
      uiOutput("showDataTable")
    } else {
      uiOutput("showDensityMap")
    }
  })
  
  output$showDensityMap <- renderUI({
    tagList(fluidRow(column(
      10, offset = 1, plotOutput("densityMap", height = "600px")
    )))
  })
  
  output$showDataTable <- renderUI({
    dataType <- list(DATA_TYPE_TOTAL, DATA_TYPE_BIRDS, DATA_TYPE_RAW)
    names(dataType) <-
      c(
        geti18nValue("data.view.total", userInfo$lang),
        geti18nValue("data.view.birds", userInfo$lang),
        geti18nValue("data.view.raw", userInfo$lang)
      )
    tagList(
      h5(geti18nValue("view.data.dataset",userInfo$lang)),
      radioButtons(
        "dataType",
        label = geti18nValue("data.view.label", userInfo$lang),
        choices = dataType,
        inline = TRUE
      ),
      dataTableOutput("dataTable", width = "90%")
    )
  })
  
  output$dataContent <- renderUI({
    if (length(getSubsets(userInfo)) < 2 |
        (!is.empty(input$dataAction) &&
         input$dataAction == DATA_ACTION_VIEW)) {
      uiOutput("viewData")
    } else {
      uiOutput("compareData")
    }
  })
  
  output$selectCompareSubsets <- renderUI({
    isolate({
      # List all subsets
      subsets <- getSubsets(userInfo)
      subsetChoices <- names(subsets)
      names(subsetChoices) <- getSubsetsLabels(subsets)
    })
    selectizeInput(
      "subsetCompareChoices",
      geti18nValue("compare.choices.subset", userInfo$lang),
      choices = subsetChoices,
      multiple = TRUE,
      options = list(maxItems = 2)
    )
  })
  
  output$compareData <- renderUI({
    tagList(fluidRow(uiOutput("selectCompareSubsets")),
            fluidRow())
  })
  
  output$viewData <- renderUI({
      dataType <- list(DATA_TABLE, DENSITY_MAP)
      names(dataType) <-
        c(
          geti18nValue("data.show.table", userInfo$lang),
          geti18nValue("data.density.map", userInfo$lang)
        )
      tagList(div(class = "dataAction dataTabs", radioButtons(
                "showDataType",
                label = NULL,
                choices = dataType,
                inline = TRUE
      )), uiOutput("showData"))
  })
  
  output$viewDataActionButtons <- renderUI({
    tagList(div(
      class = "actionButtons",
      actionButton(
        class = "actionButton",
        "selectionAction",
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
