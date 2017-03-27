




getSubsetCompareChoices <- function(userInfo, subsetId) {
  subsets <- getSubsets(userInfo)
  subsets <- subsets[names(subsets) != subsetId]
  subsetChoices <- names(subsets)
  names(subsetChoices) <- getSubsetsLabels(subsets)
}


# generate distance models for all subsets
generateDensityModel <- function(subsetIds, input, userInfo) {
  
  loginfo("retrieving models")
  distanceData <- getDistanceData(subsetIds, userInfo)
  transects <- getTransects(distanceData)
  
  gridSize <- input$densityGridSize
  if (is.empty(gridSize)) {
    gridSize <- DEFAULT_GRIDSIZE
  }
  grid <- createHexGrid(transects, width = gridSize * 1000, convex = FALSE)
  # generate distance model for each subset
  models <- lapply(subsetIds, function(id, data, grid, userInfo) {
    loginfo("generating model for subset %s", id)
    d <- data[subset == id]
    detectionModel <- getDetectionModel(d)
    densityModel <-
      getDensityModel(d, grid, detectionModel$estimator)
    list(detection = detectionModel, density = densityModel)
  }, distanceData, grid, userInfo)
  names(models) <- subsetIds
  models
}


densityMap <- function(input, output, session, userInfo) {
  # Are we in comparison page?
  isCompare <- reactive({
    action <- input$dataAction
    (!is.null(action) && action == DATA_ACTION_COMPARE)
  })
  
  observeEvent(input$displayDensityMap, {
    compare <- isolate(isCompare())
    if (!compare) {
      withProgress(
        message = geti18nValue("generating.density.models", userInfo$lang),
        detail = geti18nValue("wait.message", userInfo$lang),
        style = "old",
        value = 0,
        {
          subsetId <- getCurrentSubsetId(userInfo)
          model <- generateDensityModel(subsetId, input, userInfo)
          addDistanceModel(subsetId, model[[subsetId]], userInfo)
        }
      )
    }
  }, ignoreInit = TRUE)
  
  
  # generate comparison model
  compareModel <- eventReactive(input$displayDensityMap, {
    if (isCompare()) {
      validate(
        need(
          input$selectCompareSubset1,
          geti18nValue("need.subset1", userInfo$lang)
        ),
        need(
          input$selectCompareSubset2,
          geti18nValue("need.subset2", userInfo$lang)
        )
      )
      isolate({
        sub1 <- input$selectCompareSubset1
        sub2 <- input$selectCompareSubset2
      })
      models <- generateDensityModel(c(sub1, sub2), input, userInfo)
      model1 <- models[[sub1]]$density
      model2 <- models[[sub2]]$density
      newDensities <- compareDensities(model1, model2)
      list(densities = newDensities, grid = model1$grid, subsets = c(sub1, sub2))
    }
  }, ignoreInit = TRUE)
  
  # Display the density map
  output$densityMap <- renderPlot({
    compare <- isCompare()
    loginfo("rendering plot")
    
    if (compare ||
        input$showDataType == DENSITY_MAP) {
      withProgress(
        message = geti18nValue("displaying.density.map", userInfo$lang),
        detail = geti18nValue("wait.message", userInfo$lang),
        style = "old",
        value = 0,
        {
          # subset comparison
          if (compare) {
            model <- compareModel()
            if (is.empty(model)) {
              return()
            }
            plotDensityMap(
              densities = model$densities,
              grid = model$grid,
              transects = NULL,
              shpm = LAND_MAP_SHP,
              lang = userInfo$lang,
              subsetNames = getSubsetLabelsById(model$subsets, userInfo)
            )
          } else {
            subsetId <- getCurrentSubsetId(userInfo, isolate = FALSE)
            model <- getDistanceModel(subsetId, userInfo)
            if (!is.null(model)) {
              plotDensityModel(model$density, shpm = LAND_MAP_SHP, lang = userInfo$lang, 
                               subsetNames = getSubsetLabelsById(subsetId, userInfo))
            }
          }
        }
      )
    }
    
  })
  
  output$densityMapOptions <- renderUI({
    # grid size
    # display transects
    # display Button
    gridSize <- NULL
    if (!isCompare()) {
      gridSize <- getDistanceGridSize(getCurrentSubsetId(userInfo), userInfo)
    }
    if (is.null(gridSize)) {
      gridSize <- DEFAULT_GRIDSIZE
    }
    tagList(
      column(6, div(class = "gridSize",
             numericInput("densityGridSize", 
                             label = geti18nValue("grid.size", userInfo$lang), 
                             value = gridSize),
             span("km")),
             i18nTextOutput("warning.grid.size", userInfo$lang, style = "color: blue;")),
      column(3, ""),
      column(3, class = "compareSubsetButton",
                   div(
                     class = "actionButtons",
                     actionButton(
                       class = "actionButton",
                       inputId = "displayDensityMap",
                       geti18nValue("show.density.map", userInfo$lang)
                     )
                   )))
  })
}
