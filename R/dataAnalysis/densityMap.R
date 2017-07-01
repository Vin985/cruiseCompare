




getSubsetCompareChoices <- function(userInfo, subsetId) {
  subsets <- getSubsets(userInfo)
  subsets <- subsets[names(subsets) != subsetId]
  subsetChoices <- names(subsets)
  names(subsetChoices) <- getSubsetsLabels(subsets)
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
      compareModels(c(sub1, sub2), input, userInfo)
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
              return(NULL)
            }
            plotDensityMap(
              densities = model$densities,
              grid = model$grid,
              transects = NULL,
              shpm = userInfo$landShp,
              lang = userInfo$lang,
              subsetNames = getSubsetLabelsById(model$subsets, userInfo)
            )
          } else {
            subsetId <- getCurrentSubsetId(userInfo, isolate = FALSE)
            model <- getDistanceModel(subsetId, userInfo)
            if (!is.null(model)) {
              plotDensityModel(model$density, shpm = userInfo$landShp, lang = userInfo$lang)
            } else {
              return(NULL)
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
             i18nTextOutput("warning.grid.size", userInfo$lang,
                            style = "color: blue; font-size: 12px;")),
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
