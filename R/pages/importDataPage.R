## Page Id
IMPORT_DATA_PAGE <- "importData"

IMPORT_TYPE_CSV <- "csv"
IMPORT_TYPE_RDATA <- "rdata"

RDATASETS <- c("alcidae", "quebec")
ECSASDATASET <- "ECSAS"

REQUIRED_COLUMNS <- list(
  col.count = "Count",
  col.distance = "Distance",
  col.effort = "WatchLenKm",
  col.observer.id = "ObserverID",
  col.cruise.id = "CruiseID",
  col.transect.id = "WatchID",
  col.longitude = "LongStart",
  col.latitude = "LatStart",
  col.species = "Alpha",
  col.date = "Date"
)

# OPTIONAL_COLUMNS <- list(
#   col.observer.id = "ObserverID",
#   col.cruise.id = "CruiseID",
#   col.cruise.start = "StartDate",
#   col.cruise.end = "EndDate",
#   col.date.year = "Year",
#   col.date.month = "Month",
#   col.species.english = "English",
#   col.species.french = "French"
# )

####
## Server helper function
####
checkImportedColumns <- function(data) {
  return(data)
}

importRData <- function(dataset) {
  if (dataset == ECSASDATASET) {
    load(ECSAS_PATH)
  } else {
    data(list = dataset, envir = environment())
  }
  importedData <- get(tolower(dataset))
  return(importedData)
}

importCsvData <- function(input, userInfo) {
  path <- input$csvDataFile
  importedData <- fread(path$datapath)
  return(importedData)
  # TODO : test csv files
}

importData <- function(input, userInfo) {
  # Load data, either provided datasets or csv file
  importedData <- if (input$importDataType == IMPORT_TYPE_RDATA) {
    importRData(isolate(input$rDataChoice))
  } else {
    importCsvData(input, userInfo)
  }

  return(importedData)

}

matchColumns <- function(input, userInfo) {
  columns <- c(REQUIRED_COLUMNS, getFilterColumns(FILTER_LIST))
  inputids <- paste0(names(columns), ".id")

  d <- isolate(userInfo$fullData)
  matched <-
    lapply(1:length(columns), function(idx, columns, data) {
      res <- list()
      expectedName <- columns[idx]
      matchName <- isolate(input[[inputids[idx]]])
      # No provided column
      if (is.empty(matchName)) {
        # if required, add error message
        if (expectedName %in% REQUIRED_COLUMNS) {
          colName <- geti18nValue(names(columns)[idx], userInfo$lang)
          res$err <- i18nInsert("field.required", userInfo$lang, replace = c(colname = colName))
        }
      } else if (matchName != expectedName) {
        res$old <- matchName
        res$new <- expectedName
      }
      res
    }, columns, d)

  old <- getListFields(matched, "old")
  new <- getListFields(matched, "new")
  err <- getListFields(matched, "err")

  new <- new[!duplicated(old)]
  old <- old[!duplicated(old)]


  # If no errors rename columns, otherwise return errors
  if (length(err) == 0 && !is.null(old) && !is.null(new)) {
    # rename fields from imported data to avoid problems later
    setnames(d, old, new)
    return(NULL)
  } else {
    # Return errors for future display
    return(err)
  }

}

getListFields <- function(list, field) {
  unlist(lapply(list, function(x) {
    x[[field]]
  }))
}

## Observers
importDataPage <- function(input, output, session, userInfo) {
  userInfo$columnsMatched <- FALSE
  importDataPageUI(input, output, userInfo)

  importedDataTable <- reactive({
    DT::datatable(
      isolate(userInfo$fullData),
      filter = "top",
      extensions = c("Buttons", "ColReorder"),
      options = list(
        sScrollX = "100%",
        orderClasses = TRUE,
        lengthMenu = c(10, 20, 50, 100),
        pageLength = 5,
        dom = 'Bfrtip',
        buttons = I('colvis')
      )
    )
  })

  observeEvent(input$importDataAction, {
    userInfo$fullData <- importData(input, userInfo)
    userInfo$columnsMatched <- FALSE
    userInfo$imported <- TRUE
  })

  observeEvent(input$selectFiltersAction, {
    # if already imported data but no change, do not reprepare data and keep filters
    if (!isolate(userInfo$imported)) {
      changePage(SELECTION_PAGE, userInfo)
    } else {
      res <- prepareData(input$filterECSAS, userInfo)
      if (res == 0) {
        ## Reset subset list
        userInfo$importError <- NULL
        resetSubsets(input, userInfo)
        changePage(SELECTION_PAGE, userInfo)
        selectDataFilters(input, output, session, userInfo)
        launchEvent(IMPORT_DATA_EVENT, userInfo)
      } else {
        userInfo$importError <- "import.error"
      }
    }
  })

  observeEvent(input$matchColumnsAction, {
    err <- matchColumns(input, userInfo)

    if (is.null(err)) {
      updateColumnRows(c(REQUIRED_COLUMNS, getFilterColumns(FILTER_LIST)), userInfo, session)
      userInfo$columnsMatched <- TRUE
      userInfo$missingFields <- NULL
      output$vizDataTable <-
        DT::renderDataTable(importedDataTable())
    } else {
        userInfo$missingFields <- err
    }
  })

  observeEvent(input$viewOnMapAction, {
    showModal(viewOnMapModal(input, output, session, userInfo))
  })

}

## UI
importDataPageUI <- function(input, output, userInfo) {

  output$missingFields <- renderUI({
    displayErrors(userInfo$missingFields)
  })

  output$importError <- renderUI({
    displayError(userInfo$importError)
  })

  output$importDataPage <- renderUI({
    tagList(
      uiOutput("importDataChoice"),
      uiOutput("matchColumns"),
      uiOutput("vizualizeData"),
      uiOutput("selectFiltersButton")
    )
  })

  output$importDataChoice <- renderUI({
    importChoices <- list(IMPORT_TYPE_RDATA, IMPORT_TYPE_CSV)
    names(importChoices) <-
      c(
        geti18nValue("import.type.rdata", userInfo$lang),
        geti18nValue("import.type.csv", userInfo$lang)
      )

    tagList(h3(geti18nValue("import.data", userInfo$lang)),
            div(
              class = "well clearfix",
              div(class = "description", i18nText("import.data.desc", userInfo$lang)),
              fluidRow(
                class = "row-eq-height",
                column(
                  3,
                  radioButtons(
                    "importDataType",
                    geti18nValue("import.data.choice", userInfo$lang),
                    choices = importChoices
                  )
                ),
                column(3, offset = 1, uiOutput("importDataOptions")),
                column(
                  5,
                  addActionButton("importDataAction", "import.data", userInfo)
                )
              )
            ))
  })

  output$importDataOptions <- renderUI({
    tagList(
    if (input$importDataType == IMPORT_TYPE_RDATA) {
      uiOutput("importDataRdata")
    } else {
      uiOutput("importDataCsv")
    }, div(checkboxInput("filterECSAS", geti18nValue("filter.data.ecsas", userInfo$lang), value = FALSE)))
  })

  output$importDataRdata <- renderUI({
    choices <- RDATASETS
    if (isLogged(userInfo$user)) {
      choices <- c(choices, ECSASDATASET)
    }
    selectInput(
      "rDataChoice",
      label = geti18nValue("rdata.choice.label", userInfo$lang),
      choices = choices
    )
  })

  output$importDataCsv <- renderUI({
    fileInput("csvDataFile", geti18nValue("import.csv.file", userInfo$lang))
  })

  output$importDataButton <- renderUI({
    tagList(div(
      class = "actionButtons",
      style = "padding: 0",
      actionButton(
        class = "actionButton",
        "importDataAction",
        geti18nValue("import.data", userInfo$lang)
      )
    ))
  })

  output$matchColumns <- renderUI({
    if (!is.null(userInfo$fullData)) {
      tagList(
        labelWithHelp("match.column.required", userInfo$lang, textclass = "h3"),
        div(
          class = "well clearfix",
          div(class = "description", i18nText("match.columns.desc", userInfo$lang)),
          uiOutput("missingFields", style = "margin-bottom:10px"),
          # required columns (for distance analysis)
          fluidRow(column(
            5,
            addColumnRows(REQUIRED_COLUMNS, userInfo, textclass = "required")
          ),
          # optional columns (for filters)
          column(
            5,
            addColumnRows(getFilterColumns(FILTER_LIST), userInfo)
          )),
          fluidRow(
            column(
              8,
              i18nTextOutput("required.field.warning", class = "required", userInfo$lang),
              i18nTextOutput("optional.field.warning",
                             userInfo$lang,
                             style = "padding-bottom: 10px")
            ),
            column(
              4,
              addActionButton("matchColumnsAction", "match.columns", userInfo)
            )
          )
        )
      )
    }
  })

  output$vizualizeData <- renderUI({
    if (userInfo$columnsMatched) {
      tagList(labelWithHelp("vizualize.data", userInfo$lang, textclass = "h3"),
                      div(
                        class = "well clearfix",
                        div(class = "description", i18nText("vizualize.data.desc", userInfo$lang)),
                        uiOutput("viewOnMapButton"),
                        DT::dataTableOutput("vizDataTable")
                      ))
    }
  })

  output$viewOnMapButton <- renderUI({
      tagList(div(
        actionButton(
          class = "actionButton viewOnMapButton",
          "viewOnMapAction",
          geti18nValue("view.on.map", userInfo$lang)
        )
      ))
  })

  output$selectFiltersButton <- renderUI({
    if (userInfo$columnsMatched) {
      tagList(
        div(uiOutput("importError")),
        div(
        class = "actionButtons",
        actionButton(
          class = "actionButton",
          "selectFiltersAction",
          geti18nValue("filter.data", userInfo$lang)
        )
      ))
    }
  })

  ## Map container
  output$displayDataMap <- renderUI({
    div(class = "displayMap",
        leafletOutput("dataMap", height = "100%", width = "100%"))
  })

  ## Map
  output$dataMap <-  renderLeaflet({
    data <- isolate(userInfo$fullData)
    withProgress({
      leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
        addTiles(group = "Base") %>%
        setView(lng = -65,
                lat = 49,
                zoom = 5) %>%
        addLayersControl(
          baseGroups = c("Base"),
          options = layersControlOptions(collapsed = TRUE),
          position = "bottomright"
        ) %>%  addCircleMarkers(
          data = data[!is.na(data$Count), ],
          lng = ~ LongStart,
          lat = ~ LatStart,
          radius = 2,
          fillColor = "blue",
          group = "observations",
          stroke = TRUE,
          color = "black",
          weight = 1,
          clusterOptions = markerClusterOptions()
        )
    }, message = "Patience...")
  })
}


####
## Render helper functions
###
updateColumnRows <- function(rows, userInfo, session) {
  lapply(1:length(rows), updateColumnRow, rows, userInfo, session)
}

updateColumnRow <- function(index, rows, userInfo, session) {
  choices <- c("", names(userInfo$fullData))
  selected <- ifelse(rows[index] %in% choices, rows[index], "")
  rowsid <- names(rows)[index]
  updateSelectInput(
    session,
    paste0(rowsid, ".id"),
    choices = choices,
    selected = selected
  )
}

addColumnRows <- function(rows, userInfo, textclass = "") {
  lapply(1:length(rows), addColumnRow, rows, userInfo, textclass)
}

addColumnRow <- function(index, rows, userInfo, textclass = "") {
  choices <- c("", names(userInfo$fullData))
  selected <- ifelse(rows[index] %in% choices, rows[index], "")
  rowsid <- names(rows)[index]
  tagList(fluidRow(
    column(6, labelWithHelp(rowsid, userInfo$lang, textclass), style = "text-align:right"),
    column(
      6,
      selectInput(
        paste0(rowsid, ".id"),
        label = NULL,
        choices = choices,
        selected = selected,
        multiple = FALSE
      )
    )
  ))
}

addActionButton <- function(id, label, userInfo) {
  div(class = "actionButtons importAction",
      actionButton(class = "actionButton",
                   id,
                   geti18nValue(label, userInfo$lang)))
}

viewOnMapModal <- function(input, output, session, userInfo) {
  modalDialog(uiOutput("displayDataMap"),
    footer = NULL,
    easyClose = TRUE
  )
}


