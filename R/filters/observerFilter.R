



## Name of the filter
OBSERVER_FILTER <- "observer"

## Types of filters
TYPE_OBSERVER <- "observer"
TYPE_CRUISE <- "cruises"

MISC_OBSERVER_LIST <- "observerList"

OBSERVER_COLUMNS <-   c(
  col.observer.name = "ObserverName",
  col.cruise.start = "StartDate",
  col.cruise.end = "EndDate"
)
#

###################
### Initialize
##################

canUseObserverFilter <- function(userInfo) {
  # All fields are optional or always created
  return(TRUE)
}


## Main entry for the filter. Register Observers and Renders
initObserverFilter <- function(input, output, session, userInfo) {
  selectObserverRender(input, output, session, userInfo)
  selectObserverObserver(input, output, session, userInfo)
}

getCruiseValue <- function(cruiseId, userInfo) {
  data <- getFullData(userInfo, as.df = TRUE)
  if (!is.null(data$StartDate) & !is.null(data$EndDate)) {
    dates <-
      data[data$CruiseID == cruiseId, c("StartDate", "EndDate")][1,]
    i18nInsert(
      "cruises.value",
      replace = c(
        id = cruiseId,
        start = format(dates$StartDate, "%Y-%m-%d"),
        end = format(dates$EndDate, "%Y-%m-%d")
      ),
      userInfo$lang
    )
  } else {
    cruiseId
  }
}

getCruisesValue <- function(condition, userInfo) {
  res <- lapply(condition, getCruiseValue, userInfo)
  paste0(res, collapse = "; ")
}

getObserverValue <- function(condition, ...) {
  res <- lapply(condition, function(name) {
    paste(rev(unlist(strsplit(name, "_"))), collapse = " ")
  })
  paste0(res, collapse = "; ")
}

#####################
### Util functions
####################


## Get the observer list for the current subset
getObserverList <- function(userInfo) {
  data <- getMiscBySubset(MISC_OBSERVER_LIST, userInfo)
  if (is.null(data)) {
    data <- setObserverList(userInfo)
  }
  return(data)
}


## Store the observer list for the current subset to avoid having to recreate
## it all the time
setObserverList <- function(userInfo) {
  data <- isolate(arrange(
    distinct(getCurrentData(userInfo), ObserverName, CruiseID),
    ObserverName
  ))
  addMiscToSubset(MISC_OBSERVER_LIST, data, userInfo)
  data
}


## Get the selected observer in the filer
getSelectedObserver <- function(userInfo) {
  return(getFilterSelection(userInfo, OBSERVER_FILTER, TYPE_OBSERVER))
}

getSelectedCruises <- function(userInfo) {
  return(getFilterSelection(userInfo, OBSERVER_FILTER, TYPE_CRUISE))
}


## Checks if observer is already in the filter
isSelectedObserver <- function(observer, userInfo) {
  selection <- getSelectedObserver(userInfo)
  return(isSelected(observer, selection))
}


##############
###  Filter
#############


## Filter data by observer
observerFilter <- function(data, condition) {
  loginfo("filtering with observer %s and cruises %s",
          condition[[TYPE_OBSERVER]],
          condition[[TYPE_CRUISE]])

  ## Filter by observer
  observer <- condition[[TYPE_OBSERVER]]
  if (!is.empty(observer)) {
    data <- data[which(data$ObserverName == observer),]
  }

  ## Filter by cruises
  cruises <- condition[[TYPE_CRUISE]]
  if (!is.empty(cruises)) {
    data <- data[data$CruiseID %in% cruises,]
  }

  return(data)
}


## Add the observer filter to the subset
addObserverFilter <- function(selections, type, userInfo) {
  filter <- getCurrentFilter(userInfo, OBSERVER_FILTER)
  condition <- getCondition(filter)

  condition[[type]] <- selections

  loginfo("add %s filter with value %s", type, selections)

  filter <- setCondition(condition, filter)
  addFilterToSubset(userInfo, filter)
}


################
### Observers
###############

observerFilterEventHandler <-
  function(input, output, session, userInfo) {
    event <- isolate(userInfo$event)
    updateChoices <- TRUE
    if (event$type == CHANGE_LANG_EVENT) {
      loginfo("changing language")
      updateChoices <- FALSE
    } else if (event$type == SUBSET_DATA_EVENT) {
      loginfo("subsetting Data")
      setObserverList(userInfo)
    }
    updateObserverInput(session, userInfo, updateChoices = updateChoices)
  }

## Update the cruises list
updateCruisesInput <-
  function(observer,
           session,
           userInfo) {
    logdebug("cruise update")

    cruiseChoices <- NULL
    ## Update selection
    cruiseSelection <- getSelectedCruises(userInfo)

    cruiseData <- getObserverList(userInfo)
    if (!is.empty(observer)) {
      cruiseChoices <-
        filter(cruiseData, ObserverName == observer)$CruiseID
    } else {
      # If observer is empty, laod all cruises
      cruiseChoices <- cruiseData$CruiseID
    }

    ## Update cruise input
    logdebug("selectedCruises : %s ",
             cruiseSelection)
    updateSelectizeInput(
      session,
      "cruises",
      label = geti18nValue("select.cruises", userInfo$lang),
      choices = cruiseChoices,
      selected = cruiseSelection,
      options = list(
        plugins = list("remove_button"),
        placeholder = geti18nValue("select.all", userInfo$lang)
      )
    )

  }


## Update the observers list
updateObserverInput <-
  function(session,
           userInfo,
           updateChoices = TRUE) {
    observerChoices <- NULL
    data <- NULL
    ## Update choice list?
    data <- getObserverList(userInfo)
    observerChoices <- c("", unique(data$ObserverName))

    ## Update selection ?
    observerSelection <- getSelectedObserver(userInfo)

    logdebug("selectedObserver : %s",
             observerSelection)
    ## Update the observer input
    updateSelectizeInput(
      session,
      "observer",
      choices = observerChoices,
      selected = observerSelection,
      label = geti18nValue("select.observers", userInfo$lang),
      options = list(placeholder = geti18nValue("select.all", userInfo$lang))
    )

    updateCruisesInput(observerSelection, session, userInfo)
  }


## Main observer function for observer selection. All observers are defined here
selectObserverObserver <-
  function(input, output, session, userInfo) {
    ## Observer selection has changed
    observeEvent(input$observer,
                 {
                   logdebug("observer changed: %s", input$observer)
                   # Only update input and filter if observer isn't in it
                   if (!isSelectedObserver(input$observer, userInfo)) {
                     # Add observer filter
                     addObserverFilter(input$observer, TYPE_OBSERVER, userInfo)
                     # Update list of cruises
                     updateCruisesInput(input$observer, session, userInfo)
                   }
                 },
                 ignoreNULL = FALSE,
                 ignoreInit = TRUE)


    ## Cruise selection has changed
    observeEvent(input$cruises,
                 {
                   # add cruise filter
                   addObserverFilter(input$cruises, TYPE_CRUISE, userInfo)
                 },
                 ignoreNULL = FALSE,
                 ignoreInit = TRUE)

  }


##############
### Renders
#############


## Main render function for observer selection. All UI render function are here
selectObserverRender <- function(input, output, session, userInfo) {
  cruiseData <- isolate(setObserverList(userInfo))

  ## Observer title
  output$observerTitle <- renderUI({
    headerWithHelp(OBSERVER_FILTER, userInfo$lang)
  })

  ## Select observerss
  output$selectObserver <- renderUI({
    isolate({
      div(class = "selector selectObserver",
          uiOutput("observerTitle"),
          tagList(
            selectizeInput(
              "observer",
              label = geti18nValue("select.observers", userInfo$lang),
              choices = c("", unique(cruiseData$ObserverName)),
              selected = "",
              multiple = FALSE,
              options = list(placeholder = geti18nValue("select.all", userInfo$lang))
            ),
            uiOutput("selectCruises")
          ))
    })
  })

  # Select cruises
  output$selectCruises <- renderUI({
    isolate({
      selectizeInput(
        "cruises",
        label = geti18nValue("select.cruises", userInfo$lang),
        choices = c("", unique(cruiseData$CruiseID)),
        multiple = TRUE,
        options = list(
          plugins = list("remove_button"),
          placeholder = geti18nValue("select.all", userInfo$lang)
        )
      )
    })
  })
}
