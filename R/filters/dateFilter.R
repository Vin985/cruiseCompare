

## Name of the filter
DATE_FILTER <- "date"

# Type of filters
TYPE_RANGE <- "range"
TYPE_YEARS <- "years"
TYPE_MONTHS <- "months"
MISC_DATE_INFO <- "dateInfo"

###################
### Initialize
##################


## Main entry for the filter. Register Observers and Renders
initDateFilter <- function(input, output, session, userInfo) {
  selectDateRender(input, output, session, userInfo)
  selectDateObserver(input, output, session, userInfo)
}


#####################
### Util functions
####################


## Get month names for display
getMonthsNames <- function(lang) {
  unlist(lapply(1:12, function(x, lang) {
    geti18nValue(paste0("month.", x), lang)
  }, lang))
}


getMonthsValue <- function(condition, userInfo) {
  res <- lapply(condition, function(month) {
    geti18nValue(paste0("month.", month), userInfo$lang)
  })
  paste0(res, collapse = "; ")
}

getRangeValue <- function(condition, userInfo) {
  i18nInsert("range.value",
             replace = c(start = as.character(condition[1]),
                         end = as.character(condition[2])),
             userInfo$lang)
}


##############
###  Filter
#############


## Filter data by date
dateFilter <- function(data, condition) {
  loginfo("filtering by date: %s", condition)

  if (!is.null(condition$range)) {
    # Filter by range
    data <-
      data[data$Date >= condition$range[1] &
             data$Date <= condition$range[2], ]
  } else if (!is.empty(condition$years)) {
    # Filter by years
    data <- data[data$Year %in% as.numeric(condition$years),]
  }

  if (!is.empty(condition$months)) {
    # Filter by months
    data <- data[data$Month %in% as.numeric(condition$months), ]
  }


  return(data)
}


## Add the region filter to the subset
addDateFilter <- function(selections, type, userInfo) {
  filter <- getCurrentFilter(userInfo, DATE_FILTER)
  condition <- getCondition(filter)

  ## Range and years a mutually exclusive. If one is selected, clear the other
  if (type == TYPE_RANGE) {
    condition[[TYPE_YEARS]] <- NULL
  } else if (type == TYPE_YEARS) {
    condition[[TYPE_RANGE]] <- NULL
  }

  condition[[type]] <- selections

  filter <- setCondition(condition, filter)
  addFilterToSubset(userInfo, filter)
}


################
### Observers
###############


dateFilterEventHandler <- function(input, output, session, userInfo) {
  event <- isolate(userInfo$event)
  if (event$type == SUBSET_DATA_EVENT) {
    logdebug("update date subset")
    setDateData(userInfo)
  }

  updateDateChoices(input, session, userInfo, event = event$type)

}

## Update date inputs
updateDateChoices <-
  function(input, session, userInfo, event) {
    logdebug("updating date inputs")
    yearChoices <- NULL
    monthChoices <- NULL
    minRange <- NULL
    maxRange <- NULL
    startRange <- NULL
    endRange <- NULL


    if (event == CHANGE_LANG_EVENT) {
      ## Language changed, update labels only
      monthChoices <- 1:12
      names(monthChoices) <- getMonthsNames(userInfo$lang)
    } else {
      ## Update range and years based on subset
      yearChoices <- getDateYears(userInfo)
      bounds <- getDateRangeBounds(userInfo)
      minRange <- bounds[1]
      maxRange <- bounds[2]
    }

    ## update selection based on filters
    filters <- getCurrentCondition(userInfo, DATE_FILTER)
    selectDateBy <-
      ifelse(is.null(filters$range), TYPE_YEARS, TYPE_RANGE)
    restrictDate <- !is.null(filters$months)

    ## Update inputs based on range or years
    if (selectDateBy == TYPE_RANGE) {
      if (is.null(filters$range)) {
        # If no selection, use bounds
        startRange <- minRange
        endRange <- maxRange
      } else {
        # Use selection
        startRange <- filters$range[1]
        endRange <- filters$range[2]
      }

      updateDateRangeInput(
        session,
        "dateRange",
        start = startRange,
        end = endRange,
        min = minRange,
        max = maxRange,
        label = geti18nValue("select.date.range", userInfo$lang)
      )
    } else {
      selectedYears <- filters$years
      updateSelectizeInput(
        session,
        "dateYears",
        label = geti18nValue("select.date.years", userInfo$lang),
        choices = yearChoices,
        selected = selectedYears,
        options = list(
          plugins = list("remove_button"),
          placeholder = geti18nValue("select.all", userInfo$lang)
        )
      )
    }

    ## Update months input
    if (restrictDate) {
      selectedMonths <- filters$months
      updateSelectizeInput(
        session,
        "dateMonths",
        label = geti18nValue("select.date.months", userInfo$lang),
        choices = monthChoices,
        selected = selectedMonths,
        options = list(
          plugins = list("remove_button"),
          placeholder = geti18nValue("select.all", userInfo$lang)
        )
      )
    }


    if (event == CHANGE_SUBSET_EVENT) {
      # Subset changed, update all inputs
      updateRadioButtons(session, "selectDateBy", selected = selectDateBy)
      updateCheckboxInput(session, "restrictDate", value = restrictDate)

    } else if (event == CHANGE_LANG_EVENT) {
      # Language changed, update labels only
      selectDateByChoices <- list(TYPE_YEARS, TYPE_RANGE)
      names(selectDateByChoices) <-
        c(
          geti18nValue("select.date.years", userInfo$lang),
          geti18nValue("select.date.range", userInfo$lang)
        )
      updateRadioButtons(
        session,
        "selectDateBy",
        label = geti18nValue("select.date.by", userInfo$lang),
        choices = selectDateByChoices,
        inline = TRUE,
        selected = selectDateBy
      )

      updateCheckboxInput(
        session,
        "restrictDate",
        label = geti18nValue("date.restrict", userInfo$lang),
        value = restrictDate
      )
    }
  }


## Main observer function for date selection. All observers are defined here
selectDateObserver <-
  function(input, output, session, userInfo) {
    ## Date Selection mode
    observeEvent(input$selectDateBy, {
      ## We changed the selection mode, reset it
      if (input$selectDateBy == TYPE_RANGE) {
        addDateFilter(input$dateRange, type = TYPE_RANGE, userInfo)
      } else {
        addDateFilter(NULL, type = TYPE_YEARS, userInfo)
      }
    }, ignoreInit = TRUE)

    ## Remove all months if no restriction
    observeEvent(input$restrictDate, {
      if (!input$restrictDate) {
        addDateFilter(NULL, type = TYPE_MONTHS, userInfo)
      }
    }, ignoreInit = TRUE)

    ## Add range filter
    observeEvent(input$dateRange, {
      addDateFilter(input$dateRange, type = TYPE_RANGE, userInfo)
    }, ignoreInit = TRUE)

    ## Add years filter
    observeEvent(input$dateYears, {
      addDateFilter(input$dateYears, type = TYPE_YEARS, userInfo)
    }, ignoreNULL = FALSE, ignoreInit = TRUE)

    ## Add months filter
    observeEvent(input$dateMonths, {
      addDateFilter(input$dateMonths, type = TYPE_MONTHS, userInfo)
    }, ignoreNULL = FALSE, ignoreInit = TRUE)


  }


getDateRangeBounds <- function(userInfo) {
  dateInfo <- getMiscBySubset(MISC_DATE_INFO, userInfo)
  return(c(dateInfo$min, dateInfo$max))
}

getDateYears <- function(userInfo) {
  dateInfo <- getMiscBySubset(MISC_DATE_INFO, userInfo)
  return(dateInfo$years)
}

setDateData <- function(userInfo) {
  data <- isolate(getCurrentData(userInfo))
  min <- min(data$Date)
  max <- max(data$Date)
  years <-
    sort(unique(data$Year), decreasing = TRUE)
  dateMisc <- list(min = min, max = max, years = years)
  addMiscToSubset(MISC_DATE_INFO, dateMisc, userInfo)
  return(dateMisc)
}

##############
### Renders
#############

## Main render function for date selection. All UI render function are here
selectDateRender <- function(input, output, session, userInfo) {

  dateInfo <- isolate(setDateData(userInfo))

  selectDateBy <- list(TYPE_YEARS, TYPE_RANGE)
  months <- 1:12


  output$dateRangeSelect <- renderUI({
    isolate({
      min <- dateInfo$min
      max <- dateInfo$max
      dateRangeInput(
        "dateRange",
        label = geti18nValue("select.date.range", userInfo$lang),
        start = min,
        end = max,
        min = min,
        max = max
      )
    })
  })

  output$dateYearsSelect <- renderUI({
    isolate({
      years <- dateInfo$years
      selectizeInput(
        "dateYears",
        label = geti18nValue("select.date.years", userInfo$lang),
        choices = years,
        multiple = TRUE,
        options = list(
          plugins = list("remove_button"),
          placeholder = geti18nValue("select.all", userInfo$lang)
        )
      )
    })
  })

  ## Select by years or by range
  output$dateSelectionMode <- renderUI({
    # Select by range
    if (input$selectDateBy == TYPE_RANGE) {
      uiOutput("dateRangeSelect")
    } else {
      # Select by years
      uiOutput("dateYearsSelect")
    }
  })

  ## Restrict date by months
  output$dateRestriction <- renderUI({
    if (input$restrictDate) {
      isolate({
        names(months) <- getMonthsNames(userInfo$lang)
        selectizeInput(
          "dateMonths",
          label = geti18nValue("select.date.months", userInfo$lang),
          choices = months,
          multiple = TRUE,
          options = list(
            plugins = list("remove_button"),
            placeholder = geti18nValue("select.all", userInfo$lang)
          )
        )
      })
    }
  })


  ## Date title
  output$dateTitle <- renderUI({
    filterHeader(DATE_FILTER, userInfo)
  })

  ## Date UI
  output$selectDate <- renderUI({
    isolate({
      names(selectDateBy) <-
        c(
          geti18nValue("select.date.years", userInfo$lang),
          geti18nValue("select.date.range", userInfo$lang)
        )
      div(
        class = "selector selectDate",
        uiOutput("dateTitle"),
        tagList(
          radioButtons(
            "selectDateBy",
            label = geti18nValue("select.date.by", userInfo$lang),
            choices = selectDateBy,
            inline = TRUE
          ),
          uiOutput("dateSelectionMode"),
          checkboxInput(
            "restrictDate",
            geti18nValue("date.restrict", userInfo$lang),
            value = FALSE
          ),
          uiOutput("dateRestriction")
        )
      )
    })
  })
}
