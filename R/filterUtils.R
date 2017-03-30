



#######################
###  ECFilter Class
#####################


## Define the ECFilter class
ECFilter <-
  setClass(
    "ECFilter",
    slots = c(type = "character", condition = "list"),
    prototype = list(condition = list())
  )

### Accessors

getType <- function(filter) {
  filter@type
}

getCondition <- function(filter) {
  filter@condition
}

setCondition <- function(condition, filter) {
  filter@condition <- condition
  filter
}

initializeFilters <-
  function(filterList,
           input,
           output,
           session,
           userInfo) {
    initFunctions <-
      paste0("init", capitalizeFirst(filterList), "Filter")
    applyFunctionList(initFunctions, input, output, session, userInfo)
  }

propagateEvent <-
  function(filterList,
           input,
           output,
           session,
           userInfo) {
    eventHandlers <- paste0(filterList, "FilterEventHandler")
    applyFunctionList(eventHandlers, input, output, session, userInfo)
  }

applyFunctionList <- function(funcList, ...) {
  lapply(funcList, function(funcName, ...) {
    FUNC <- searchFunction(funcName)
    FUNC(...)
  }, ...)
}

## Filter data by a specific filter
filterBy <- function(by, filters, data) {
  # Get the filter
  filter <- filters[[by]]
  
  # if it exists, Call the "filter" function. It should be named
  # *by*Filter. E.g: observerFilter if by equals "observer"
  if (!is.null(filter)) {
    filterFunction <- searchFunction(paste0(by, "Filter"))
    data <- filterFunction(data, getCondition(filter))
  }
  
  data
}

searchFunction <- function(funcName) {
  func <- tryCatch({
    get(funcName)
  }, error = function(e) {
    stop(paste0("A function named ", funcName, " should be created"))
  })
  func
}


## Get all filters given a subsetId
getFiltersBySubset <- function(subsetId, userInfo) {
  getFilters(getSubset(subsetId, userInfo))
}


getFilterBySubset <-
  function(subsetId, userInfo, type, create = TRUE) {
    filter <- getFiltersBySubset(subsetId, userInfo)[[type]]
    if (create && is.null(filter)) {
      filter <- ECFilter(type = type)
    }
    return(filter)
  }

## Return the current filter of desired type
getCurrentFilter <- function(userInfo, type) {
  getFilterBySubset(NULL, userInfo, type)
}


## Get a filter condition based on subset Id
getConditionBySubset <- function(subsetId, userInfo, type) {
  filter <- getFilterBySubset(subsetId, userInfo, type)
  condition <- list()
  if (!is.null(filter)) {
    condition <- getCondition(filter)
  }
  condition
}

## Get the condition for the current filter of desired type
getCurrentCondition <- function(userInfo, type) {
  getConditionBySubset(NULL, userInfo, type)
}


## Get the values of the filters and convert them in a single
## string if they are of atomic type
getConditionValues <- function(type, condition, ...) {
  ## Create a single string with values
  cond <- condition[[type]]
  funcName <- paste0("get", capitalizeFirst(type), "Value") 
  if (exists(funcName)) {
    FUN <- get(funcName)
    return(FUN(cond, ...))
  } else if (is.atomic(condition)) {
    return(paste0(cond, collapse = "; "))
  } else {
    ## For non atomic types (e.g: region selection)
    return("")
  }
}


## Get data from filter conditions
getFilterData <- function(filter, ...) {
  condition <- getCondition(filter)
  values <- lapply(names(condition), getConditionValues, condition, ...)
  names(values) <- names(condition)
  return(values)
}

# getFilterValue <- function(filter, ...) {
#   values <- lapply(names(condition), function(type, condition,...) {
#   
#     if (type == TYPE_CRUISE) {
#       res <- lapply(cond, getCruiseValue, userInfo)
#       paste0(res, collapse = "; ")
#     } else {
#       paste0(cond, collapse = "; ")
#     }
#   }, condition)
#   names(values) <- names(condition)
#   return(values)
#   
#   FUN <- searchFunction()
#   FUN(getCondition(filter), ...)
# }

## Get filter values for display
getFilterValues <- function(subset, userInfo, ...) {
  filters <- getFiltersBySubset(subset, userInfo)
  filters <- filters[sort(names(filters))]
  values <- lapply(filters, getFilterData, userInfo, ...)
  names(values) <- names(filters)
  unlist(values)
}

## Return the selection defined by selectionType in a filter
getFilterSelection <-
  function(userInfo,
           filterType,
           selectionType,
           subsetId = NULL) {
    selection <- getConditionBySubset(subsetId, userInfo, filterType)
    return(selection[[selectionType]])
  }


## Is the item in the given condition
isSelected <- function(item, filterSelection) {
  if (is.empty(item) && is.empty(filterSelection)) {
    return(TRUE)
  }
  # check if they are the same
  return(identical(item, filterSelection))
}


## Recursively get through a list to concatenate all list names
getFilterNamesList <- function(filters) {
  if (length(filters) > 0) {
    unlist(lapply(seq_along(filters), function(i, filters) {
      # If filter has name, use it, otherwise use index
      name <- names(filters)[i]
      # If element is a list, paste current name and recall the function
      condition <- getCondition(filters[[i]])
      if (length(condition) > 0) {
        paste(name, getListNames(condition), sep = ".")
      }
    }, filters))
  } else {
    NULL
  }
}


## Apply filters to extract data for the defined subset
filterData <- function(subset, userInfo) {
  logdebug("filtering...")
  
  # Get filters for the current subset
  filters <- getFilters(subset)
  
  # Get whole data
  tmp <- isolate(userInfo$data)
  
  # Apply filters
  for (filter in filterList) {
    tmp <- filterBy(filter, filters, tmp)
  }
  
  analyzed <- analyzeData(tmp@data)
  
  list(raw = tmp, analyzed = analyzed)
}

## Filter all subsets
filterSubsets <- function(userInfo) {
  subsets <- getSubsets(userInfo)
  if (!is.null(subsets)) {
    logdebug("subsetting...")
    subsetsData <- lapply(subsets, filterData, userInfo)
    names(subsetsData) <- names(subsets)
    userInfo$subsetData <- subsetsData
  }
} 
