

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

## Filter data by a specific filter
filterBy <- function(by, filters, data) {
  # Get the filter
  filter <- filters[[by]]
  
  # if it exists, Call the "filter" function. It should be named
  # *by*Filter. E.g: observerFilter if by equals "observer"
  if (!is.null(filter)) {
    funcName <- paste0(by, "Filter")
    filterFunction <- tryCatch({
      get(funcName)
    }, error = function(e) {
      stop(paste0("A function named ", funcName, " should be created"))
    })
    
    data <- filterFunction(data, getCondition(filter))
  }
  
  data
}

#######################
###  ECSubset Class
#####################

## Define the ECSubset class
ECSubset <-
  setClass(
    "ECSubset",
    slots = c(
      id = "character",
      filters = "list",
      label = "character",
      misc = "list",
      observerList = "data.frame"
    ),
    prototype = list(filters = list(), misc = list())
  )

getId <- function(subset) {
  subset@id
}

getFilters <- function(subset) {
  subset@filters
}

addFilter <- function(subset, filter) {
  type <- getType(filter)
  subset@filters[[type]] <- filter
  return(subset)
}

getLabel <- function(subset) {
  subset@label
}

getMisc <- function(subset, id){
  subset@misc[[id]]
}


setMisc <- function(subset, id, value) {
  subset@misc[[id]] <- value
  subset
}

## Create new subset
createSubset <- function(userInfo, label = NULL) {
  # Check if subsets is initialized
  subsets <- isolate(userInfo$subsets)
  if (is.null(subsets)) {
    subsets <- list()
  }
  cpt <- isolate(userInfo$subsetCpt)
  # Create a new subsetId
  subsetId <- paste0("subset", cpt)
  # Create a new subset
  if (is.empty(label)) {
    label <- subsetId
  }
  subsets[[subsetId]] <- ECSubset(id = subsetId, label = label)
  userInfo$subsets <- subsets
  # Increment the counter
  userInfo$subsetCpt <- cpt + 1
  # Set as current subset
  userInfo$currentSubset <- subsetId
  return(subsetId)
}


######################
### UserInfo Utils
####################


## Getthe current SubsetId
getCurrentSubsetId <- function(userInfo) {
  isolate(userInfo$currentSubset)
}

setCurrentSubset <- function(subsetId, userInfo){
  userInfo$currentSubset <- subsetId
}

## Get info from userInfo by subsetId and type
extractInfoBySubsetId <- function(userInfo, subsetId, container) {
  if (is.null(subsetId)) {
    subsetId <- getCurrentSubsetId(userInfo)
  }
  isolate(userInfo[[container]][[subsetId]])
}


getSubsets <- function(userInfo) {
  isolate(userInfo$subsets)
}

## Get a subset from userInfo by subsetId
getSubset <- function(subsetId, userInfo) {
  extractInfoBySubsetId(userInfo, subsetId, "subsets")
}

## Set Subset to userInfo
setSubset <- function(subset, userInfo) {
  subsetId <- getId(subset)
  userInfo$subsets[[subsetId]] <- subset
}

## Get Subset data by subsetId
getSubsetData <- function(subsetId, userInfo) {
  extractInfoBySubsetId(userInfo, subsetId, "subsetData")
}

## Return the current subset
getCurrentSubset <- function(userInfo) {
  getSubset(NULL, userInfo)
}

## Get the data for the current subset
getCurrentSubsetData <- function(userInfo) {
  getSubsetData(NULL, userInfo)
}


## Add a filter to a subset
addFilterToSubset <-
  function(userInfo, filter, subsetId = NULL) {
    # browser()
    # Get subset. By default, if subsetId is null, the current one will
    # be used
    subset <- getSubset(subsetId, userInfo)
    
    # Add filter to subset
    subset <- addFilter(subset, filter)
    
    ## Update subset in userInfo
    setSubset(subset, userInfo)
  }


## Return the current subset data if it exists or global data otherwise
getCurrentData <- function(userInfo, as.df = TRUE) {
  # Return the current subset data
  data <- getCurrentSubsetData(userInfo)
  
  # If subset is empty return global data
  if (is.empty(data)) {
    data <- getFullData()
  }
  
  # Return data as a data.frame instead of a SpatialPointsDataFrame
  if (as.df) {
    return(data@data)
  } else {
    return(data)
  }
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


addMiscToSubset <- function(id, value, userInfo, subsetId = NULL) {
  subset <- getSubset(subsetId, userInfo)
  subset <- setMisc(subset, id, value)
  setSubset(subset, userInfo)
}

getMiscBySubset <- function(id, userInfo, subsetId = NULL) {
  subset <- getSubset(subsetId, userInfo)
  getMisc(subset, id)
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
