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
      misc = "list"
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

getMisc <- function(subset, id) {
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

## Removes all subseets and create default one
resetSubsets <- function(input, userInfo) {
  userInfo$subsets <- NULL
  userInfo$subsetData <- NULL
  userInfo$subsetCpt <- 1
  removeSubsetObservers(userInfo)
  id <- createSubset(userInfo)
  createSubsetLinkObserver(id, input, userInfo)
}


######################
### UserInfo Utils
####################


## Get the current SubsetId
getCurrentSubsetId <- function(userInfo, isolate = TRUE) {
  if (isolate) {
    isolate(userInfo$currentSubset)
  } else {
    userInfo$currentSubset
  }
}


## Set the current subset to the one defined by subsetId
setCurrentSubset <- function(subsetId, userInfo) {
  userInfo$currentSubset <- subsetId
}


## Get info from userInfo by subsetId and type
extractInfoBySubsetId <- function(userInfo, subsetId, container, isolate = TRUE) {
  if (is.null(subsetId)) {
    subsetId <- getCurrentSubsetId(userInfo)
  }
  if (isolate) {
    isolate(userInfo[[container]][[subsetId]])
  } else {
    userInfo[[container]][[subsetId]]
  }
}


## Return all subsets
getSubsets <- function(userInfo, isolate = TRUE) {
  if (isolate) {
    isolate(userInfo$subsets)
  } else {
    userInfo$subsets
  }
}


## Get a subset from userInfo by subsetId
getSubset <- function(subsetId, userInfo, isolate = TRUE) {
  extractInfoBySubsetId(userInfo, subsetId, "subsets", isolate)
}


## Set Subset to userInfo
setSubset <- function(subset, userInfo) {
  subsetId <- getId(subset)
  userInfo$subsets[[subsetId]] <- subset
}


## Get Subset data by subsetId
getSubsetData <- function(subsetId, userInfo, as.df = FALSE, type = "raw", isolate = TRUE) {
  subsetData <- extractInfoBySubsetId(userInfo, subsetId, "subsetData", isolate)
  data <- subsetData[[type]]
  if (type == "raw" && as.df && !is.null(data@data)) {
    data <- data@data
  }
  return(data)
}


## Return the current subset
getCurrentSubset <- function(userInfo) {
  getSubset(NULL, userInfo)
}


## Get the data for the current subset
getCurrentSubsetData <- function(userInfo, as.df = FALSE, isolate = TRUE) {
  getSubsetData(NULL, userInfo, as.df, isolate)
}


## Return the current subset data if it exists or global data otherwise
getCurrentData <- function(userInfo, as.df = TRUE) {
  # Return the current subset data
  data <- getCurrentSubsetData(userInfo)

  # If subset is empty return global data
  if (is.empty(data)) {
    data <- getFullData(userInfo)
  }

  # Return data as a data.frame instead of a SpatialPointsDataFrame
  if (as.df) {
    return(data@data)
  } else {
    return(data)
  }
}


getDistanceAnalysis <- function(subsetId, userInfo, isolate = TRUE){
  getSubsetData(subsetId, userInfo, type = "distance", isolate)
}


getAnalyzedData <- function(subsetId, userInfo, isolate = TRUE){
  getSubsetData(subsetId, userInfo, type = "analyzed", isolate)
}


getCurrentAnalyzedData <- function(userInfo, isolate = TRUE){
  getSubsetData(NULL, userInfo, type = "analyzed", isolate)
}

## Add a filter to a subset
addFilterToSubset <-
  function(userInfo, filter, subsetId = NULL) {
    # Get subset by id. If null, the current one will be used
    subset <- getSubset(subsetId, userInfo)

    # Add filter to subset
    subset <- addFilter(subset, filter)

    ## Update subset in userInfo
    setSubset(subset, userInfo)
  }


## Add data to the misc part of the subset
addMiscToSubset <- function(id, value, userInfo, subsetId = NULL) {
  subset <- getSubset(subsetId, userInfo)
  subset <- setMisc(subset, id, value)
  setSubset(subset, userInfo)
}


## Get misc info from the subset
getMiscBySubset <- function(id, userInfo, subsetId = NULL) {
  subset <- getSubset(subsetId, userInfo)
  getMisc(subset, id)
}



## Return a vector of subset labels
getSubsetsLabels <- function(subsets) {
  unlist(lapply(subsets, function(x) {
    return(getLabel(x))
  }))
}

## Return a vector of subset labels
getSubsetLabelsById <- function(subsetIds, userInfo) {
  r <- unlist(lapply(subsetIds, function(x, userInfo) {
    s <- getSubset(x, userInfo)
    return(getLabel(s))
  }, userInfo))
  r
}


## Add an observer for the subset
setSubsetObserver <- function(subset, observer, userInfo) {
  subsetObservers <- isolate(userInfo$subsetObservers)
  if (is.empty(subsetObservers)) {
    userInfo$subsetObservers <- list()
  }
  userInfo$subsetObservers[[subset]] <- observer
}

## Remove the observer for a give subset
removeSubsetObserver <- function(subset, userInfo) {
  userInfo$subsetObservers[[subset]]$destroy()
  userInfo$subsetObservers[[subset]] <- NULL
}


## Remove all subset observers
removeSubsetObservers <- function(userInfo) {
  lapply(userInfo$subsetObservers, function(x){
    x$destroy
  })
  userInfo$subsetObservers <- NULL
}

## Check if there are some empty subsetted datasets. If yes, display error
checkEmptySubsetData <- function(userInfo) {
  subsetData <- isolate(userInfo$subsetData)
  res <- lapply(names(subsetData), function(x, subsetData){
    data <- subsetData[[x]]
    if (nrow(data$raw) == 0) {
      return(i18nInsert("subset.data.empty", userInfo$lang, replace = c(subsetid = x)))
    }
    return(NULL)
  }, subsetData)
  err <- unlist(res)
  if (length(err) == 0) {
    # No dataset empty, no error
    userInfo$subsetErrors <- NULL
    FALSE
  } else {
    # Empty dataset
    userInfo$subsetErrors <- err
    TRUE
  }
}



areSubsetsDifferent <- function(subset1, subset2, userInfo) {
  s1 <- getSubsetData(subset1, userInfo, as.df = TRUE)
  s2 <- getSubsetData(subset2, userInfo, as.df = TRUE)
  return(!identical(s1, s2))
}
