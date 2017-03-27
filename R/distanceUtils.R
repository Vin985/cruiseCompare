


getDistances <- function(userInfo) {
  distances <- userInfo$distance
  if (is.null(distances)) {
    distances <- list()
  }
  distances
}

getDistanceBySubset <-
  function(subsetId, userInfo, isolate = TRUE) {
    distances <-
      if (isolate) {
        isolate(getDistances(userInfo))
      } else {
        getDistances(userInfo)
      }
    distance <- distances[[subsetId]]
    if (is.null(distance)) {
      distance <- list()
    }
    distance
  }

addDistanceElement <- function(type, subsetId, value, userInfo) {
  distance <- getDistanceBySubset(subsetId, userInfo)
  distance[[type]] <- value
  userInfo$distance[[subsetId]] <- distance
}

getDistanceElement <-
  function(type, subsetId, userInfo, isolate = TRUE) {
    distance <- getDistanceBySubset(subsetId, userInfo, isolate)
    if (isolate) {
      isolate(distance[[type]])
    } else {
      distance[[type]]
    }
  }

addDistanceModel <- function(subsetId, model, userInfo) {
  addDistanceElement("model", subsetId, model, userInfo)
}

getDistanceModel <- function(subsetId, userInfo, isolate = FALSE) {
  getDistanceElement("model", subsetId, userInfo, isolate)
}

addDistanceGridSize <- function(subsetId, model, userInfo) {
  addDistanceElement("gridsize", subsetId, model, userInfo)
}

getDistanceGridSize <-
  function(subsetId, userInfo, isolate = TRUE) {
    getDistanceElement("gridsize", subsetId, userInfo, isolate)
  }
