DISTANCE_MODEL_NAMES <-
  c("uniform",
    "uniform",
    "half-normal",
    "half-normal",
    "hazard rate",
    "hazard rate")
DISTANCE_ADJ_NAMES <-
  c("cosinus",
    "polynomial",
    "cosinus",
    "hermite polynomial",
    "cosinus",
    "polynomial")



getObserverNames <- function(observers) {
  # Get unique names
  if (is.factor(observers)) {
    obs <- levels(droplevels(observers))
  } else {
    obs <- unique(observers)
  }
  obs <- obs[!is.na(obs)]
  split_obs <- strsplit(obs, "_")
  # Reverse last name and first name
  names <- lapply(split_obs, function(x) {
    paste(x[2], x[1])
  })
  # Combine all observers
  return(paste(names, collapse = ", "))
}


getDistanceData <- function(subsetIds, userInfo) {
  loginfo("retrieving data")
  # create one table for all subsets
  data <-
    do.call(rbind, lapply(subsetIds, function(id, userInfo) {
      d <- getSubsetData(id, userInfo, as.df = TRUE, isolate = FALSE)
      # add a subset column
      d$subset <- id
      d
    }, userInfo))
  # convert to data.table
  data <- data.table(data)
}

analyzeData <- function(data) {
  data <- droplevels(data)
  dt <- data.table(data)
  setkey(dt, WatchID)
  
  dt$Alpha[dt$Alpha == ""] <- NA
  dt$English[dt$English == ""] <- NA
  
  ##adapt species names
  dt$English <- as.factor(dt$English)
  dt$Alpha <- as.factor(dt$Alpha)
  
  ### Total densities
  total <- dt[, .(
    WatchLenKm = mean(WatchLenKm, na.rm = TRUE),
    Count = sum(Count, na.rm = TRUE)
  ),
  by = WatchID]
  total[is.na(Count)] <- 0
  total[, Densities := Count / WatchLenKm]
  total <- total[!is.na(WatchLenKm)]
  
  
  ### Species descriptive stats
  birds <- dt[, .(
    Flocks = .N,
    Count = sum(Count, na.rm = T),
    mean = mean(Count, na.rm = T),
    sd = sd(Count, na.rm = T),
    French = unique(French)
  ), by = English]
  birds[, cv := sd / mean]
  birds[, meanFlock := round(Count / Flocks, 1)]
  # birds <- birds[!is.na(cv)]
  setkey(birds, English)
  
  #descriptives
  observer <- getObserverNames(dt$ObserverID)
  
  list(total = total, birds = birds)
}


getDetectionModel <- function(dt) {
  loginfo("generating detection model")
  if (!is.data.table(dt)) {
    dt <- as.data.table(dt)
  }
  total <- dt[, .(WatchLenKm = mean(WatchLenKm, na.rm = TRUE)),
              by = WatchID]
  total <- total[!is.na(WatchLenKm)]
  
  # Multiply by 0.3 because we have linear transects 300m wide
  dt <- droplevels(dt)
  dt[, STR_AREA := sum(total$WatchLenKm) * 0.3]
  
  all.dist <- mcds.wrap(
    dt,
    SMP_EFFORT = "WatchLenKm",
    DISTANCE = "Distance",
    SIZE = "Count",
    units = list(
      Type = "Line",
      Distance = "Perp",
      Length_units = "Kilometers",
      Distance_units = "Meters",
      Area_units = "Square kilometers"
    ),
    breaks = c(0, 50, 100, 200, 300),
    STR_LABEL = "STR_LABEL",
    STR_AREA = "STR_AREA",
    SMP_LABEL = "WatchID",
    path = ANALYSIS_DIR,
    pathMCDS = TOOLS_DIR,
    verbose = FALSE
  )
  
  all.sp.best <- keep.best.model(all.dist)
  
  
  mod.selected <- which.min(sapply(1:length(all.dist), function(i) {
    all.dist[[i]]$AIC[3]
  }))
  
  key <-
    sapply(strsplit(names(all.dist)[mod.selected], "_"), "[", 2)
  exp <-
    sapply(strsplit(names(all.dist)[mod.selected], "_"), "[", 3)
  estimator = list(c(key, exp))
  
  return(list(
    idx = mod.selected,
    model = all.sp.best,
    estimator = estimator
  ))
}


getDetectionProbability <- function(model) {
  ####Extract the probability of detection
  detectionProbability <- model$parameter_estimates$Global[, -1]
  detectionProbability[, -1] <- round(detectionProbability[, -1], 2)
}


getPrediction <- function(model) {
  ###extract prediction
  prediction <- model$density_estimate$Global
  prediction[, -c(1, 2)] <- round(prediction[, -c(1, 2)], 2)
}


getPredictionDF <- function(model) {
  data.frame(x = model$detection[['Global']][, 'distance'],
             y = model$detection[['Global']][, 'predicted'])
}



getTransects <- function(data, prj = PROJ_AREA) {
  transects <- data[, .(lat = LatStart, lon = LongStart)]
  coordinates(transects) <- ~ lon + lat
  transects <- SpatialPointsDataFrame(transects, data = data)
  proj4string(transects) <- CRS(prj)
  transects <- spTransform(transects, CRS(PROJ_LAEA))
  transects
}



getDensityModel <-
  function(data,
           grid,
           estimator,
           labelId = "Date") {
    if (!is.data.table(data)) {
      data <- as.data.table(data)
    }
    transects <- getTransects(data)
    
    # compute cell/zone area (km2) : needs lambert projection for distance
    tmp <- spTransform(grid, CRS(PROJ_LAMBERT))
    area <- gArea(tmp, byid = T) / 1000000
    grid$km2 <- area
    
    # select visited cells and intersect with shp
    inters <- gIntersects(transects, grid, byid = TRUE)
    ingrid <- vapply(1:nrow(inters), function(idx) {any(inters[idx,])}, FALSE)
    grid2 <- grid[ingrid, ]
    
    # Overlay transects and grid and attribute squares to observations
    x <- over(transects, grid2)
    data$square <- x$ID
    data$square_area <- x$km2
    data <- data[!is.na(data$square), ]
    
    data$SMP_LABEL <-
      paste(data$CruiseID, data$Date, data$square, sep = "_")
    
    # Construct sample labels considering that day transects can overlap with multiple squares
    temp <-
      aggregate(WatchLenKm ~ SMP_LABEL, data = unique(data[, c("SMP_LABEL", "WatchID", "WatchLenKm"), with = FALSE]), sum)
    names(temp)[2] <- "SMP_EFFORT"
    d <- merge(data, temp, by = "SMP_LABEL", sort = FALSE)
    
    d <-
      d[, c(
        "square",
        "square_area",
        "Date",
        "SMP_LABEL",
        "SMP_EFFORT",
        "Distance",
        "Count",
        "Alpha"
      ), with = FALSE]
    
    # Aggregate observations by label
    dd <- d[, .(V1 = sum(Count, na.rm = TRUE)), by = SMP_LABEL]
    # get the label name for transect without observations
    dd <- dd[V1 == 0, ]
    #keep only lines for empty transects or non-empty lines for non-empty transects
    d <-
      d[(d$SMP_LABEL %in% dd$SMP_LABEL & !duplicated(d$SMP_LABEL)) |
          (!d$SMP_LABEL %in% dd$SMP_LABEL & !(d$Alpha == "")), ]
    setkey(d, square)
    
    # distance sampling w/spatial stratification
    path <- ANALYSIS_DIR
    pathMCDS <- TOOLS_DIR
    breaks <- c(0, 50, 100, 200, 300)
    SMP_LABEL <- "SMP_LABEL"
    SMP_EFFORT <- "SMP_EFFORT"
    DISTANCE <- "Distance"
    SIZE <- "Count"
    STR_LABEL <- "square"
    STR_AREA <- "square_area"
    split <- F
    stratum <- "STR_LABEL"
    detection <- "All"
    empty <- NULL
    
    units = list(
      Type = "Line",
      Distance = "Perp",
      Length_units = "Kilometers",
      Distance_units = "Meters",
      Area_units = "Square kilometers"
    )
    
    #analyse - calcul des densites
    x <-
      mcds.wrap(
        d,
        estimator = estimator,
        units = units,
        stratum = stratum,
        empty = empty,
        detection = detection,
        split = split,
        path = path,
        pathMCDS = pathMCDS,
        breaks = breaks,
        STR_LABEL = STR_LABEL,
        STR_AREA = STR_AREA,
        SMP_LABEL = SMP_LABEL,
        SMP_EFFORT = SMP_EFFORT,
        DISTANCE = DISTANCE,
        SIZE = SIZE,
        verbose = FALSE
      )
    
    return(list(
      model = x,
      grid = grid,
      transects = transects
    ))
  }


getDensities <- function(model) {
  stratums <- model$density_estimate$Stratum
  densities <-
    stratums[stratums$Parameters == "D", c("Stratum", "Estimates", "SE", "% of var.")]
  names(densities) <- c("ID", "Estimates", "SE", "CoefVar")
  
  # save shp - grid + data associated to cells
  densities$Estimates <- as.numeric(densities$Estimates)
  densities$CoefVar <- as.numeric(densities$CoefVar)
  
  densities
}


compareDensities <- function(model1, model2) {
  d1 <- getDensities(model1$model)
  d2 <- getDensities(model2$model)
  d <- merge(d1, d2, by = "ID", all = TRUE)
  d$Estimates <- d$Estimates.x - d$Estimates.y
  d
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


compareModels <- function(subsetIds, input, userInfo) {
  models <- generateDensityModel(subsetIds, input, userInfo)
  model1 <- models[[subsetIds[1]]]$density
  model2 <- models[[subsetIds[2]]]$density
  newDensities <- compareDensities(model1, model2)
  list(densities = newDensities, grid = model1$grid, subsets = subsetIds, models = models)
}
