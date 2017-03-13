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

DENSITY_MAP_PROJ <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
LAMBERT_PROJ <- "+proj=lcc +lat_1=46 +lat_2=60 +lat_0=44 +lon_0=-68.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"


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


analyzeData <- function(data) {
  data <- droplevels(data)
  dt <- data.table(data)
  setkey(dt, WatchID)
  
  dt$Alpha[dt$Alpha == ""] <- NA
  dt$English[dt$English == ""] <- NA
  
  ##adapt species names
  dt$English <- as.factor(dt$English)
  dt$Alpha <- as.factor(dt$Alpha)
  
  # if (length(grep("Genus", levels(Observation.df$English))) >= 1) {
  #   index.genus <- grep("Genus", levels(Observation.df$English))
  #   new.genus.labs <- sapply(1:length(index.genus), function(i) {
  #     paste(strsplit(levels(Observation.df$English)[index.genus[i]], ": ")[[1]][2], "sp.", "")
  #   })
  #   levels(Observation.df$English)[index.genus] <- new.genus.labs
  #   names(Observation.df$English)[index.genus] <-  new.genus.labs
  # }
  #
  # if (length(grep("Family", levels(Observation.df$English))) >= 1) {
  #   index.family <- grep("Family", levels(Observation.df$English))
  #   new.family.labs <- sapply(1:length(index.family), function(i) {
  #     paste(strsplit(levels(Observation.df$English)[index.family[i]], ": ")[[1]][2], "sp.", "")
  #   })
  #   levels(Observation.df$English)[index.family] <- new.family.labs
  #   names(Observation.df$English)[index.family] <-  new.family.labs
  # }
  
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
    sd = sd(Count, na.rm = T)
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
  if (!is.data.table(dt)) {
    dt <- as.data.table(dt)
  }
  total <- dt[, .(WatchLenKm = mean(WatchLenKm, na.rm = TRUE)),
              by = WatchID]
  total <- total[!is.na(WatchLenKm)]
  
  # Multiply by 0.3 because we have linear transects 300m wide
  dt <- droplevels(dt)
  dt[, STR_AREA := sum(total$WatchLenKm) * 0.3]
  
  all.dist <- distance.wrap(
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
  
  key <- sapply(strsplit(names(all.dist)[mod.selected], "_"), "[", 2)
  exp <- sapply(strsplit(names(all.dist)[mod.selected], "_"), "[", 3)
  estimator = list(c(key, exp))
    
  return(list(idx = mod.selected, model = all.sp.best, estimator = estimator))
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


createGrid <- function(transect, size = 50000, hexgrid = TRUE, shpm = NULL) {
  # create study area using a bounding box
  b <- bbox(transect)
  # need larger buffer than just min max, with 50 km grid cell
  b[, 2] <-  b[, 2] + 1
  b[, 1] <-  b[, 1] - 1
  
  # Build grid
  grid <-
    create.grid(
      Latitude = c(b[2, 1], b[2, 2]),
      Longitude = c(b[1, 1], b[1, 2]),
      Grid.size = c(size, size),
      Clip = F,
      clip.shape = shpm,
      projection = CRS(proj4string(transect)),
      hexgrid = hexgrid
    )
  grid$ID <- paste("parc", grid$ID, sep = "")
  grid
}


getTransects <- function(data, prj = DENSITY_MAP_PROJ) {
  transects <- data[, .(lat = LatStart, lon = LongStart)]
  coordinates(transects) <- ~ lon + lat
  transects <- SpatialPointsDataFrame(transects, data = data)
  proj4string(transects) <- CRS(prj)
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
    tmp <- spTransform(grid, CRS(LAMBERT_PROJ))
    area <- gArea(tmp, byid = T) / 1000000
    grid$km2 <- area
    
    # select visited cells and intersect with shp
    grid2 <-
      grid[apply(gIntersects(transects, grid, byid = TRUE), 1, any),]
    
    # Overlay transects and grid and attribute squares to observations
    x <- over(transects, grid2)
    data$square <- x$ID
    data$square_area <- x$km2
    data <- data[!is.na(data$square),]
    
    data$SMP_LABEL <-
      paste(data$CruiseID, data$Date, data$square, sep = "_")
    
    # Construct sample labels considering that day transects can overlap with multiple squares
    temp <-
      aggregate(WatchLenKm ~ SMP_LABEL, data = unique(data[, c("SMP_LABEL", "WatchID", "WatchLenKm")]), sum)
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
      )]
    
    # Aggregate observations by label
    dd <- d[, .(V1 = sum(Count, na.rm = TRUE)), by = SMP_LABEL]
    # get the label name for transect without observations
    dd <- dd[V1 == 0,]
    #keep only lines for empty transects or non-empty lines for non-empty transects
    d <- d[(d$SMP_LABEL %in% dd$SMP_LABEL & !duplicated(d$SMP_LABEL)) |
           (!d$SMP_LABEL %in% dd$SMP_LABEL & !(d$Alpha == "")),] 
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
      distance.wrap(
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
    
    return(list(model = x, grid = grid, transects = transects))
  }


getDensities <- function(model) {
  stratums <- model$density_estimate$Stratum
  densities <-
    stratums[stratums$Parameters == "D", c("Stratum", "Estimates", "% of var.")]
  names(densities) <- c("ID", "Estimates", "CoefVar")
  
  # save shp - grid + data associated to cells
  densities$Estimates <- as.numeric(densities$Estimates)
  densities$CoefVar <- as.numeric(densities$CoefVar)

  densities  
}
  

mapDensitiesToGrid <- function(densityModel) {
  
  densities <- getDensities(densityModel$model)
  grid <- densityModel$grid
  
  # join estimates to shp cells
  idx <- match(grid$ID, densities$ID)
  temp <- densities[idx, ]
  row.names(temp) <- row.names(grid)
  grid@data <- join(grid@data, temp, by = "ID")
  
  # browser()
  
  # Remove unvisited cells
  estimates <- grid$Estimates[!is.na(grid$Estimates)]
  unvisited <- grid[is.na(grid$Estimates), ]
  
  breaks <- getBreaks(densities, max(grid$Estimates, na.rm = TRUE))
  # Find in which interval estimates are. Add 1 because 0 is an interval in itself
  grid$classno <- findInterval(grid$Estimates, breaks) + 1
  grid$classno[grid$Estimates == 0] <- 1
  
  tags <- getBreakTags(breaks)
  grid$class <- tags[grid$classno]
  
  grid <- grid[order(grid$classno), ]
  
  grid$abundance <- grid$km2 * grid$Estimates
  
  return(list(breaks = breaks, grid = grid, univisited = unvisited))
}


getBreaks <- function(densities, maxDensity) {
  #select variable to map
  temp <- densities[!is.na(densities$Estimates) & densities$Estimates > 0, ]
  breaks <- quantile(temp$Estimates, c(0, .5, .75, .95))
  
  #needed for data below first rounded class
  breaks[1] <- 0
  breaks[length(breaks) + 1] <- maxDensity + 0.1
  breaks <- round(breaks, 1)
  breaks
}


getBreakTags <- function(breaks) {
  ##associate data w/breaks intervals
  tags <- vector()
  for (i in 1:length(breaks)) {
    if (i == 1) {
      tags[1] <- as.character(breaks[i])
    } else {
      tags[i] <- paste(" > ", breaks[i - 1], " - ", breaks[i], sep = "")
    }
  }
  tags
}


getPaletteColors <- function(breaks) {
  #colors for legend
  br.palette <- colorRampPalette(c("green", "yellow", "red"), space = "rgb")
  palette <- br.palette(n = length(breaks) - 1)
  #ajout class 0
  palette <- c("lightgray", palette)
  palette
}


plotDensityMap <- function(densityModel, shpm = NULL, ...) {
  transects <- densityModel$transects
  # browser()
  res <- mapDensitiesToGrid(densityModel)
  
  grid <- res$grid
  unvisited <- res$unvisited
  breaks <- res$breaks
  
  # Earth background
  if (is.null(shpm)) {
    shpm <- readOGR(MAPS_DIR, "ne_10m_land")
  }
  
  # make sure data and map have the same projection
  prj <- proj4string(transects)
  shpm <- spTransform(shpm, CRS(prj))
  
  
  palette <- getPaletteColors(breaks)
  classes <- unique(grid$class[!is.na(grid$class)])
  
  args <- list(...)
  legendTitle <- ifelse(is.null(args$legendTitle), "birds/km2", args$legendTitle)
  plot(
    grid,
    bg = "lightblue",
    border = "darkgray",
    axes = T,
    cex.axis = 1.5
  )
  grid2 <- grid[!is.na(grid$class), ]
  for (class in classes) {
    tmp <- grid2[grid2$class == class, ]
    plot(tmp,
         col = palette[tmp$classno],
         border = "darkgray",
         add = T)
  }
  plot(
    shpm,
    add = T,
    col = "darkkhaki",
    border = "darkkhaki",
    axes = T
  )
  plot(
    transects,
    col = "black",
    pch = 16,
    cex = .5,
    add = T
  )
  legend(
    "bottomright",
    bty = "n",
    legend = getBreakTags(breaks),
    fill = palette,
    title = legendTitle,
    cex = 1.5
  )
  legend(
    "bottomleft",
    bty = "n",
    cex = 1.5,
    legend = c("Locations of 5-min observation periods"),
    col = "darkgray",
    pch = 16
  )
  
}
  
  
