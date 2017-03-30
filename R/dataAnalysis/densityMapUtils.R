

createGrid <-
  function(transect,
           size = 50000,
           hexgrid = TRUE,
           shpm = NULL) {
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

createHexGrid <- function(x, width = 100000, seed = 111, convex = TRUE) {
  x <- spTransform(x, CRS(PROJ_LAEA))
  grid <- hexgrid(x, width = width, seed = seed, convex = convex)
  names(grid) <- "ID"
  # grid$ID <- seq(1, length(grid), by = 1)
  # grid$ID <- paste("parc", grid$ID, sep = "")
  grid
}



getBreaks <- function(densities, maxDensity) {
  #select variable to map
  temp <- densities[!is.na(densities$Estimates),]
  
  pos <- temp$Estimates[temp$Estimates > 0]
  neg <- temp$Estimates[temp$Estimates < 0]
  
  bp <- quantile(pos, c(.5, .75, .95))
  bn <- quantile(neg, c(.5, .75, .95))
  
  # Add and substract 0.1 to min/max because of rounding that can
  # mess with intervals
  bp <- c(bp, max(pos) + 0.1)
  if (any(is.na(bn))) {
    bn <- NULL
    bp <- c(0, bp)
  } else {
    bn <- c(min(neg) - 0.1, bn)
  }
  
  # combine all breaks
  breaks <- c(bn, bp)
  
  #needed for data below first rounded class
  breaks <- round(breaks, 1)
  breaks
}


getBreakTags <- function(breaks) {
  ##associate data w/breaks intervals
  tags <- vector()
  for (i in 2:length(breaks)) {
    tags[i - 1] <-
      paste(" > ", breaks[i - 1], " - ", breaks[i], sep = "")
  }
  # If no negative breaks, add 0 as a specific class
  if (!any(breaks < 0)) {
    tags <- c("0", tags)
  }
  tags
}



mapDensitiesToGrid <- function(densities, grid) {
  # join estimates to shp cells
  idx <- match(grid$ID, densities$ID)
  temp <- densities[idx,]
  row.names(temp) <- row.names(grid)
  grid@data <- join(grid@data, temp, by = "ID")
  
  # Remove unvisited cells
  estimates <- grid$Estimates[!is.na(grid$Estimates)]
  unvisited <- grid[is.na(grid$Estimates),]
  
  breaks <- getBreaks(densities, max(grid$Estimates, na.rm = TRUE))
  # Find in which interval estimates are. Add 1 because 0 is an interval in itself
  grid$classno <- findInterval(grid$Estimates, breaks)
  # If no negative breaks, add 0 as a specific class
  if (!any(breaks < 0)) {
    grid$classno <- grid$classno + 1
    grid$classno[grid$Estimates == 0] <- 1
  }
  tags <- getBreakTags(breaks)
  grid$class <- tags[grid$classno]
  
  grid <- grid[order(grid$classno),]
  
  grid$abundance <- grid$km2 * grid$Estimates
  
  return(list(
    breaks = breaks,
    grid = grid,
    unvisited = unvisited
  ))
}




getPaletteColors <- function(breaks) {
  #colors for legend
  colorRamp <- c("lightgray", "darkgreen")
  palette <- NULL
  if (any(breaks < 0)) {
    colorRamp <- c("darkred", colorRamp)
  } else {
    # add class 0
    palette <- "white"
  }
  br.palette <- colorRampPalette(colorRamp, space = "rgb")
  palette <- c(palette, br.palette(n = length(breaks) - 1))
  
  palette
}

plotDensityModel <- function(densityModel, ...) {
  plotDensityMap(
    getDensities(densityModel$model),
    densityModel$transects,
    densityModel$grid,
    ...
  )
}

plotDensityMap <-
  function(densities, transects, grid, shpm = NULL, lang = "fr", ...) {
    res <- mapDensitiesToGrid(densities, grid)
    
    grid <- res$grid
    unvisited <- res$unvisited
    breaks <- res$breaks
    
    # Earth background
    if (is.null(shpm)) {
      shpm <- readOGR(MAPS_DIR, LAND_MAP_LAYER)
    }

    # make sure data and map have the same projection
    prj <- proj4string(grid)
    shpm <- spTransform(shpm, CRS(prj))
    
    
    # comparisons
    sub1 <- sub2 <- NULL
    if (!is.null(grid$Estimates.x)) {
      sub1 <- unvisited[!is.na(unvisited$Estimates.x),]
      sub2 <- unvisited[!is.na(unvisited$Estimates.y),]
    }
    
    # Cells never visites
    # never <- unvisited[is.na(unvisited$Estimates.x) & is.na(unvisited$Estimates.y), ]
    palette <- getPaletteColors(breaks)
    classes <- unique(grid$class[!is.na(grid$class)])
    
    args <- list(...)
    legendTitle <-
      ifelse(is.null(args$legendTitle), geti18nValue("birds.density.legend", lang), args$legendTitle)
    legendCex <-
      ifelse(is.null(args$legendCex), 1, args$legendCex)
    transectsCex <-
      ifelse(is.null(args$transectsCex), .2, args$transectsCex)
    transectsCol <-
      ifelse(is.null(args$transectsCol), "darkgrey", args$transectsCol)
    subsetNames <- if (is.null(args$subsetNames)) {
      c("subset1", "subset2")
    } else {
      args$subsetNames
    }
    
    
    bounds <- bbox(grid)
    plot(
      grid,
      bg = hcl(240, 50, 66),
      axes = T,
      cex.axis = 1.5,
      xlim = bounds[1,] * 1.5
    )
    grid2 <- grid[!is.na(grid$class),]
    for (class in classes) {
      tmp <- grid2[grid2$class == class,]
      plot(tmp,
           col = palette[tmp$classno],
           border = "black",
           add = T)
    }
    l <- legend(
      "bottomright",
      bty = "n",
      legend = getBreakTags(breaks),
      fill = palette,
      title = legendTitle,
      cex = legendCex
    )
    
    ## Comparison only: transects visited in one subset but not the other
    if (!is.null(sub1) && !is.null(sub2)) {
      plot(sub1,
           col = "lightblue1",
           border = "black",
           add = T)
      plot(sub2,
           col = "lightblue3",
           border = "black",
           add = T)
      # get legend dimensions
      tl <- legend(plot = FALSE, 
                   x = "right",
                   bty = "n",
                   legend = subsetNames,
                   title = geti18nValue("visited.cells.legend", lang),
                   cex = legendCex)
      
      legend(
        x = l$rect$left - (tl$rect$w * 1.05 - l$rect$w),
        y = l$rect$top + tl$rect$h * 1.05,
        #coords, #"right",
        bty = "n",
        legend = subsetNames,
        fill = c("lightblue1", "lightblue3"),
        title = geti18nValue("visited.cells.legend", lang),
        cex = legendCex
      )
    }
    plot(
      shpm,
      add = T,
      border = "grey55",
      col = "darkkhaki"#alpha("darkkhaki", 0.65)
    )
    if (!is.null(transects)) {
      plot(
        transects,
        col = transectsCol,
        pch = 16,
        cex = transectsCex,
        add = T
      )
      legend(
        "bottomleft",
        bty = "n",
        cex = legendCex,
        legend = geti18nValue("transects.legend", lang),
        col = transectsCol,
        pch = 16
      )
    }
    return(grid2)
    
  }
