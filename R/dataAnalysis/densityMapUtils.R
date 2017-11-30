

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

getQuantiles <- function(data, quantiles) {
  res <- 0
  # If there is data, get quantiles, otherwise return 0
  if (length(data) > 0) {
    res <- quantile(data, c(.5, .75, .95))
  }
  res
}

getBreaks <- function(densities, maxDensity) {
  #select variable to map
  temp <- densities[!is.na(densities$Estimates),]

  pos <- temp$Estimates[temp$Estimates > 0]
  neg <- temp$Estimates[temp$Estimates < 0]

  bp <- getQuantiles(pos, c(.5, .75, .95))
  bn <- getQuantiles(neg, c(.5, .75, .95))


  # Add and substract 0.1 to min/max because of rounding that can
  # mess with intervals
  if (length(bp) > 1) {
    bp <- c(bp, max(pos) + 0.1)
  }
  if (length(bn) > 1) {
    bn <- c(min(neg) - 0.1, bn)
  }

  # bp <- c(bp, max(pos) + 0.1)
  # if (any(is.na(bn))) {
  #   bn <- 0
  # } else {
  #   bn <- c(min(neg) - 0.1, bn)
  # }

  # combine all breaks
  breaks <- unique(c(bn, bp))

  #needed for data below first rounded class
  breaks <- round(breaks, 1)
  breaks
}


getBreakTags <- function(breaks) {
  ##associate data w/breaks intervals
  tags <- vector()
  if (length(breaks) > 1) {
    for (i in 2:length(breaks)) {
      tags[i - 1] <-
        paste(" > ", breaks[i - 1], " - ", breaks[i], sep = "")
    }
    # If no negative breaks, add 0 as a specific class
    if (!any(breaks < 0)) {
      tags <- c("0", tags)
    }
  } else {
    tags <- 0
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

  breaks <- NULL
  if (length(estimates > 0)) {
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
  }
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
    } else {
      # don't display unvisited cells if no comparison
      grid <- grid[!is.na(grid$Estimates), ]
    }

    args <- list(...)
    legendTitle <-
      ifelse(is.null(args$legendTitle), geti18nValue("birds.density.legend", lang), args$legendTitle)
    legendCex <-
      ifelse(is.null(args$legendCex), 1, args$legendCex)
    transectsCex <-
      ifelse(is.null(args$transectsCex), .2, args$transectsCex)
    transectsCol <-
      ifelse(is.null(args$transectsCol), "darkred", args$transectsCol)
    subsetNames <- if (is.null(args$subsetNames)) {
      c("subset1", "subset2")
    } else {
      args$subsetNames
    }





    bounds <- bbox(grid)
    xlegend <- "bottomright"
    ylegend <- NULL
    plot(
      grid,
      bg = hcl(240, 50, 66),
      cex.axis = 1.5,
      xlim = bounds[1,] * 1.5
    )
    if (!is.null(breaks)) {
      palette <- getPaletteColors(breaks)
      classes <- unique(grid$class[!is.na(grid$class)])

      grid2 <- grid[!is.na(grid$class),]
      for (class in classes) {
        tmp <- grid2[grid2$class == class,]
        plot(tmp,
             col = palette[tmp$classno],
             border = "black",
             add = T)
      }
      leg <- legend(
        xlegend,
        bty = "n",
        legend = getBreakTags(breaks),
        fill = palette,
        title = legendTitle,
        cex = legendCex
      )
    }
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
      if (exists("leg")) {
        xlegend <- leg$rect$left - (tl$rect$w * 1.05 - leg$rect$w)
        ylegend <- leg$rect$top + tl$rect$h * 1.05
      }
      legend(
        x = xlegend,
        y = ylegend,
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



    # boxcut <- par("usr")
    # xxlat <- boxcut[1]
    # m <- expand.grid(xxlat, seq(boxcut[3], boxcut[4], length.out = 100))
    # p <- SpatialPoints(m, proj4string = CRS(proj4string(grid)))
    # p2 <- spTransform(p, CRS(DENSITY_MAP_PROJ))
    # r <- range(coordinates(p2)[, 2])
    # selat <- pretty(r)
    # if (selat[1] < r[1])
    #   selat <- selat[-1]
    # if (selat[length(selat)] > r[2])
    #   selat <- selat[-length(selat)]
    # yylat <- sapply(selat, function(k) {
    #   coordinates(p)[which.min(abs(coordinates(p2)[, 2] - k)), 2]
    # })
    #
    #
    #
    # yylon <- boxcut[3] + 100000#par("usr")[3]
    # m <- expand.grid(seq(boxcut[1], boxcut[2], by = 100), yylon)
    # p <- SpatialPoints(m, proj4string = CRS(proj4string(grid)))
    # p2 <- spTransform(p, CRS(DENSITY_MAP_PROJ))
    # r <- range(coordinates(p2)[, 1])
    # selon <- pretty(r)
    # xxlon <- sapply(selon, function(k) {
    #   coordinates(p)[which.min(abs(coordinates(p2)[, 1] - k)), 1]
    # })

    xt <- getAxisTicks("x", proj4string(grid))
    yt <- getAxisTicks("y", proj4string(grid))

    xticks <- parse(text = paste0(abs(xt), "*degree ~ W"))
    yticks <- parse(text = paste0(yt, "*degree ~ N"))

    axis(1, at = as.numeric(names(xt)), labels = xticks, cex.axis = 1.5)
    axis(2, at = as.numeric(names(yt)), labels = yticks, cex.axis = 1.5)

    return(grid)

  }

getAxisTicks <- function(type, gridProj) {
  boxcut <- par("usr")
  if (type == "x") {
    bounds <- boxcut[1:2]
    m <- expand.grid(seq(bounds[1], bounds[2], length.out = 100), 1)
    i <- 1
  } else {
    bounds <- boxcut[3:4]
    m <- expand.grid(1, seq(bounds[1], bounds[2], length.out = 100))
    i <- 2
  }

  p <- SpatialPoints(m, proj4string = CRS(gridProj))
  p2 <- spTransform(p, CRS(DENSITY_MAP_PROJ))
  r <- range(coordinates(p2)[, i])
  ticks <- pretty(r, n = 7)

  old <- sapply(ticks, function(k) {
    coordinates(p)[which.min(abs(coordinates(p2)[, i] - k)), i]
  })

  # if ticks are outside range, then draw it outside the plot
  # this is to ensure axes are drawn all along the plot and not only
  # between ticks
  if (ticks[1] < r[1])
    old[1] <- bounds[1] - 100
  if (ticks[length(ticks)] > r[2])
    old[length(ticks)] <- bounds[2] + 100

  names(ticks) <- old



  return(ticks)
}
