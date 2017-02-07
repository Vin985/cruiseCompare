

cleanDatabase <- function(d) {
  
  d$Alpha <- as.character(d$Alpha)
  d$Alpha[is.na(d$Alpha) |
             d$Alpha %in% c("RIEN", "NOBI")] <- ""

  groups <- read.csv("bird_groups.csv",
                     header = TRUE,
                     stringsAsFactors = FALSE)
  
  ### get data for french names from EC's official list on my github
  spname <- read.csv("EC_AVIAN_CORE_20161216.csv",
                     header = TRUE,
                     stringsAsFactors = FALSE)
  
  
  d$English <-
    spname$English_Name[match(d$Alpha, spname$Species_ID)]
  d$French <- spname$French_Name[match(d$Alpha, spname$Species_ID)]
  
  d$English[is.na(d$English)] <- ""
  d$French[is.na(d$French)] <- ""

  d$English[grep("Genus: Gulls", d$English)] <- "Genus: Gulls"
  m <- match(d$English, groups$sp)

  # If no species are in English because of an empty transect, we don't want ;a group name
  d$group_detection <-
    ifelse(!d$English %in% c("", NA), groups$group_detection[m], "")
  d$group_atlas <-
    ifelse(!d$English %in% c("", NA), groups$group_atlas[m], "")

  # Check what does not have a group name to make sure it is not an important species
  empty <- is.na(d$group_detection) | d$group_detection == ""
  sort(table(d$English[empty]))
  empty <- is.na(d$group_atlas) | d$group_detection == ""
  sort(table(d$English[empty]))

  # Turn what does not have a group to empty values in species, distance, groups and count

  k1 <- !d$Alpha %in% c("", NA) & d$group_detection %in% c("", NA)
  k2 <- !d$Alpha %in% c("", NA) & d$group_atlas %in% c("", NA)

  d$Alpha <- ifelse(k1 & k2, "", d$Alpha)
  d$English <- ifelse(k1 & k2, "", d$English)
  d$Count <- ifelse(k1 & k2, "", d$Count)
  d$Distance <- ifelse(k1 & k2, "", d$Distance)
  d$group_detection <- ifelse(k1 & k2, "", d$group_detection)
  d$group_atlas <- ifelse(k1 & k2, "", d$group_atlas)
  
  if (is.null(d$ObserverName)) {
    d$ObserverName <- as.character(d$ObserverID)
  } else {
    d$ObserverName <- as.character(d$ObserverName)
    d$ObserverID <- factor(d$ObserverName)
  }
  
  d$Count <- as.numeric(d$Count)
  
  d
}

is.empty <- function(x) {
  suppressMessages(standardGeneric(x))
}
is.empty.null <- function(x) {
  return(TRUE)
}
is.empty.logical <- function(x) {
  return(is.null(x))
}
is.empty.character <- function(x) {
  return(is.null(x) || length(x) == 0 || x == "")
}
is.empty.numeric <- function(x) {
  return(is.null(x) || length(x) == 0)
}
is.empty.data.frame <- function(x) {
  return(is.null(x) || nrow(x) == 0)
}

invisible(setMethod("is.empty", "NULL", is.empty.null))
invisible(setMethod("is.empty", "logical", is.empty.logical))
invisible(setMethod("is.empty", "character", is.empty.character))
invisible(setMethod("is.empty", "numeric", is.empty.numeric))
invisible(setMethod("is.empty", "list", is.empty.numeric))
invisible(setMethod("is.empty", "data.frame", is.empty.data.frame))
invisible(setMethod("is.empty", "SpatialPointsDataFrame", is.empty.data.frame))


selection2polygon <- function(selections) {
  lapply(selections, function(x) {
    locs <- x$geometry$coordinates
    Polygon(matrix(unlist(locs), ncol = 2, byrow = TRUE))
  })
}

drawSelections2sp <- function(selections, proj) {
  pols <- selection2polygon(selections)
  if (length(pols) > 0) {
    pols %>% Polygons(ID = 1) %>% list() %>% SpatialPolygons(proj4string =
                                                               CRS(proj))
  } else {
    NULL
  }
}

toSpatialDataframe <- function(data, proj) {
  sploc <- data.frame(lat = data$LatStart, lon = data$LongStart)
  coordinates(sploc) <- ~ lon + lat
  sploc <- SpatialPointsDataFrame(sploc, data = data)
  proj4string(sploc) <- CRS(proj)
  return(sploc)
}


## Recursively get through a list to concatenate all list names
getListNames <- function(list) {
  if (length(list) > 0) {
    unlist(lapply(seq_along(list), function(i, list) {
      # If list has names , use it, otherwise use index
      name <- ifelse(is.empty(names(list)), i, names(list)[i])
      # If element is a list, paste current name and recall the function
      if (is.list(list[[i]]) && length(list[[i]] > 0)) {
        paste(name, getListNames(list[[i]]), sep = ".")
      } else {
        # else return name
        name
      }
    }, list))
  } else {
    NULL
  }
}
