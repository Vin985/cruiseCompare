library(ECSASconnect)
library(GeoAviR)
require(MASS)
require(knitr)
require(rgdal)
library(png)
library(grid)
require(maptools)
library(RColorBrewer)
library(dplyr)


testmd <- function(Observation.df) {
  ##adapt species names
  Observation.df$English <- as.factor(Observation.df$English)
  
  if (length(grep("Genus", levels(Observation.df$English))) >= 1) {
    index.genus <- grep("Genus", levels(Observation.df$English))
    new.genus.labs <- sapply(1:length(index.genus), function(i) {
      paste(strsplit(levels(Observation.df$English)[index.genus[i]], ": ")[[1]][2], "sp.", "")
    })
    levels(Observation.df$English)[index.genus] <- new.genus.labs
    names(Observation.df$English)[index.genus] <-  new.genus.labs
  }
  
  if (length(grep("Family", levels(Observation.df$English))) >= 1) {
    index.family <- grep("Family", levels(Observation.df$English))
    new.family.labs <- sapply(1:length(index.family), function(i) {
      paste(strsplit(levels(Observation.df$English)[index.family[i]], ": ")[[1]][2], "sp.", "")
    })
    levels(Observation.df$English)[index.family] <- new.family.labs
    names(Observation.df$English)[index.family] <-  new.family.labs
  }
  
  freq.sp <- table(Observation.df$English)
  browser()
  ###agregation Densities
  total_km_watch.df <-
    aggregate(WatchLenKm ~ WatchID,
              data = Observation.df,
              FUN = mean,
              na.rm = T)
  total_birds_watch.df <-
    aggregate(Count ~ WatchID,
              data = Observation.df,
              FUN = sum,
              na.rm = T)
  total.df <-
    join(total_km_watch.df, total_birds_watch.df, by = "WatchID")
  ###fill watch without observations with zeroes
  total.df$Count[is.na(total.df$Count)] <- 0
  total.df$Densities <-  total.df$Count / total.df$WatchLenKm
  
  ###agregation species
  total_birds_sp.df <-
    aggregate(Count ~ English,
              data = Observation.df,
              FUN = sum,
              na.rm = T)
  #add factor CR
  total_birds_sp.df$English <- as.factor(total_birds_sp.df$English)
  mean_birds_sp.df <-
    aggregate(Count ~ English,
              data = Observation.df,
              FUN = mean,
              na.rm = T)
  #add factor CR
  mean_birds_sp.df$English <- as.factor(mean_birds_sp.df$English)
  sd_birds_sp.df <-
    aggregate(Count ~ English,
              data = Observation.df,
              FUN = sd,
              na.rm = T)
  #add factor CR
  sd_birds_sp.df$English <- as.factor(sd_birds_sp.df$English)
  
  ##group size
  birds.df <- join(mean_birds_sp.df, sd_birds_sp.df, by = "English")
  names(birds.df) <- c("English", "mean", "sd")
  birds.df$cv <- birds.df$sd / birds.df$mean
  ###change name for text
  total_birds_sp.df$English <- as.factor(total_birds_sp.df$English)
  
  #drop single observation species
  birds.df <- birds.df[!is.na(birds.df$cv), ]
  
  #descriptives
  observer <-
    paste(strsplit(levels(Observation.df$ObserverID), "_")[[1]][2], strsplit(levels(Observation.df$ObserverID), "_")[[1]][1], sep =
            " ")
  vessel <- levels(as.factor(Observation.df$PlatformText))
  date1 <-
    paste(substr(unique(Observation.df$StartDate), 9, 10), month.name[as.numeric(substr(unique(Observation.df$StartDate), 6, 7))], sep =
            " ")
  date2 <-
    paste(substr(unique(Observation.df$EndDate), 9, 10),
          month.name[as.numeric(substr(unique(Observation.df$EndDate), 6, 7))],
          substr(unique(Observation.df$StartDate), 1, 4),
          sep = " ")
  
  table1.df <-
    join(data.frame(English = names(freq.sp), Flocks = as.numeric(freq.sp)),
         total_birds_sp.df,
         by = "English")
  
  table1.df$mean <- round(table1.df$Count / table1.df$Flocks, 1)
  names(table1.df)[names(table1.df) == "Count"] <- "Birds"
  names(table1.df)[names(table1.df) == "mean"] <- "Mean flock size"
  table1.df <- table1.df[order(-table1.df$Birds), ]
  
  total_birds_bin.df <-
    aggregate(Count ~ Distance,
              data = Observation.df,
              FUN = sum,
              na.rm = T)
  ###extract order to test
  bin.order <-
    paste(order(total_birds_bin.df[, 2] * c(1, 1, 0.5, 0.5), decreasing = TRUE), collapse =
            "")
  
  possible.outcome <- c(
    paste(c(1, 2, 3, 4), collapse = ""),
    paste(c(2, 1, 3, 4), collapse = ""),
    paste(c(1, 2, 4, 3), collapse = ""),
    paste(c(2, 1, 4, 3), collapse = "")
  )
  
  qualifier <-
    c("excellent", "average", "good", "mediocre", "unsuitable")
  
  ###table observation
  Dist_sp.tbl <- table(Observation.df$English, Observation.df$Distance)
  
  #Id species with enough info
  keep.sp <- which(apply(Dist_sp.tbl, 1, sum) > 50)
  ###qualify detection process
  bin.sp <-
    sapply(1:length(keep.sp),  function(i)
      paste(order(
        Dist_sp.tbl[keep.sp[i], ] * c(1, 1, 0.5, 0.5), decreasing = TRUE
      ), collapse = ""))
  qualify.sp <-
    sapply(1:length(keep.sp), function(i)
      qualifier[match(bin.sp[i], possible.outcome)[1]])
  
  
  Observation.df$STR_AREA <- sum(total_km_watch.df$WatchLenKm) * 0.3
  
  
  browser()
  
  all.dist <- distance.wrap(
    Observation.df,
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
    path = "analysis/temp",
    pathMCDS = "tools",
    verbose = FALSE
  )
  
  
  
  all.sp.best <- keep.best.model(all.dist)
  
  model.names <-
    c("uniform",
      "uniform",
      "half-normal",
      "half-normal",
      "hazard rate",
      "hazard rate")
  adj.names <-
    c("cosinus",
      "polynomial",
      "cosinus",
      "hermite polynomial",
      "cosinus",
      "polynomial")
  mod.selected <-
    which.min(sapply(1:6, function(i)
      all.dist[[i]]$AIC[3]))
  
  ####Extract the probability of detection
  table3.df <- all.sp.best$parameter_estimates$Global[, -1]
  table3.df[, -1] <- round(table3.df[, -1], 2)
  
  p.line <- which(table3.df$Parameters == "p")
  
  ###extract prediction
  table4.df <- all.sp.best$density_estimate$Global
  table4.df[, -c(1, 2)] <- round(table4.df[, -c(1, 2)], 2)
  d.line <- which(table4.df$Parameters == "D")
  N.line <- which(table4.df$Parameters == "N")
  
}
