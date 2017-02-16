


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
library(data.table)

Observation.df <- droplevels(spdata@data)
Observation.dt <- data.table(Observation.df)
setkey(Observation.dt, WatchID)

Observation.dt$Alpha[Observation.dt$Alpha == ""] <- NA
Observation.dt$English[Observation.dt$English == ""] <- NA

##adapt species names
Observation.df$English <- as.factor(Observation.df$English)
Observation.df$Alpha <- as.factor(Observation.df$Alpha)

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

freq.sp <- table(Observation.df$English, useNA = "no")

###agregation Densities
total.dt <- Observation.dt[, .(WatchLenKm = mean(WatchLenKm, na.rm = TRUE),
                               Count = sum(Count, na.rm = TRUE)),
                           by = WatchID]
total.dt[is.na(Count)] <- 0
total.dt[, Densities := Count / WatchLenKm]


###agregation species
birds.dt <- Observation.dt[!is.na(English), .(Count = sum(Count, na.rm = T),
                               mean = mean(Count, na.rm = T),
                               sd = sd(Count, na.rm = T)), by = English]
birds.dt[, cv := sd / mean]
birds.dt <- birds.dt[!is.na(cv)]
setkey(birds.dt, English)

#descriptives
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
  names <- lapply(split_obs, function(x){paste(x[2], x[1])})
  # Combine all observers
  return(paste(names, collapse = ", "))
}
observer <- getObserverNames(Observation.dt$ObserverID)

# TO CHANGE??
vessel <- levels(as.factor(Observation.df$PlatformText))

date1 <-
  paste(substr(unique(Observation.df$StartDate), 9, 10), month.name[as.numeric(substr(unique(Observation.df$StartDate), 6, 7))], sep =
          " ")

date2 <-
  paste(substr(unique(Observation.df$EndDate), 9, 10), month.name[as.numeric(substr(unique(Observation.df$EndDate), 6, 7))], substr(unique(Observation.df$StartDate), 1, 4), sep =
          " ")

birds.dt[, flocks := freq.sp[match(birds.dt$English, names(freq.sp))]]

table1.df <- join(data.frame(English = names(freq.sp), Flocks = as.numeric(freq.sp)),
                  total_birds_sp.df, by="English")

table1.df$mean <- round(table1.df$Count/table1.df$Flocks,1)
names(table1.df)[names(table1.df)=="Count"]<-"Birds"
names(table1.df)[names(table1.df)=="mean"]<-"Mean flock size"
table1.df <- table1.df[order(-table1.df$Birds),]
