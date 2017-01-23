
rm(list = ls())

library(shiny)
library(dplyr)
library(leaflet)
library(leaflet.extras)
library(logging)
library(maptools)
library(ecapputils)

source("utils.R")
source("changeSubsets.R")
source("filters.R")

logReset()
logging::basicConfig("INFO")


## EVENTS
CHANGE_LANG_EVENT <- 1
CHANGE_SUBSET_EVENT <- 2
SUBSET_DATA_EVENT <- 3

LANG_DATA <- loadLanguages("lang.csv")


ROOT_DIR <- "C:/dev/cruiseYearly"
DATA_DIR <- file.path(ROOT_DIR, "../data")

PROJ_AREA <-
  "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"


load(file.path(DATA_DIR, "ECSAS/ECSASAtlas.RData"))
ecsas <- cleanDatabase(ecsas)
spdata <- toSpatialDataframe(ecsas, PROJ_AREA)


## Get the full dataset from reactive values
getFullData <- function() {
  spdata
}

# noga <- ecsas[ecsas$Alpha == "NOGA",]
# noga <- noga[!is.na(noga$LatStart) & !is.na(noga$LongStart), ]
# noga <- toSpatialDataframe(noga, PROJ_AREA)
