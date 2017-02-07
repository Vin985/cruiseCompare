
rm(list = ls())

detachAllPackages <- function() {
  basic.packages <-
    c(
      "package:stats",
      "package:graphics",
      "package:grDevices",
      "package:utils",
      "package:datasets",
      "package:methods",
      "package:base"
    )

  package.list <-
    search()[ifelse(unlist(gregexpr("package:", search())) == 1, TRUE, FALSE)]

  package.list <- setdiff(package.list, basic.packages)

  if (length(package.list) > 0)
    for (package in package.list)
      detach(package, character.only = TRUE)

}

detachAllPackages()

library(ECSASconnect)
library(GeoAviR)
library(shiny)
require(MASS)
require(knitr)
require(rgdal)
library(png)
library(grid)
library(RColorBrewer) 
library(leaflet)
library(leaflet.extras)
library(logging)
library(maptools)
library(dplyr)
library(ecapputils)

ROOT_DIR <- "C:/dev/cruiseCompare"
DATA_DIR <- file.path(ROOT_DIR, "../data")

setwd(ROOT_DIR)


source("utils.R")
source("changeSubsets.R")
source("filters.R")
source("createReport.R")
source("testmd.R")
# source("../GeoAviR/R/distance.filter.R")

logReset()
logging::basicConfig("INFO")


## EVENTS
CHANGE_LANG_EVENT <- "EVENT_changeLang"
CHANGE_SUBSET_EVENT <- "EVENT_changeSubset"
SUBSET_DATA_EVENT <- "EVENT_subsetData"

## Load Language file
LANG_DATA <- loadLanguages("lang.csv")


PROJ_AREA <-
  "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"



### Extract ECSAS Data
# ECSAS_DATA_PATH <- file.path(DATA_DIR, "ECSAS")
# ECSAS_DB_FILE <- "ECSAS.mdb"
# 
# # ALPHA_LIST_FILE <- file.path(DATA_DIR, "alpha_code.csv")
# # alpha.ecsas <- read.table(ALPHA_LIST_FILE, header = T, sep = ",")
# ecsas <- ECSAS.extract(
#   ecsas.drive = DATA_DIR,
#   ecsas.file = ECSAS_DB_FILE
# )
# 
# save(ecsas, file = file.path(DATA_DIR, "ECSAS/ECSAS_raw.Rdata"))
# 
# ecsas <- distance.filter(ecsas, dist2m = FALSE, distanceLabel.field = "DistanceCode")
# save(ecsas, file = file.path(DATA_DIR, "ECSAS/ECSAS_filtered.Rdata"))


load(file.path(DATA_DIR, "ECSAS/ECSAS_filtered.RData"))
ecsas <- cleanDatabase(ecsas)
spdata <- toSpatialDataframe(ecsas, PROJ_AREA)


## Get the full dataset from reactive values
getFullData <- function(as.df = FALSE) {
  if (as.df) {
    spdata@data 
  }else {
    spdata
  }
}

# noga <- ecsas[ecsas$Alpha == "NOGA",]
# noga <- noga[!is.na(noga$LatStart) & !is.na(noga$LongStart), ]
# noga <- toSpatialDataframe(noga, PROJ_AREA)
