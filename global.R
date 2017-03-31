
rm(list = ls())

# detachAllPackages <- function() {
#   basic.packages <-
#       c(
#           "package:stats",
#           "package:graphics",
#           "package:grDevices",
#           "package:utils",
#           "package:datasets",
#           "package:methods",
#           "package:base"
#       )
#   
#   package.list <-
#       search()[ifelse(unlist(gregexpr("package:", search())) == 1, TRUE, FALSE)]
#   
#   package.list <- setdiff(package.list, basic.packages)
#   
#   if (length(package.list) > 0)
#     for (package in package.list)
#       detach(package, character.only = TRUE)
#   
# }
# 
# detachAllPackages()

library(shiny)
library(plyr)
library(dplyr)
library(dtplyr)
library(data.table)
library(MASS)
library(knitr)
library(rgdal)
library(png)
library(grid)
library(RColorBrewer) 
library(leaflet)
library(leaflet.extras)
library(logging)
library(maptools)
library(FRutils)
library(ECSASconnect)
library(R2MCDS)
library(ecapputils)
library(DT)


ROOT_DIR <- "C:/dev/cruiseCompare"

SRC_DIR <- file.path(ROOT_DIR, "R")
DEST_DIR <- file.path(ROOT_DIR, "dest")
DATA_DIR <- file.path(ROOT_DIR, "../data")

## Subfolders
FILTERS_SRC_DIR <- file.path(SRC_DIR, "filters")
DATA_ANALYSIS_SRC_DIR <- file.path(SRC_DIR, "dataAnalysis")
PAGES_SRC_DIR <- file.path(SRC_DIR, "pages")

ASSETS_DIR <- file.path(ROOT_DIR, "assets")
MAPS_DIR <- file.path(ASSETS_DIR, "maps")
REPORTS_DIR <- file.path(ASSETS_DIR, "reports")
TOOLS_DIR <- file.path(ASSETS_DIR, "tools")
ANALYSIS_DIR <- file.path(DEST_DIR, "analysis")


setwd(ROOT_DIR)

## Utilities
source(file.path(SRC_DIR, "utils.R"))
source(file.path(SRC_DIR, "filterUtils.R"))
source(file.path(SRC_DIR, "subsetUtils.R"))
source(file.path(SRC_DIR, "distanceUtils.R"))


## Filters
source(file.path(FILTERS_SRC_DIR, "regionFilter.R"))
source(file.path(FILTERS_SRC_DIR, "speciesFilter.R"))
source(file.path(FILTERS_SRC_DIR, "dateFilter.R"))
source(file.path(FILTERS_SRC_DIR, "observerFilter.R"))

## Data Analysis
source(file.path(DATA_ANALYSIS_SRC_DIR, "dataAnalysisUtils.R"))
source(file.path(DATA_ANALYSIS_SRC_DIR, "densityMap.R"))
source(file.path(DATA_ANALYSIS_SRC_DIR, "densityMapUtils.R"))

source(file.path(SRC_DIR, "subsetTabs.R"))
source(file.path(SRC_DIR, "createReport.R"))
source(file.path(SRC_DIR, "filters.R"))


## Pages
source(file.path(PAGES_SRC_DIR, "selectionPage.R"))
source(file.path(PAGES_SRC_DIR, "viewDataPage.R"))

logReset()
logging::basicConfig("INFO")


## Events
CHANGE_LANG_EVENT <- "EVENT_changeLang"
CHANGE_PAGE_EVENT <- "EVENT_changePage"
CHANGE_SUBSET_EVENT <- "EVENT_changeSubset"
SUBSET_DATA_EVENT <- "EVENT_subsetData"


## Load Language file
LANG_DATA <- loadLanguages(file.path(ASSETS_DIR, "lang.csv"))


PROJ_AREA <-
    "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
PROJ_LAEA <- "+proj=laea +lat_0=50 +lon_0=-65"
DENSITY_MAP_PROJ <-
  "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
PROJ_LAMBERT <-
  "+proj=lcc +lat_1=46 +lat_2=60 +lat_0=44 +lon_0=-68.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"




### Extract ECSAS Data
# ECSAS_DATA_PATH <- file.path(DATA_DIR, "ECSAS")
# ECSAS_DB_FILE <- "ECSAS.mdb"
# 
# ALPHA_LIST_FILE <- file.path(DATA_DIR, "alpha_code.csv")
# alpha.ecsas <- read.table(ALPHA_LIST_FILE, header = T, sep = ",")
# ecsas <- ECSAS.extract(
#   ecsas.drive = DATA_DIR,
#   ecsas.file = ECSAS_DB_FILE
# )
# 
# save(ecsas, file = file.path(DATA_DIR, "ECSAS/ECSAS_raw.Rdata"))
# 
# ecsas <- mcds.filter(ecsas, dist2m = FALSE, distanceLabel.field = "DistanceCode")
# ecsas$Date <- strptime(ecsas$Date,"%Y-%m-%d")
# ecsas$Date <- as.Date(ecsas$Date)
# save(ecsas, file = file.path(ASSETS_DIR, "ECSAS_filtered.Rdata"))


load(file.path(ASSETS_DIR, "ECSAS_filtered.Rdata"))
ecsas <- cleanDatabase(ecsas)
spdata <- toSpatialDataframe(ecsas, PROJ_AREA)


LAND_MAP_LAYER <- "ne_10m_land"
ALL_MAP_SHP <- readOGR(MAPS_DIR, LAND_MAP_LAYER)
databox <- bbox(spdata)
databox <- databox + c(0, -20, 20, 10) 
LAND_MAP_SHP <- gIntersection(bbox2pol(databox, proj4string = PROJ_AREA), ALL_MAP_SHP, byid = T)

# Default grid size for distance anlaysis in km
DEFAULT_GRIDSIZE <- 100

## Get the full dataset from reactive values
getFullData <- function(as.df = FALSE) {
  if (as.df) {
    spdata@data 
  } else {
    spdata
  }
}
