
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
library(ECSASconnect)
library(GeoAviR)
library(ecapputils)
library(DT)
library(FRutils)

ROOT_DIR <- "C:/dev/cruiseCompare"

SRC_DIR <- file.path(ROOT_DIR, "R")
DEST_DIR <- file.path(ROOT_DIR, "dest")
DATA_DIR <- file.path(ROOT_DIR, "../data")

ASSETS_DIR <- file.path(ROOT_DIR, "assets")
MAPS_DIR <- file.path(ASSETS_DIR, "maps")
TOOLS_DIR <- file.path(ASSETS_DIR, "tools")
ANALYSIS_DIR <- file.path(DEST_DIR, "analysis/temp")


setwd(ROOT_DIR)

## Utilities
source(file.path(SRC_DIR, "utils.R"))
source(file.path(SRC_DIR, "dataAnalysisUtils.R"))
source(file.path(SRC_DIR, "filterUtils.R"))
source(file.path(SRC_DIR, "subsetUtils.R"))


## Filters
source(file.path(SRC_DIR, "filters/regionFilter.R"))
source(file.path(SRC_DIR, "filters/speciesFilter.R"))
source(file.path(SRC_DIR, "filters/dateFilter.R"))
source(file.path(SRC_DIR, "filters/observerFilter.R"))


source(file.path(SRC_DIR, "subsetTabs.R"))
source(file.path(SRC_DIR, "createReport.R"))
source(file.path(SRC_DIR, "filters.R"))
source(file.path(SRC_DIR, "testmd.R"))


## Pages
source(file.path(SRC_DIR, "pages/selectionPage.R"))
source(file.path(SRC_DIR, "pages/viewDataPage.R"))

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
# ecsas2 <- distance.filter(ecsas, dist2m = FALSE, distanceLabel.field = "DistanceCode")
# save(ecsas, file = file.path(DATA_DIR, "ECSAS/ECSAS_filtered.Rdata"))


load(file.path(ASSETS_DIR, "ECSAS_filtered.RData"))
ecsas <- cleanDatabase(ecsas)
spdata <- toSpatialDataframe(ecsas, PROJ_AREA)


## Get the full dataset from reactive values
getFullData <- function(as.df = FALSE) {
  if (as.df) {
    spdata@data 
  } else {
    spdata
  }
}
