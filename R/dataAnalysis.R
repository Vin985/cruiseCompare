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
library(dtplyr)

source(file.path(SRC_DIR, "dataAnalysisUtils.R"))

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

freq.sp <- table(Observation.dt$English, useNA = "no")

### Total densities
total.dt <- Observation.dt[, .(WatchLenKm = mean(WatchLenKm, na.rm = TRUE),
                               Count = sum(Count, na.rm = TRUE)),
                           by = WatchID]
total.dt[is.na(Count)] <- 0
total.dt[, Densities := Count / WatchLenKm]


### Species descriptive stats
birds.dt <- Observation.dt[!is.na(English), .(Count = sum(Count, na.rm = T),
                               mean = mean(Count, na.rm = T),
                               sd = sd(Count, na.rm = T)), by = English]
birds.dt[, cv := sd / mean]
birds.dt <- birds.dt[!is.na(cv)]
setkey(birds.dt, English)

#descriptives

observer <- getObserverNames(Observation.dt$ObserverID)

# TO CHANGE??
vessel <- levels(as.factor(Observation.df$PlatformText))

birds.dt[, Flocks := freq.sp[match(birds.dt$English, names(freq.sp))]]
birds.dt[, meanFlock := round(Count / Flocks, 1)]



Observation.dt[, STR_AREA := sum(WatchLenKm) * 0.3]

all.dist <- distance.wrap(
  Observation.dt,
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

pred.df <-
  data.frame(x = all.sp.best$detection[['Global']][, 'distance'],
             y = all.sp.best$detection[['Global']][, 'predicted'])

fig3 <- predicted_hist(all.sp.best)

png("detection.png",
    width = 480,
    height = 480,
    units = "px")
print(fig3)
invisible(dev.off())

d <- Observation.df
transect <- data.frame(lat = d$LatStart, lon = d$LongStart)
coordinates(transect) <- ~ lon + lat
transect <- SpatialPointsDataFrame(transect, data = d)
proj4string(transect) <-
  CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")



#### earth background
shpm <- readOGR("./maps", "ne_10m_land")

###mettre les donn?es et le fond de carte ensemble - assurer la m?me projection au cas o?
prj <- proj4string(transect)
shpm <- spTransform(shpm, CRS(prj))

#create study area using a bounding box
b <- bbox(transect)
b[, 2] <-
  b[, 2] + 1 #need larger buffer than just min max, with 50 km grid cell
b[, 1] <-
  b[, 1] - 1 #need larger buffer than just min max, with 50 km grid cell

### Build a 50000 x 50000 meters grid - lat et long ? partir du jeu de donn?es? on voir plus tard avec plot(transect) que la grille sert aussi ? restreinfre l'?tendue spatiale
size <- 50000
new.grid <-
  create.grid(
    Latitude = c(b[2, 1], b[2, 2]),
    Longitude = c(b[1, 1], b[1, 2]),
    Grid.size = c(size, size),
    Clip = F,
    clip.shape = shpm,
    projection = CRS(prj)
  )
new.grid$ID <- paste("parc", new.grid$ID, sep = "")

#selectionner les cellules visit?es et couper le shp avec le continent
new.grid <-
  new.grid[apply(gIntersects(transect, new.grid, byid = TRUE), 1, any), ] #limitation de taille de vecteur?

prjm <-
  "+proj=lcc +lat_1=46 +lat_2=60 +lat_0=44 +lon_0=-68.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
test <- spTransform(new.grid, CRS(prjm))
area <- data.frame(km2 = gArea(test, byid = T) / 1000000)
area$ID <- 1:nrow(area)
area$ID <- paste("parc", area$ID, sep = "")
test <- SpatialPolygonsDataFrame(test, data = area)
new.grid <- spTransform(test, CRS(prj))

### Overlay transects and grid and attribute squares to observations
x <- over(transect, new.grid)
d$square <- x$ID
d$square_area <- x$km2
d <- d[!is.na(d$square), ]

sample <-
  names(d)[4] #"Date" #Select variable WatchID, or Date or else
d$SMP_LABEL <- paste(d$square, d[, c(sample)], sep = "_")

### Construct sample labels considering that day transects can overlap with multiple squares
temp <-
  aggregate(WatchLenKm ~ SMP_LABEL, data = unique(d[, c("SMP_LABEL", "WatchID", "WatchLenKm")]), sum)
names(temp)[2] <- "SMP_EFFORT"
d <- merge(d, temp, sort = FALSE)

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
dd <-
  d %>% group_by(SMP_LABEL) %>% summarise(V1 = sum(Count, na.rm = TRUE))
dd <-
  dd[dd$V1 == 0, ] #get the label name for transect without observations
d <-
  d[(d$SMP_LABEL %in% dd$SMP_LABEL &
       !duplicated(d$SMP_LABEL)) |
      (!d$SMP_LABEL %in% dd$SMP_LABEL &
         !(d$Alpha == "")), ] #keep only lines for empty transects or non-empty lines for non-empty
d <- d[order(d$square), ]
path <- "analysis/temp"
pathMCDS <- "tools"
breaks <- c(0, 50, 100, 200, 300)#un peu redondant avec filtre/donn?es
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
keep.me = which.min(sapply(1:length(all.dist), function(i) {
  all.dist[[i]]$AIC[3]
}))#model selected
key <- sapply(strsplit(names(all.dist)[keep.me], "_"), "[", 2)
exp <- sapply(strsplit(names(all.dist)[keep.me], "_"), "[", 3)
estimator = list(c(key, exp))
units = list(
  Type = "Line",
  Distance = "Perp",
  Length_units = "Kilometers",
  Distance_units = "Meters",
  Area_units = "Square kilometers"
)

#analyse- calcul des densit?es telles que demand?es
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

#get dist sampling model density estimates
path1 <-
  paste("x$", "density_estimate$Stratum", sep = "")#model without strat
assign("tmp", eval(parse(text = path1)))

######si aucun sous-groupe = x$density_estimate$Stratum#####

densities <-
  tmp[tmp$Parameters == "D", c("Stratum", "Estimates", "% of var.")]

####save shp - grille + donn?es assoc?es aux cellules
densities$Estimates <- as.numeric(densities$Estimates)
names(densities)[3] <- "CoefVar" #probleme avec % dans nom
densities$CoefVar <- as.numeric(densities$CoefVar)
names(densities)[names(densities) == "Stratum"] <- "ID"

#join les estimations aux shp des cellules
o <- match(new.grid$ID, densities$ID) #IDs for new.grid before??
temp <- densities[o, ]
row.names(temp) <- row.names(new.grid)
new.grid2 <- spCbind(new.grid, temp)
new.grid2 <-
  new.grid2[!is.na(new.grid2$Estimates), ] #enlever les cellules non visit?es dans le subset
test <- new.grid2@data

#select variable to map
#names(new.grid2@data)
map <- names(new.grid2@data)[4] #or else
temp <- densities[densities$Estimates > 0, ]
brks <- quantile(temp[, c(map)], c(0, .5, .75, .95))

#table(new.grid2@data[,c(map)])

brks[1] <-
  ifelse(brks[1] != 0, 0, brks[1])#needed for data below first rounded class
brks[length(brks) + 1] <- max(new.grid2@data[, c(map)]) + 0.1
brks <- round(brks, 1)#3 is no good

##associate data w/brks intervals
tags <- vector()
for (i in 1:length(brks)) {
  if (i == 1) {
    new.grid2@data$class  <-
      ifelse(new.grid2@data[, c(map)] == brks[i], as.character(brks[i]), "-")
    new.grid2@data$classno  <-
      ifelse(new.grid2@data[, c(map)] == brks[i], as.numeric(i), "-")
    tags[1] <- as.character(brks[i])
    
  }
  if (i > 1) {
    new.grid2@data$class  <-
      ifelse(
        new.grid2@data[, c(map)] > brks[i - 1] &
          new.grid2@data[, c(map)] <= brks[i],
        paste(" > ", brks[i - 1], " - ", brks[i], sep = ""),
        new.grid2@data$class
      )
    new.grid2@data$classno  <-
      ifelse(
        new.grid2@data[, c(map)] > brks[i - 1] &
          new.grid2@data[, c(map)] <= brks[i],
        as.numeric(i),
        new.grid2@data$classno
      )
    tags[i] <- paste(" > ", brks[i - 1], " - ", brks[i], sep = "")
  }
}
classes <- unique(new.grid2$class[!is.na(new.grid2$class)])

#colors for legend
br.palette <-
  colorRampPalette(c("green", "yellow", "red"), space = "rgb")
nb <- length(brks) - 1
br.palette(nb)
pal <- br.palette(n = nb)
pal <- c("lightgray", pal)#ajout class 0
new.grid2$color <- pal[as.numeric(new.grid2$classno)]

legendtitle <- "birds/km2"
new.grid2$classno <- as.numeric(new.grid2$classno)
new.grid2 <- new.grid2[order(new.grid2$classno), ]

png(
  "Fig4.png",
  width = 960,
  height = 960,
  antialias = "cleartype",
  units = "px"
)
plot(
  new.grid2,
  bg = "lightblue",
  border = "lightblue",
  axes = T,
  cex.axis = 1.5
)
for (i in 1:length(classes)) {
  plot(new.grid2[new.grid2$class[!is.na(new.grid2$class)] == classes[i], ],
       col = new.grid2@data[new.grid2$class[!is.na(new.grid2$class)] == classes[i], "color"],
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
  transect,
  col = "darkgray",
  pch = 16,
  cex = 1,
  add = T
)
legend(
  "bottomright",
  bty = "n",
  legend = tags,
  fill = pal,
  title = legendtitle,
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
invisible(dev.off())

new.grid2$abun <- new.grid2$km2 * new.grid2$Estimates
