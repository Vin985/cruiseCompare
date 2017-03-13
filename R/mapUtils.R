##
# Pour clipperla bathymetrie et tout
# Combine Amerique du nord et Europe
test <- spRbind(eu, na)
# Recupereles limites des zones
b1 <- bbox(test)
# convertit en objet spatial
clipe <- as(extent(b1), "SpatialPolygons")
proj4string(clipe) <- CRS(proj4string(eu))
cropd <- SpatialPolygonsDataFrame(clipe, data.frame(x = 1), match.ID =
FALSE)
# intersectionne!
b01 <- gIntersection(b0, cropd, byid = TRUE)
