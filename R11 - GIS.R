install.packages("rgdal") # nefunguje
install.packages("sf")
library(sf)
nms = st_layers(dsn = "C:/Users/nikol/Plocha/Programming_in_R/gis")

for(i in nms){
  assign(i, value = st_read(dsn = "C:/Users/nikol/Plocha/Programming_in_R/gis", layer = i))
}

area = st_read(dsn = "C:/Users/nikol/Plocha/Programming_in_R/gis", layer = "area")
islands = st_read(dsn = "C:/Users/nikol/Plocha/Programming_in_R/gis", layer = "islands")
lakes = st_read(dsn = "C:/Users/nikol/Plocha/Programming_in_R/gis", layer = "lakes")
nests = st_read(dsn = "C:/Users/nikol/Plocha/Programming_in_R/gis", layer = "nests")
roads = st_read(dsn = "C:/Users/nikol/Plocha/Programming_in_R/gis", layer = "roads")

attributes(area)
str(area)
area$geometry
str(area$geometry[[1]][[1]])
dev.off()
{
plot(area$geometry[[1]][[1]][,1:2], type = "l", ann = F, axes = F, col = "lightgoldenrod3")
polygon(area$geometry[[1]][[1]][,1:2], col = "lightgoldenrod3", border = "lightgoldenrod3")
polygon(lakes$geometry[[1]][[1]][,1:2], type = "l", ann = F, axes = F, col = "cadetblue2", border = "cadetblue2")

for(i in 1:nrow(lakes)){
  polygon(lakes$geometry[[i]][[1]][,1:2], col = "cadetblue2", border = "cadetblue2")
}

islands = islands[-75,] # odecetli jsme 75. radek
for(i in 1:nrow(islands)){
  polygon(islands$geometry[[i]][[1]][,1:2], col = "lightgoldenrod3", border = "lightgoldenrod3")
}
coors = data.frame(lon = unlist(nests$geometry)[(1:length(unlist(nests$geometry)))%%2==1],
                   lat = unlist(nests$geometry)[(1:length(unlist(nests$geometry)))%%2==0])
points(coors$lon, coors$lat, pch = 16, cex = 0.7, col = "firebrick4")

str(roads)
for(i in 1:nrow(roads)){
  lines(roads[i,]$geometry[[1]][,1:2], col = "tan4", lwd = 1.5)
}
ar = as.data.frame(area)
warnings()
str(roads[1,]$geometry[[1]])
}

nests$POINT_X[!is.na(nests$POINT_X)]=
gsub(pattern = ",", replacement = ".", x = nests$POINT_X[!is.na(nests$POINT_X)])
nests$POINT_X = as.numeric(nests$POINT_X)

nests$POINT_Y[!is.na(nests$POINT_Y)]=
gsub(pattern = ",", replacement = ".", x = nests$POINT_Y[!is.na(nests$POINT_Y)])
nests$POINT_Y = as.numeric(nests$POINT_Y)

#________________________________________________________
# JEDNODUSSI ZPUSOB MNOHEM
install.packages("sp")
library(sp)
plot(st_geometry(area), col = "lightgoldenrod3")
plot(st_geometry(lakes), col = "cadetblue2",  add = T)
plot(st_geometry(islands), col = "lightgoldenrod3", add = T)
plot(st_geometry(roads), col = "tan4", add = T)
plot(st_geometry(nests), col = "firebrick", add = T, pch = 16, cex = 0.7)

install.packages("osmdata")
library(osmdata)
getbb("Prague")
available_features()
available_tags("amenity")
o = opq(getbb("Al Marmoom"))
feature = add_osm_feature(opq = o, key = "amenity", value = "parking")
ff = osmdata_sf(feature)
plot(st_geometry(ff$osm_polygons), add = T, col = "olivedrab1")
parking = as_Spatial(ff$osm_polygons)
st_write(obj = st_geometry(ff$osm_polygons), dsn = "./gis", layer = "parking", driver = "ESRI Shapefile" )

install.packages("geosphere")
library(geosphere)
centroid = colMeans(ff$osm_polygons$geometry[[1]][[1]])
distm(centroid, coors[147,])
distm(coors[148,], coors[147,])
?points
points(coors[147,],col = "midnightblue", pch = 21, cex = 2)

# ORTOFOTO
url0 = "https://ags.cuzk.cz/arcgis1/services/ORTOFOTO/MapServer/WMSServer"
gdal_utils(util = "info", paste0("WMS:", url0), quiet = F)
load("coors.RData")
sour = st_as_sf(x = df, coords = c("lon", "lat"), crs = 4326)
bbox1 = paste(st_bbox(sour)+c(-.4, -.16, .4, .16), collapse = ",")
url1 = "https://ags.cuzk.cz/arcgis1/services/ORTOFOTO/MapServer/WMSServer?SERVICE=WMS&VERSION=1.1.1&REQUEST=GetMap&LAYERS=0&SRS=EPSG:4326&BBOX="
url = paste0(url1,bbox1)
t = tempfile()
reso = c("-outsize", "3840", "2160", "-co", "COMPRESS=JPEG")
gdal_utils(util = "translate", source = url, destination = t, options = reso)
install.packages("terra")
library(terra)
r = rast(t)
x11()
par(xpd = T, mar = c(0,0,0,0))
plot(r)
points(df, col = "red", pch = 20)
install.packages("prettymapr")
library(prettymapr)

addnortharrow(pos = "bottomright", cols = c("black", "white"), text.col = "white", padin = c(1, .2))
addscalebar(plotunit = "km", label.col = "white", padin = c(1, .2))
dev.off()
install.packages("maps")
library(maps)
cz = map(database = "world", regions = "Czech Republic", fill = F, plot = F)
par(fig = c(.01, .3, .7, .99), new = T)
plot(cz, axes = F, ann = F, type = "l")
bb2 = st_bbox(sour)+c(-.4,-.16,.4,.16)
lines(x = c(bb2[1], bb2[3], bb2[3], bb2[1], bb2[1]),
      y = c(bb2[2], bb2[2], bb2[4], bb2[4], bb2[2]), 
      col = "red")
