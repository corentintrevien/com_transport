library(curl)
library(osmdata)
library(osmar)
library(xml2)
library(reshape)
library(rgdal)
library(rgeos)
library(sp)
library(sf)
library(dplyr)
library(plyr)
library(Rfast)
library(stringr)
library(maptools)
library(data.table)
library(fasterize)

source("W:/Bureau/OSM/Z_functions.R")

options(scipen = 15)

osm_autoroute <- read_oms_table("W:/Bureau/OSM/Data/France/dataosm_france_autoroute")

lines_autoroute <- osm_autoroute$ways$refs
lines_autoroute[,c("lon","lat")] <- osm_autoroute$nodes$coords[match(lines_autoroute$ref,
                                        osm_autoroute$nodes$coords$id),c("lon","lat")]

lines_autoroute <- split(lines_autoroute[,c("lon","lat")],lines_autoroute$id)
lines_autoroute <- lapply(lines_autoroute,Line)
lines_autoroute <- lapply(1:length(lines_autoroute),function(x) Lines(lines_autoroute[x],names(lines_autoroute[x])))
lines_autoroute <- SpatialLines(lines_autoroute,proj4string = CRS("+init=epsg:4326"))
data_autoroute <- osm_autoroute$ways$tags
data_autoroute <- data_autoroute[data_autoroute$k %in% c("highway","surface","ref",
                                                         "operator","name","maxspeed","lit",
                                                         "lanes","int_ref"),]
data_autoroute <- dcast(data = data_autoroute, id ~ k, value.var = "v")
row.names(data_autoroute) <- data_autoroute$id

autoroute <- SpatialLinesDataFrame(lines_autoroute,data_autoroute,match.ID = T)

writeOGR(autoroute, "W:/Bureau/OSM/Data/final/autoroute.shp",layer="autoroute",driver="ESRI Shapefile")



