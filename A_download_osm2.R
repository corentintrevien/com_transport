library(curl)
library(osmdata)
library(osmar)
library(xml2)
library(reshape)
library(rgdal)
library(rgeos)
library(sp)
library(sf)
library(stringi)
library(stringr)
library(raster)
library(fasterize)
library(RPostgreSQL)

#install.packages("RPostgreSQL")
set_overpass_url("https://lz4.overpass-api.de/api/interpreter")
#set_overpass_url("https://z.overpass-api.de/api/interpreter")
#set_overpass_url("https://overpass.kumi.systems/api/interpreter")
get_overpass_url()
dir <- "W:/Bureau/OSM/Data/download"
source("W:/Bureau/OSM/functions.R")

list_regions <- c("Bretagne","Hauts-de-France","Pays-de-la-Loire","Normandie","Île-de-France","Grand-Est",
                  "Bourgogne-Franche-Comté","Nouvelle-Aquitaine","Occitanie","Provence-Alpes-Côte-d'Azur",
                  "Auvergne-Rhône-Alpes","Corse","Centre-Val-de-Loire")


bbox <- lapply(list_regions, function(x) opq (x, timeout = 900,memsize = 1073741824)$bbox)
bbox <- lapply(bbox,function(x) matrix(as.numeric(strsplit(x,",")[[1]])[c(2,2,4,4,2,1,3,3,1,1)],nrow=5))
bbox <- lapply(bbox,Line)
bbox <- SpatialLines(list(Lines(bbox,"ID")))

plain_str <- function(str){
  plain <- str_replace_all(stri_trans_general(tolower(str),"Latin-ASCII"), "[:punct:]", "_") 
  return(plain)
}

#Decoupage de la france en carreaux

region <- readOGR("W:/Bureau/OSM/Data/ADE/REGION.shp")
region <- region[!(region$NOM_REG %in% c("La RÃ©union","Martinique","Guadeloupe","Mayotte","Guyane")),]
region <- spTransform(region, CRS("+init=epsg:3035"))
region <- gSimplify(region, tol=100)
france <- gBuffer(region, byid=FALSE, width=100)
france <- spTransform(france, CRS("+init=epsg:4326"))
bbox_france <- bbox(france)
bbox_france <- floor(bbox_france)

#Division en carreaux du territoire Français 
box_x <- bbox_france["x","min"]:bbox_france["x","max"]
box_y <- bbox_france["y","min"]:bbox_france["y","max"]
box <- data.frame(x = rep(box_x,each=length(box_y)), y = rep(box_y,times=length(box_x))) 
box <- lapply(1:nrow(box),function(x) box[rep(x,5),] + c(0,1,1,0,0,0,0,1,1,0))
box <- lapply(box,function(x) Polygons(list(Polygon(x,hole=FALSE)),paste(x[1,1],x[1,2]))) 
box = SpatialPolygons(box,proj4string = CRS("+init=epsg:4326") )

#plot(france)

intersection_box <- gIntersection(france,box,byid = TRUE)
intersection_box <- substr(names(gArea(spTransform(intersection_box, CRS("+init=epsg:3035")),byid = TRUE)),8,12)

box_france <- lapply(intersection_box,function(x) unlist(as.list(bbox(box[x]))))
box_france <- data.frame(do.call(rbind,box_france))
colnames(box_france) <- c("xmin","ymin","xmax","ymax")
write.csv(box_france,paste(dir,"/Reseau_routier/box_france.csv",sep=""),row.names = FALSE)

box_france <- read.csv(paste(dir,"/Reseau_routier/box_france.csv",sep=""))
list_box_france <- split(box_france,row.names(box_france))
list_box_france <- lapply(list_box_france,unlist)

#Téléchangement des données de réseau routier
download_osm_reseau_routier <- function(box_coord){
  #box_coord <- c(-1,45,0,46)
  query <- opq(bbox = box_coord, timeout = 900,memsize = 1073741824)
  query <- add_osm_feature (query, key="highway")
  query <- osmdata_xml (query)
  write_xml(query,paste(dir,"/Reseau_routier/reseau_routier_",gsub("-", "w", paste(box_coord[1],box_coord[2],sep="_")),".osm",sep=""))
} 

for(box_france in list_box_france){download_osm_reseau_routier(box_france)}

#Bus
download_osm_bus_region <- function(region){
  #region <- "Bourgogne-Franche-Comté"
  plain_region <- plain_str(region)
  
  query <- opq(region, timeout = 900,memsize = 1073741824)
  query <- add_osm_feature (query, key="route", value = c("bus"))
  query <- osmdata_xml (query)
  write_xml(query,paste(dir,"/bus_des_",plain_region,".osm",sep=""))
  
  query <- opq (region, timeout = 900,memsize = 1073741824)
  query <- add_osm_feature (query, key="route", value = c("bus"))
  query$suffix <- ");\n(._;<<;);\nout body;"
  query <- osmdata_xml (query)
  write_xml(query,paste(dir,"/bus_asc_",plain_region,".osm",sep=""))
  
} 

list_regions2 <- c("Corse","Centre-Val-de-Loire")

for(region in list_regions2){download_osm_bus_region(region)}

download_osm_railway_region <- function(region){
  #region <- "Bourgogne-Franche-Comté"
  plain_region <- plain_str(region)
  
  query <- opq(region, timeout = 900,memsize = 1073741824)
  query <- add_osm_feature (query, key="railway", value = c('tram','subway','rail','light_rail',"abandoned",
                                                            "construction","disused","preserved","halt",
                                                            "stop_position","platform","station","tram_stop"))
  query <- osmdata_xml (query)
  write_xml(query,paste(dir,"/railway_des_",plain_region,".osm",sep=""))
  
  query <- opq (region, timeout = 900,memsize = 1073741824)
  query <- add_osm_feature (query, key="railway", value = c('tram','subway','rail','light_rail',"abandoned",
                                                            "construction","disused","preserved","halt",
                                                            "stop_position","platform","station","tram_stop"))
  query$suffix <- ");\n(._;<<;);\nout body;"
  query <- osmdata_xml (query)
  write_xml(query,paste(dir,"/railway_asc_",plain_region,".osm",sep=""))
  
} 

list_regions2 <- c("Provence-Alpes-Côte-d'Azur",
                  "Auvergne-Rhône-Alpes","Corse","Centre-Val-de-Loire")

for(region in list_regions2){download_osm_railway_region(region)}


download_osm_region <- function(region){
  #region <- "Bourgogne-Franche-Comté"
  plain_region <- plain_str(region)
  
  query <- opq (region, timeout = 900,memsize = 1073741824)
  query <- add_osm_feature (query, key="railway", value = c('tram','subway','rail','light_rail',))
  query <- osmdata_xml (query)
  write_xml(query,paste(dir,"/railway_des_",plain_region,".osm",sep=""))
  
  query <- opq (region, timeout = 900,memsize = 1073741824)
  query <- add_osm_feature (query, key="railway", value = c('tram','subway','rail','light_rail'))
  query$suffix <- ");\n(._;<<;);\nout body;"
  query <- osmdata_xml (query)
  write_xml(query,paste(dir,"/railway_asc_",plain_region,".osm",sep=""))
  
  query <- opq (region, timeout = 900,memsize = 1073741824)
  query <- add_osm_feature (query, key="public_transport", value = c('stop_area','station','group_stop_area'))
  query <- osmdata_xml (query)
  write_xml(query,paste(dir,"/public_",plain_region,".osm",sep=""))
} 

for(region in list_regions2){download_osm_region(region)}




download_osm_route_region <- function(region){
  #region <- "Bourgogne-Franche-Comté"
  plain_region <- plain_str(region)
  
  query <- opq (region, timeout = 900,memsize = 1073741824)
  query <- add_osm_feature (query, key="route", value = c('tram','subway','rail','light_rail',"train"))
  query <- osmdata_xml (query)
  write_xml(query,paste(dir,"/route_des_",plain_region,".osm",sep=""))
  
  query <- opq (region, timeout = 900,memsize = 1073741824)
  query <- add_osm_feature (query, key="route", value = c('tram','subway','rail','light_rail',"train"))
  query$suffix <- ");\n(._;<<;);\nout body;"
  query <- osmdata_xml (query)
  write_xml(query,paste(dir,"/route_asc_",plain_region,".osm",sep=""))
  
  
} 

for(region in list_regions){download_osm_route_region(region)}


download_route_region <- function(region){
  #region <- "Bretagne"
  plain_region <- plain_str(region)
  
  query <- opq (region, timeout = 900,memsize = 1073741824)
  query <- add_osm_feature (query, key="route", value = c('tram','subway','railway','light_rail','train'))
  query <- osmdata_xml (query)
  write_xml(query,paste("/Users/corentintrevien/Desktop/OSM/Data/route_des_",plain_region,".osm",sep=""))
  
  query <- opq (region, timeout = 900,memsize = 1073741824)
  query <- add_osm_feature (query, key="route", value = c('tram','subway','railway','light_rail','train'))
  query$suffix <- ");\n(._;<<;);\nout body;"
  query <- osmdata_xml (query)
  write_xml(query,paste("/Users/corentintrevien/Desktop/OSM/Data/route_asc_",plain_region,".osm",sep=""))
} 

for(region in list_regions){download_route_region(region)}
download_route_region("Centre-Val-de-Loire")
