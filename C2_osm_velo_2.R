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

osm_velo <- read_oms_table("W:/Bureau/OSM/Data/France/dataosm_france_reseau_cyclable")

tags_velo <- osm_velo$ways$tags
tags_velo$pos2pt <- regexpr(':', tags_velo$k) 
tags_velo$radk <- ifelse(tags_velo$pos2pt>1,substr(tags_velo$k,1,tags_velo$pos2pt-1),tags_velo$k)
tags_velo <- tags_velo[tags_velo$radk %in% c("highway","bicycle","footway","busway","cycleway","sidewalk","service","psv","oneway","lane","lanes") |
                       tags_velo$k == "name",]

data_velo <- dcast(tags_velo[,c("id","k","v")], id ~ k, value.var = "v",fill = "")

#Suppression de variables 

remove_bicycle <- c('dismount','dismonted','dismounted',"dismout",'discouraged','no','use_sidepath','limited','Pied Ã  terre obligatoir par moments')

data_velo <- data_velo[!( data_velo[,"bicycle:backward"] %in% remove_bicycle | data_velo[,"bicycle:forward"] %in% remove_bicycle | data_velo[,"bicycle"] %in% remove_bicycle),]

#Suppression de variables 

data_velo$piste_cyclable <- ifelse(data_velo[,"cycleway"] %in% c("track","opposite_track","separate","left","right","segregated","yes",
                                                       "both","yes","oneway","track:right","track:left")  | 
                             data_velo[,"cycleway:both"] %in% c("track","opposite_track","separate")  | 
                             data_velo[,"cycleway:right"] %in% c("track","opposite_track","separate")  | 
                             data_velo[,"cycleway:opposite"] %in% c("track","opposite_track","separate") | 
                             data_velo[,"cycleway:left"] %in% c("track","opposite_track","separate")  | 
                             data_velo[,"cycleway:track"] == "track" | 
                             data_velo[,"highway"] == "cycleway",1,0)

data_velo$bande_cyclable <- ifelse(substr(data_velo[,"bicycle:lanes"],1,3) == "yes" | 
                              substr(data_velo[,"bicycle:lanes:backward"],1,3) == "yes" | 
                              substr(data_velo[,"bicycle:lanes:forward"],1,3) == "yes" |  
                              data_velo[,"cycleway"] %in% c("lane","opposite_lane","buffered_lane","chaussidoux") | 
                              data_velo[,"cycleway:both"] %in% c("lane","opposite_lane","yes") | 
                              data_velo[,"cycleway:right"] %in% c("lane","opposite_lane","yes") | 
                              data_velo[,"cycleway:left"] %in% c("lane","opposite_lane","yes"),1,0)

data_velo$contre_sens_cyclable <- ifelse(data_velo[,"oneway:bicycle"] == "no" |
                                     data_velo[,"cycleway"] == "opposite" | 
                                     data_velo[,"cycleway:both"] == "opposite" | 
                                     data_velo[,"cycleway:right"] == "opposite" | 
                                     data_velo[,"cycleway:left"] == "opposite" | 
                                     data_velo[,"cycleway:opposite"] == "yes" ,1,0)

data_velo$voie_bus_cyclable <- ifelse(data_velo[,"cycleway:right"] %in% c("share_busway","opposite_shared_busway","opposite_share_busway") |
                                   data_velo[,"cycleway:left"] %in% c("share_busway","opposite_shared_busway","opposite_share_busway") |
                                   data_velo[,"cycleway:both"] %in% c("share_busway","opposite_shared_busway","opposite_share_busway") |
                                   data_velo[,"cycleway"] %in% c("share_busway","opposite_shared_busway","opposite_share_busway") |
                                   (data_velo[,"psv"] == "yes" & data_velo[,"bicycle"] == "yes"),1,0)

data_velo$sentier_cyclable <- ifelse(data_velo[,"highway"] %in% c("path","bridleway","track") & data_velo[,"bicycle"] %in% c("yes","designated","permissive"),1,0)

data_velo$mode_doux <- ifelse(data_velo[,"sidewalk:bicycle"] == "yes" |
                         data_velo[,"sidewalk:right:bicycle"] %in% c("yes","share_cycleway") |
                         data_velo[,"sidewalk:left:bicycle"] %in% c("yes","share_cycleway") |
                         data_velo[,"sidewalk:both:bicycle"] %in% c("yes","share_cycleway") |
                         data_velo[,"highway"] == "living_street" |
                          data_velo[,"cycleway"] %in% c("sidewalk","use_sidepath") |
                         (data_velo[,"highway"] %in% c("pedestrian","footway") & 
                              (data_velo[,"bicycle"] == "yes" | data_velo[,"cycleway"]=="yes" )) ,1,0)

data_velo <- data_velo[data_velo$piste_cyclable == 1 | data_velo$bande_cyclable == 1 | data_velo$contre_sens_cyclable == 1 | 
                 data_velo$sentier_cyclable == 1 | data_velo$voie_bus_cyclable == 1 | data_velo$mode_doux == 1,]


data_velo$infra_velo <- ifelse(data_velo$mode_doux == 1,"Aménagement mode doux","")
data_velo$infra_velo <- ifelse(data_velo$sentier_cyclable == 1,"Sentier cyclable",data_velo$infra_velo)
data_velo$infra_velo <- ifelse(data_velo$contre_sens_cyclable == 1,"Contre-sens cyclable",data_velo$infra_velo)
data_velo$infra_velo <- ifelse(data_velo$voie_bus_cyclable == 1,"Voie de bus cyclable",data_velo$infra_velo)
data_velo$infra_velo <- ifelse(data_velo$bande_cyclable == 1,"Bande cyclable",data_velo$infra_velo)
data_velo$infra_velo <- ifelse(data_velo$piste_cyclable == 1,"Piste cyclable",data_velo$infra_velo)
rownames(data_velo) <- data_velo$id

coord_infra_velo <- osm_velo$ways$refs[osm_velo$ways$refs$id %in% data_velo$id,]
coord_infra_velo[,c("lat","lon")] <- osm_velo$nodes$coords[match(coord_infra_velo$ref,osm_velo$nodes$coords$id),c("lat","lon")]
coord_infra_velo <- split(coord_infra_velo, coord_infra_velo$id)
coord_infra_velo <- lapply(coord_infra_velo, function(l)(Lines(Line(cbind(l$lon,l$lat)),unique(l$id))))
coord_infra_velo <- SpatialLines(coord_infra_velo,proj4string = CRS("+init=EPSG:4326"))
coord_infra_velo <- SpatialLinesDataFrame(coord_infra_velo, data_velo[,c("infra_velo","name")], match.ID = TRUE)

pdf("W:/Bureau/OSM/Map/essai_bike.pdf")

plot(coord_infra_velo[coord_infra_velo$infra_velo=="Aménagement mode doux",],col="pink",lwd = 0.02)
plot(coord_infra_velo[coord_infra_velo$infra_velo %in% c("Piste cyclable","Bande cyclable"),],col="orange",add=TRUE,lwd = 0.02)
plot(coord_infra_velo[coord_infra_velo$infra_velo %in% c("Voie de bus cyclable"),],col="blue",add=TRUE,lwd = 0.02)
plot(coord_infra_velo[coord_infra_velo$infra_velo %in% c("Contre-sens cyclable"),],col="red",add=TRUE,lwd = 0.02)
plot(coord_infra_velo[coord_infra_velo$infra_velo=="Sentier cyclable",],col="green",add=TRUE,lwd = 0.02)

dev.off()

coord_infra_velo <- spTransform(coord_infra_velo,CRS("+init=epsg:3035"))

writeOGR(coord_infra_velo, "W:/Bureau/OSM/Data/final/velo.shp",layer="velo",driver="ESRI Shapefile")




#Raster des données de vélo
raster_france <- raster(extent(3209600,4284600,2029200,3135400), res=200)
crs(raster_france) <- "+init=epsg:3035" 
#raster_velo <- rasterize(coord_infra_velo,raster_france,fun = "length")

fasterize_lines2 <- function(lines){
  #lines <- coord_infra_velo[9,]
  bbox <- floor(bbox(lines)/200)
  box <- cbind(x = rep((bbox["x","min"]:bbox["x","max"])*200,each=bbox["y","max"] - bbox["y","min"] + 1), 
               y = rep((bbox["y","min"]:bbox["y","max"])*200,times=bbox["x","max"] - bbox["x","min"] + 1)) 
  box <- lapply(1:nrow(box),function(x) box[rep(x,5),] + c(0,200,200,0,0,0,0,200,200,0))
  box <- SpatialPolygons(lapply(box,function(x) Polygons(list(Polygon(x,hole=FALSE)),paste(x[1,2],"E",x[1,1],sep=""))),proj4string = CRS("+init=epsg:3035")) 
  carreaux <- gLength(gIntersection(box,lines,byid=TRUE),byid=TRUE)
  carreaux <- cbind(IdINSPIRE = substr(names(carreaux),1,15), id = substr(names(carreaux),17,30), length = carreaux)
  #plot(box)
  #plot(lines,add=TRUE)
  
  return(carreaux)
}

#fasterize_lines2(coord_infra_velo["2574",])


deb <- Sys.time()

#carreaux_velo <- lapply(1:1000,function(x) fasterize_lines2(coord_infra_velo[x,]))
carreaux_velo <- lapply(1:length(coord_infra_velo),function(x) fasterize_lines2(coord_infra_velo[x,]))
carreaux_velo <- do.call(rbind,carreaux_velo)
row.names(carreaux_velo) <- NULL
carreaux_velo <- data.frame(carreaux_velo,stringsAsFactors = FALSE)
carreaux_velo$IdINSPIRE <- paste("CRS3035RES200mN",carreaux_velo$IdINSPIRE,sep="")
carreaux_velo$infra_velo <- coord_infra_velo@data[match(carreaux_velo$id,row.names(coord_infra_velo)),"infra_velo"]
carreaux_velo$length <- as.integer(carreaux_velo$length)
carreaux_velo <- cast(carreaux_velo,IdINSPIRE~infra_velo,value='length',fun.aggregate=sum)
write.csv(carreaux_velo,"W:/Bureau/OSM/Data/final/carreaux_velo.csv",row.names = FALSE)  

fin <- Sys.time()
fin-deb 

