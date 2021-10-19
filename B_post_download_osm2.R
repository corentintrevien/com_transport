library(curl)
library(osmdata)
library(osmar)
library(xml2)
library(reshape)
library(rgdal)
library(rgeos)
library(raster)
library(fasterize)
library(sp)
library(data.table)
library(sf)
library(zip)
library(dplyr)
library(plyr)
library(Rfast)
library(stringr)
library(stringi)
library(maptools)
#http://www.?pnvkarte.de
options(scipen = 15)
source("Z_functions.R")
dir <- "Rail/download"

list_regions <- c("Hauts-de-France","Pays-de-la-Loire","Normandie","Bretagne","Île-de-France","Grand-Est",
                  "Bourgogne-Franche-Comté","Nouvelle-Aquitaine","Occitanie","Provence-Alpes-Côte-d'Azur",
                  "Auvergne-Rhône-Alpes","Corse","Centre-Val-de-Loire")


xmlParseFrance <- function(type){
  osm_france <- lapply(list_regions, function(r){
    print(r)
    file <- paste0(type,"_",plain_str(r),".osm")
    unzip("Rail/download/OSM.zip",file,exdir="Rail/download")
    osm_data <- xmlParse(paste0("Rail/download/",file))
    file.remove(paste0("Rail/download/",file))
    return(osm_data)
  })
  return(osm_france)
}

#Decompression et lecture des données
railway_asc <- xmlParseFrance("railway_asc")
railway_des <- xmlParseFrance("railway_des")
route_des <- xmlParseFrance("route_des")
route_asc <- xmlParseFrance("route_asc")
public <- xmlParseFrance("public")

#Application de la fonction 'osm_parse' (vérifier la fonction exacte)
parsed_railway_asc <- lapply(railway_asc,osm_parse)
parsed_railway_des <- lapply(railway_des,osm_parse)
parsed_route_asc <- lapply(route_asc,osm_parse)
parsed_route_des <- lapply(route_des,osm_parse)
parsed_public <- lapply(public,osm_parse)

#Fusion des données régionales
france_railway_asc <- osm_table_fusion(parsed_railway_asc)
france_railway_des <- osm_table_fusion(parsed_railway_des)
france_route_asc <- osm_table_fusion(parsed_route_asc)
france_route_des <- osm_table_fusion(parsed_route_des)
france_public <- osm_table_fusion(parsed_public)

#Fusions des différentes catégories de données
osm_full <- list(france_railway_asc,france_railway_des,france_route_asc,france_route_des,france_public)
osm_full <- osm_table_fusion(osm_full)

write_oms_table(osm_full,"Rail/rail_osm_full")
osm_full <- read_oms_table("Rail/rail_osm_full")

#Sélection des données situées en France 
osm_france <- select_france(osm_full)


#############
#CORRECTIONS#
#############

#Correction tram d'Avignon
bbox_avignon <- getbb("Avignon")
id_tram_stop <- osm_france$nodes$tags[osm_france$nodes$tags$k =="railway" & osm_france$nodes$tags$v=="tram_stop","id"]
id_tram_stop_avignon <- osm_france$nodes$coords[osm_france$nodes$coords$id %in% id_tram_stop &
                                                  osm_france$nodes$coords$lon <= bbox_avignon[1,2] & 
                                                  osm_france$nodes$coords$lon >= bbox_avignon[1,1] &
                                                  osm_france$nodes$coords$lat <= bbox_avignon[2,2] & 
                                                  osm_france$nodes$coords$lat >= bbox_avignon[2,1],"id"]
tram_stop_avignon <- data.frame(id =  rep(c("10181947","10181946"),each=length(id_tram_stop_avignon)),
                                ref = rep(id_tram_stop_avignon,2), 
                                type = "node",role = "stop")
osm_france$relations$refs <- rbind(osm_france$relations$refs,tram_stop_avignon)

#Correction tram de Besançon
#ref_route_tram <- osm_france$relations$refs[osm_france$relations$refs$id %in% osm_france$relations$tags[osm_france$relations$tags$k == "route" & osm_france$relations$tags$v == "tram","id"],"ref"]
#id_missing_node_tram <- osm_france$node$tags[osm_france$node$tags$k == "tram" & osm_france$node$tags$v == "yes" & !(osm_france$node$tags$id %in% ref_route_tram),"id"]
#id_missing_way_tram <- osm_france$ways$tags[osm_france$ways$tags$k == "tram" & osm_france$ways$tags$v == "yes" & !(osm_france$ways$tags$id %in% ref_route_tram),"id"]

#osm_france$node$tags[osm_france$node$tags$k == "tram"

#map_missing_node_tram <- SpatialPoints(osm_france$node$coords[osm_france$node$coords$id %in% id_missing_node_tram,c("x","y")])
#map_missing_way_tram <- osm_france$ways$refs[osm_france$ways$refs$id %in% id_missing_way_tram,]
#map_missing_way_tram[,c("x","y")] <- osm_france$node$coords[match(map_missing_way_tram$ref,osm_france$node$coords$id),c("x","y")]
#map_missing_way_tram <- split(map_missing_way_tram, map_missing_way_tram$id)
#map_missing_way_tram <- lapply(map_missing_way_tram, function(l)(Lines(Line(cbind(l$x,l$y)),unique(l$id))))
#map_missing_way_tram <- SpatialLines(map_missing_way_tram)

tram_stop_besancon1 <- data.frame(id =  "4109606",ref = c("583912295","583912290","583906769"), 
                                  type = "way",role = "stop")
tram_stop_besancon2 <- data.frame(id =  "4109607",ref = c("583912298","583912291","583906769"), 
                                  type = "way",role = "stop")

osm_france$relations$refs <- rbind(osm_france$relations$refs,tram_stop_besancon1)
osm_france$relations$refs <- rbind(osm_france$relations$refs,tram_stop_besancon2)

#Correction tram de Reims 
osm_france$relations$tags$id <- ifelse(osm_france$relations$tags$id %in% c("10544852","10544853","10544854","10544855","1874970","1874971"),
                                       "FICTRAMREIMS",
                                       osm_france$relations$tags$id)
osm_france$relations$refs$id <- ifelse(osm_france$relations$refs$id %in% c("10544852","10544853","10544854","10544855","1874970","1874971"),
                                       "FICTRAMREIMS",
                                       osm_france$relations$refs$id)
osm_france$relations$tags <- osm_france$relations$tags[!duplicated(osm_france$relations$tags[,c("id","k")]),]
osm_france$relations$refs <- osm_france$relations$refs[!duplicated(osm_france$relations$refs[,c("id","ref")]),]

#Correction tram de Montpellier -> retrait d'une ligne en construction 
osm_france$relations$tags <- osm_france$relations$tags[!(osm_france$relations$tags$id %in% c("4107862","4107863","4107864","4107865")),]
osm_france$relations$refs <- osm_france$relations$refs[!(osm_france$relations$refs$id %in% c("4107862","4107863","4107864","4107865")),]

#Correction tram d'?le-de-France -> retrait d'une ligne en construction 
osm_france$relations$tags <- osm_france$relations$tags[!(osm_france$relations$tags$id %in% c("10041578")),]
osm_france$relations$refs <- osm_france$relations$refs[!(osm_france$relations$refs$id %in% c("10041578")),]

#Retrait des lignes touristiques
id_way_tourist <- osm_france$ways$tags[osm_france$ways$tags$k %in% c("railway:preserved","tourism","abandoned") |
                                         (osm_france$ways$tags$k=="railway" & osm_france$ways$tags$v %in% c("abandoned","preserved")) |
                                         (osm_france$ways$tags$k=="usage" & osm_france$ways$tags$v=="tourism"),"id"]
id_relation_tourist <- osm_france$relation$tags[osm_france$relation$tags$k %in% c("railway:preserved","tourism") |
                                                  (osm_france$relation$tags$k=="usage" & osm_france$relation$tags$v=="tourism"),"id"]
id_relation_tourist <- append(id_relation_tourist,c("6684089","6389927"))

osm_france$ways$tags <- osm_france$ways$tags[!(osm_france$ways$tags$id %in% id_way_tourist),]
osm_france$ways$refs <- osm_france$ways$refs[!(osm_france$ways$refs$id %in% id_way_tourist),]
osm_france$relations$tags <- osm_france$relations$tags[!(osm_france$relations$tags$id %in% id_relation_tourist),]
osm_france$relations$refs <- osm_france$relations$refs[!(osm_france$relations$refs$id %in% id_relation_tourist),]

#Correction tram de Nancy -> Les passages sur route sont codés en voie
id_ways_nancy <- osm_france$relations$refs[osm_france$relations$refs$id %in% c("2074460","2074461") & osm_france$relations$refs$type == "way","ref"]
id_rail_nancy <- osm_france$ways$tags[osm_france$ways$tags$id %in% id_ways_nancy & osm_france$ways$tags$k == "railway","id"]
osm_france$ways$tags[!(osm_france$ways$tags$id %in% id_rail_nancy) & osm_france$ways$tags$id %in% id_ways_nancy & osm_france$ways$tags$k == "highway","v"] <- "pvs"
osm_france$ways$tags[!(osm_france$ways$tags$id %in% id_rail_nancy) & osm_france$ways$tags$id %in% id_ways_nancy & osm_france$ways$tags$k == "highway","k"] <- "railway"

write_oms_table(osm_france,"Rail/rail_osm_france")

###########################
#Infrastructures cyclables#
###########################
# 
# 
# box_france <- read.csv(paste(dir,"/Reseau_routier/box_france.csv",sep=""))
# list_box_france <- split(box_france,row.names(box_france))
# list_box_france <- lapply(list_box_france,unlist)
# list_box_france <- lapply(list_box_france,function(x) gsub("-", "w", paste(x[1],x[2],sep="_")))
# 
# select_cycleway <- function(osmtables){
#   #osmtables <- parsed_reseau_routier[[2]]
#   cycleway_ids <- unique(osmtables$ways$tags[substr(osmtables$ways$tags[,"k"],1,7) %in% c("bicycle","cyclewa") |
#                                               substr(osmtables$ways$tags[,"v"],1,7) %in% c("bicycle","cyclewa") |
#                                               (osmtables$ways$tags[,"k"] == "highway" & osmtables$ways$tags[,"v"]=="living_street") |
#                                               osmtables$ways$tags[,"k"] %in% c("oneway:bicycle","sidewalk:bicycle"),"id"])
#                  
#   cycleway <- subset_osm_table(osmtables,id_nodes = unique(osmtables$ways$refs[osmtables$ways$refs[,"id"] %in% cycleway_ids,"ref"]) ,id_ways=cycleway_ids)
#   return(cycleway)
# }
# #AJOUTER 
# 
# reseau_routier <- lapply(list_box_france, function(x) xmlParse(paste(dir,"/Reseau_routier/reseau_routier_",x,".osm",sep="")))
# parsed_reseau_routier <- lapply(reseau_routier,osm_parse)
# parsed_reseau_cyclable <- lapply(parsed_reseau_routier,select_cycleway)
# 
# #Le carreau 97 fait planter le programme et il est presque totalement en dehors de France 
# reseau_cyclable <- osm_table_fusion(parsed_reseau_cyclable[1:96])
# france_reseau_cyclable <- check_nodes_france(reseau_cyclable)
# france_reseau_cyclable <- select_france(france_reseau_cyclable)
# 
# write_oms_table(france_reseau_cyclable,"W:/Bureau/OSM/Rail/France/dataosm_france_reseau_cyclable")
# 
# ###############################
# #Infrastructures autorouti?res#
# ###############################
# 
# 
# box_france <- read.csv(paste(dir,"/Reseau_routier/box_france.csv",sep=""))
# list_box_france <- split(box_france,row.names(box_france))
# list_box_france <- lapply(list_box_france,unlist)
# list_box_france <- lapply(list_box_france,function(x) gsub("-", "w", paste(x[1],x[2],sep="_")))
# 
# 
# select_major_road <- function(osmtables){
#   
#   #osmtables <- parsed_reseau_routier[[2]]
#   major_road_ids <- unique(osmtables$ways$tags[osmtables$ways$tags[,"k"] == "highway" & 
#                                                osmtables$ways$tags[,"v"] %in% c("trunk","motorway"),"id"])
#   
#   major_road <- subset_osm_table(osmtables,id_nodes = unique(osmtables$ways$refs[osmtables$ways$refs[,"id"] %in% 
#                                                                                  major_road_ids,"ref"]),
#                                            id_ways=major_road_ids)
#   return(major_road)
# }
# 
# reseau_routier <- lapply(list_box_france, function(x) xmlParse(paste(dir,"/Reseau_routier/reseau_routier_",x,".osm",sep="")))
# parsed_reseau_routier <- lapply(reseau_routier,osm_parse)
# parsed_reseau_routier_principal <- lapply(parsed_reseau_routier,select_major_road)
# 
# reseau_routier_principal <- osm_table_fusion(parsed_reseau_routier_principal[1:96])
# france_reseau_routier_principal <- check_nodes_france(reseau_routier_principal)
# france_reseau_routier_principal <- select_france(france_reseau_routier_principal)
# 
# write_oms_table(france_reseau_routier_principal,"W:/Bureau/OSM/Rail/France/dataosm_france_autoroute")
