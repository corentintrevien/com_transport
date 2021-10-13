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
source("W:/Bureau/OSM/functions.R")

options(scipen = 15)

read_oms_table <- function(path){
  osm_tab = list()
  types_members <- list(c("nodes","coords"),c("nodes","tags"),c("ways","tags"),c("ways","refs"),
                        c("relations","tags"),c("relations","refs"))
  for(tm in types_members){
    osm_tab[[tm[1]]][[tm[2]]] <- read.csv(paste(path,'_',tm[1],'_',tm[2],'.csv',sep=""),stringsAsFactors = FALSE)
  }
  return(osm_tab)
}

osm_france <- read_oms_table("W:/Bureau/OSM/Data/France/dataosm_france")


#Synthèse des moyens de transport
mdt <- function(table,list){
  replace_mdt <- function(x){
    table[,c(x)] <- addNA(table[,c(x)])
    levels(table[,c(x)]) = c(x,"")
    return(table[,c(x)])
  }
  table[,list] <- lapply(list,replace_mdt)
  mdt <- do.call(paste, c(table[list], sep=" "))
  mdt <- trimws(mdt)
  mdt <- gsub(' +',' ',mdt) 
  return(mdt)
}

subset_osm_table <- function(osm_table,id_nodes=c(),id_ways=c(),id_relations=c()){
  osm_table$nodes$coords <- osm_table$nodes$coords[osm_table$nodes$coords[,"id"] %in% id_nodes,]
  osm_table$nodes$tags <- osm_table$nodes$tags[osm_table$nodes$tags[,"id"] %in% id_nodes,]
  osm_table$ways$refs <- osm_table$ways$refs[osm_table$ways$refs[,"id"] %in% id_ways,]
  osm_table$ways$tags <- osm_table$ways$tags[osm_table$ways$tags[,"id"] %in% id_ways,]
  osm_table$relations$refs <- osm_table$relations$refs[osm_table$relations$refs[,"id"] %in% id_relations,]
  osm_table$relations$tags <- osm_table$relations$tags[osm_table$relations$tags[,"id"] %in% id_relations,]
  return(osm_table)
}

#Listes utiles dans tous le programme
list_route_type <- c("train","tram","subway","light_rail")
list_track_type <- c("rail","tram","subway","light_rail","construction","narrow_gauge","disused","psv")

################
#Donnees tracks#
################

#Identifiants tracks
id_track <- osm_france$ways$tags[osm_france$ways$tags$k=="railway" &  osm_france$ways$tags$v %in% list_track_type,"id"] 
track <- subset_osm_table(osm_france,id_ways = id_track)

#Données tracks
tags_track <- track$ways$tags[track$ways$tags$k %in% c("usage","train","service","railway","importance","name","id"),]
tags_track <- as.data.frame(cast(tags_track, id ~ k,value = "v"))
tags_track <- rename(tags_track,replace = c(id="id_track"))
row.names(tags_track) <- tags_track$id_track

#Nom des lignes de chemin de fer
id_route_rw <- osm_france$relations$tags[osm_france$relations$tags$k=="route" & osm_france$relations$tags$v == "railway","id"] 
route_rw <- subset_osm_table(osm_france,id_relations = id_route_rw)

tags_route_rw <- as.data.frame(cast(route_rw$relations$tags, id ~ k,value = "v"))
refs_route_rw <- route_rw$relations$refs[route_rw$relations$refs$type=="way",c('id','ref')]
refs_route_rw$name_line <- tags_route_rw[match(refs_route_rw$id,tags_route_rw$id),c("name")]
refs_route_rw <- rename(refs_route_rw,replace = c(id='id_line',ref='id_track')) 
tags_track[,c("name_line",'id_line')] <- refs_route_rw[match(tags_track$id_track,refs_route_rw$id_track),c("name_line",'id_line')]

#Coordonnées des tracks
points_track <- track$ways$refs
points_track <- rename(points_track,replace = c(id="id_track"))
points_track[,c('x','y')] <- osm_france$nodes$coords[match(points_track$ref, osm_france$nodes$coords$id),c('x','y')]

#Conversion en shapefile 
lines <- split(points_track, points_track$id_track)
lines <- lapply(lines, function(l)(Lines(Line(cbind(l$x,l$y)),unique(l$id_track))))
lines <- SpatialLines(lines,proj4string = CRS("+init=EPSG:3035"))
map_track <- SpatialLinesDataFrame(lines, tags_track, match.ID = TRUE)
#Bounding box de chaque track 
bbox_lines <- function(line){
  #line <-map_track@lines[[1]]
  bbox <- bbox(line)
  bbox <- data.frame(xmin =  bbox[1,1], xmax = bbox[1,2], ymin = bbox[2,1], ymax = bbox[2,2])
  bbox$id <- line@ID
  return(bbox)
}

bbox_track <- lapply(map_track@lines,bbox_lines)
bbox_track <- do.call(rbind,bbox_track)
map_track@data[,c("xmin","xmax","ymin","ymax")] <- bbox_track[match(map_track$id_track,bbox_track$id),c("xmin","xmax","ymin","ymax")]

###############
#Donnees route#
###############
#Identifiants route
id_route <- osm_france$relation$tags[osm_france$relation$tags$k=="route" & osm_france$relation$tags$v %in% list_route_type,"id"]
route <- subset_osm_table(osm_france, id_relations = id_route)

#Données routes
tags_route <- route$relations$tags[route$relations$tags$k %in% c("from","to","route","name","operator","network","id"),]
tags_route <- as.data.frame(cast(tags_route, id ~ k,value = "v"))
tags_route <- rename(tags_route,replace = c(id="id_route"))
data_route <- tags_route

#Références routes
refs_route <- route$relations$refs
refs_route <- rename(refs_route,replace = c(id="id_route"))
refs_route_nodeway <- subset_osm_table(osm_france,id_nodes = refs_route$ref,id_ways = refs_route$ref)
tags_refs_route_node <- refs_route_nodeway$nodes$tags
tags_refs_route_node <- tags_refs_route_node[tags_refs_route_node$k %in% c("railway","public_transport","highway","building","name"),]
tags_refs_route_node <- data.frame(cast(tags_refs_route_node, id ~ k,value = "v"))
tags_refs_route_node$type = "node"
tags_refs_route_way <- refs_route_nodeway$ways$tags
tags_refs_route_way <- tags_refs_route_way[tags_refs_route_way$k %in% c("railway","public_transport","highway","building","name"),]
tags_refs_route_way <- data.frame(cast(tags_refs_route_way, id ~ k,value = "v"))
tags_refs_route_way$type = "way"
refs_route_node <- refs_route[refs_route$type == "node",]
refs_route_way <- refs_route[refs_route$type == "way",]
refs_route_node[,c("railway","public_transport","highway","building","name")] <- tags_refs_route_node[match(refs_route_node$ref,tags_refs_route_node$id),
                                                                                                      c("railway","public_transport","highway","building","name")]
refs_route_way[,c("railway","public_transport","highway","building","name")] <- tags_refs_route_way[match(refs_route_way$ref,tags_refs_route_way$id),
                                                                                                    c("railway","public_transport","highway","building","name")]
refs_route <- rbind(refs_route_node,refs_route_way)

#Sélection des routes renvoyant à des tracks (et retrait des bus)
refs_route$map_track <- (refs_route$ref %in% unique(map_track$id_track))
stat_route <- data.frame(cbind(table(refs_route$id_route,refs_route$highway),
                               table(refs_route$id_route,refs_route$railway),
                               map_track = table(refs_route$id_route,refs_route$map_track)[,2]))
stat_route <- stat_route[stat_route$map_track > 0,]
stat_route$route <- stat_route$primary + stat_route$residential + stat_route$secondary + stat_route$tertiary
#stat_route$chemin_fer <- stat_route$rail +stat_route$stop  + stat_route$subway  + stat_route$tram  + stat_route$tram
stat_route$chemin_fer <- rowSums(stat_route[,list_track_type])
stat_route <-stat_route[stat_route$chemin_fer > stat_route$route/4,] 
refs_route <- refs_route[refs_route$id_route %in% row.names(stat_route),]
data_route <- data_route[data_route$id_route %in% row.names(stat_route),]
#data_route[,list_track_type] <- stat_route[match(data_route$id_route,row.names(stat_route)),list_route_type]
#data_route[,list_track_type] <- stat_route[match(data_route$id_route,row.names(stat_route)),list_track_type]

#Carte des tracks 
track_route <- refs_route[refs_route$type=="way"& refs_route$ref %in% map_track$id_track,c("id_route","ref")]
track_route <- rename(track_route,replace=c(ref="id_track"))

#Suppression des tracks sans route (notamment les voies de service et de fret)
map_track <- map_track[map_track$id_track %in% unique(track_route$id_track),]
track <- subset_osm_table(track,id_ways = track_route$id_track)

################
#Données Lignes#
################
#Master route
#id_route_master <- find(route_master_network,relation(tags(k=="route_master")))
#route_master <- subset(route_master_network,relation_ids = id_route_master)
#Selection des masters routes 
#refs_route_master <- route_master$relations$refs[route_master$relations$refs$ref %in% unique(data_route$id_route),]  
#Données des masters routes 
#tags_route_master <-  route_master$relations$tags[route_master$relations$tags$id %in% unique(refs_route_master$id),] 
#tags_route_master <- as.data.frame(cast(tags_route_master, id ~ k))

########
#Arrêts#
########

#Récupération de tout ce qui ressemble de près ou de loin à un arrêt 
refs_route_arret <- refs_route[refs_route$railway %in% c("halt","station","stop","tram_stop","buffer_stop") |
                                 refs_route$role %in% c("stop","stop_exit_only","stop_entry_only","platform","platform_entry_only","platform_exit_only") |
                                 refs_route$public_transport %in% c("stop_position","station","platform") |
                                 refs_route$building %in% c("train_station"),
                               c("id_route","ref","railway","public_transport","highway","building","role","type","name")]

#Arrêt de type node
refs_route_arret_node <- refs_route_arret[refs_route_arret$type=='node',]
refs_route_arret_node$id_node <- refs_route_arret_node$ref
#Récupération des points des ways
refs_route_arret_way <- osm_france$ways$refs[osm_france$ways$refs$id %in% unique(refs_route_arret$ref),]
refs_route_arret_way <- rename(refs_route_arret_way,replace = c(id = "ref",ref = "id_node"))
refs_route_arret_way <- merge(refs_route_arret_way,refs_route_arret[refs_route_arret$type=='way',], by = "ref")

#Pour les tram et les métros, récupération de tous les arrêts trouvés sur voie (on part du principe que tout arrêt traversé est desservi pour ce type de transport)
refs_track_arret <- osm_france$ways$refs[osm_france$ways$refs$id %in% unique(map_track[map_track$railway %in% c("subway","tram"),]$id_track),]
refs_track_arret <- rename(refs_track_arret,replace= c(id = "id_track"))
tags_track_arret <- osm_france$nodes$tags[osm_france$nodes$tags$id %in% unique(refs_track_arret$ref), ]
tags_track_arret <- tags_track_arret[tags_track_arret$k %in% c("id","railway","public_transport","highway","name"),]
tags_track_arret <- data.frame(cast(tags_track_arret, id ~ k,value = "v"))
tags_track_arret <- tags_track_arret[tags_track_arret$railway %in% c("halt","station","stop","tram_stop","buffer_stop") |
                                       tags_track_arret$public_transport %in% c("stop_position","station","platform"),]
refs_track_arret <- refs_track_arret[refs_track_arret$ref %in% unique(tags_track_arret$id),] #On ne retient que les points des voies correspondant à un arrêt
tags_points_track <- osm_france$nodes$tags[osm_france$node$tags$k %in% c("railway","public_transport","name"),]
tags_points_track <- as.data.frame(cast(tags_points_track[tags_points_track$id %in% unique(refs_track_arret$ref),], id ~ k,value = "v"))

refs_track_arret[,c("railway","public_transport","name")] <- tags_points_track[match(refs_track_arret$ref,
                                                                                     tags_points_track$id),
                                                                               c("railway","public_transport","name")]
#On retire les arrêts identifiés dans des routes
refs_track_arret <- refs_track_arret[!(refs_track_arret$ref %in% unique(refs_route_arret$ref)) ,] 

#Ajout des identifiants route

refs_route_track_arret <- track_route[track_route$id_track %in% unique(refs_track_arret$id_track),c("id_route","id_track")] 
if(nrow(refs_route_track_arret)>0){
  refs_route_track_arret <- merge(refs_route_track_arret,refs_track_arret,by = "id_track")
  refs_route_track_arret <- subset(refs_route_track_arret, select = c("id_route","ref","railway","public_transport","name"))
  refs_route_track_arret$type = 'track'
  refs_route_track_arret$role = ""
  refs_route_track_arret$highway = ""
  refs_route_track_arret$building = NA
  refs_route_track_arret$id_node = refs_route_track_arret$ref
}

#Fusion des arrêts node, way et type_node
refs_route_arret <- rbind(refs_route_arret_node,refs_route_arret_way,refs_route_track_arret)

#Recherche d'un stop area pour tous les arrêts

id_stop_area <- osm_france$relations$tags[osm_france$relations$tags$k=="public_transport" & osm_france$relations$tags$v=="stop_area","id"]
stop_area <- subset_osm_table(osm_france,id_relations =id_stop_area)
refs_stop_area <- stop_area$relations$refs 
refs_stop_area <- rename(refs_stop_area,replace= c(id="id_stop_area"))
refs_route_arret[,"id_stop_area"] <- refs_stop_area[match(refs_route_arret$ref,refs_stop_area$ref),c("id_stop_area")]

#Coordonnées des arrêts 
refs_route_arret[,c("x","y")] <- osm_france$nodes$coords[match(refs_route_arret$id_node,osm_france$nodes$coords$id),c("x","y")]

#Ajout d'un centroide pour chaque objet de type way
centroid_refs_route_arret <- aggregate(cbind(x,y) ~ ref, data =  refs_route_arret[refs_route_arret$type=="way",],FUN = mean,na.action= )
centroid_refs_route_arret <- merge(centroid_refs_route_arret,
                                   refs_route_arret[refs_route_arret$type=="way" & duplicated(refs_route_arret$ref)==FALSE,
                                                    c("id_route","ref","railway","public_transport","highway","building","role","type","id_stop_area","name"),],
                                   by = "ref")                                                            
centroid_refs_route_arret$id_node <- paste("CRT",centroid_refs_route_arret$ref,sep="")
refs_route_arret <- rbind(refs_route_arret,centroid_refs_route_arret)

#REPERAGE DES CLUSTER D'ARRET (PSEUDO STOP_AREA)
#Repérage des objets "arrêts" proches les uns des autres sur une ligne (clusters) 
find_cluster_arret <- function(ident){
  #print(ident)
  #ident <- "4109606"
  route_arret <- refs_route_arret[refs_route_arret$id_route==ident,]
  route_arret <- route_arret[!(duplicated(route_arret$id_node)),c("id_node","x","y")]
  row.names(route_arret) <- route_arret$id_node
  if(nrow(route_arret)>1){
    cluster_arret <- hclust(dist(route_arret[,c("x","y")]), method="single")
    cluster <- data.frame(cluster = cutree(cluster_arret, h=200)) 
    cluster$id_node <- row.names(cluster)
  }else{
    route_arret$cluster <- 1 
    cluster <- route_arret[,c("cluster","id_node")]
  }
  cluster$id_route <- ident
  return(cluster)
}

cluster_arret <- lapply(unique(refs_route_arret$id_route),find_cluster_arret)
cluster_arret <- do.call(rbind,cluster_arret)
refs_route_arret <- merge(cluster_arret,refs_route_arret,by=c('id_route','id_node'))
refs_route_arret$id_cluster <- paste(refs_route_arret$id_route,refs_route_arret$cluster,sep="CL")


#CONSOLIDATION DES CLUSTER ET CREATION DE STOP_AREA_NEW
new_stop_area <- count(refs_route_arret,c('id_node','id_cluster','id_stop_area','ref'))
id_stop_area_new <- data.frame(id_cluster = unique(refs_route_arret$id_cluster))
id_stop_area_new$id_stop_area_new <- paste("FICSA",str_pad(row.names(id_stop_area_new), 6, pad = "0"),sep="")
new_stop_area$id_stop_area_new <- id_stop_area_new[match(new_stop_area$id_cluster,id_stop_area_new$id_cluster),"id_stop_area_new"]

#replacement de l'identifiant fictif par un identifiant existant 
new_stop_area$id_stop_area_new <- ifelse(is.na(new_stop_area$id_stop_area),
                                         new_stop_area$id_stop_area_new,
                                         new_stop_area$id_stop_area)

#Un ref (node ou way) est dans un seul id_stop_area_new
for(i in 1:999){
  print(i)
  unique_id <- split(new_stop_area$id_stop_area_new,new_stop_area[,"ref"])
  unique_id <- lapply(unique_id,unique)
  unique_id <- unique_id[unlist(lapply(unique_id,length)) > 1]
  if(length(unique_id)==0){break}
  unique_id <- lapply(unique_id,function(x) x[order(x)])
  unique_id <- unique_id[!duplicated(unique_id)] 
  unique_id <- lapply(unique_id,function(x) data.frame(id_stop_area_unique = x[[1]],id_stop_area_new = x,stringsAsFactors = FALSE))
  unique_id <- do.call(rbind,unique_id)
  new_stop_area$id_stop_area_unique <- unique_id[match(new_stop_area$id_stop_area_new,unique_id$id_stop_area_new),
                                                 "id_stop_area_unique"]
  new_stop_area$id_stop_area_new <- ifelse(is.na(new_stop_area$id_stop_area_unique),
                                           new_stop_area$id_stop_area_new,
                                           new_stop_area$id_stop_area_unique)
}

new_stop_area <- new_stop_area[!duplicated(new_stop_area$id_node),c("id_node","id_stop_area_new")]
refs_route_arret$id_stop_area_new <- new_stop_area[match(refs_route_arret$id_node,new_stop_area$id_node),"id_stop_area_new"]

#refs_route_arret[refs_route_arret$id_stop_area_new == "FICSA007496",]
#FICSA007509,4764967

#SUPPRIMER LES DOUBLONS DE ROUTE -> MEMES STOPS DANS LE MEME ORDRE 

#SELECTION DES STOP POUR CHAQUE CLUSTER

#ETAPE 1 : Recherche d'un "stop" dans chaque id_stop_area_new  (Prévoir ultérieurement le cas où toutes les stations sont dans un stop area)
refs_route_arret$public_transport <- ifelse(is.na(refs_route_arret$public_transport),"",as.character(refs_route_arret$public_transport))
refs_route_arret$railway <- ifelse(is.na(refs_route_arret$railway),"",as.character(refs_route_arret$railway))
refs_route_arret$is_point_track <- refs_route_arret$id_node %in% unique(track$ways$refs$ref)
refs_route_arret$is_stop <- ((refs_route_arret$railway %in% c("stop","tram_stop","buffer_stop") | 
                                refs_route_arret$public_transport == "stop_position" |
                                substr(refs_route_arret$role,1,4)=="stop") &
                               refs_route_arret$is_point_track == TRUE)

nb_stop <- count(refs_route_arret,c("id_stop_area_new","id_route","is_stop"))
nb_stop <- data.frame(cast(nb_stop, id_stop_area_new + id_route ~ is_stop,value="freq"))
nb_stop <- rename(nb_stop,replace=c("TRUE."="nb_stop"))
nb_stop <- subset(nb_stop,select=c("id_stop_area_new","id_route","nb_stop"))
nb_stop[is.na(nb_stop$nb_stop),"nb_stop"] <- 0
refs_route_arret <- merge(refs_route_arret,nb_stop[,c("id_stop_area_new","id_route","nb_stop")],by = c("id_stop_area_new","id_route"))

missing_stop_etape1 <- nb_stop[nb_stop$nb_stop == 0,]

#ETAPE 2 : Si toujours pas de "stop", on choisit le point appartenant aux tracks le plus proche du centroid du cluster
ctr_stop_area_new <- aggregate(cbind(x,y) ~ id_stop_area_new , data =  refs_route_arret,FUN = mean )
row.names(ctr_stop_area_new) <- ctr_stop_area_new$id_stop_area_new
ctr_stop_area_new <- rename(ctr_stop_area_new, replace= c(x="xctr",y="yctr"))
ctr_stop_area_new <- SpatialPointsDataFrame(ctr_stop_area_new[,c("xctr","yctr")],ctr_stop_area_new,proj4string = CRS("+init=EPSG:3035"))

#Fichier cartographique des points des tracks 

point_track <- osm_france$ways$refs[osm_france$ways$refs$id %in% track_route$id_track,]
point_track <- rename(point_track,replace=c(id="id_track",ref="id_node"))
#RENDRE PLUS RAPIDE AVEC UN MATCH
#point_track[,c("x","y")] <- osm_france$nodes$coords[as.character(point_track$id_node),c("x","y")]
point_track[,c("x","y")] <- osm_france$nodes$coords[match(point_track$id_node,osm_france$nodes$coords$id),c("x","y")]

#Fonction de recherche des points des stop_area_new sans stop
find_proxy_node <- function(ident){
  #print(ident)
  #ident <- unique(missing_stop_etape1$id_stop_area_new)[2]
  #ident <- "6363919"  # ->  CORRIGER LES RELATIONS AVEC DES TROUS
  #ident <- "6564104"
  #ident <- "FICSA013229"
  
  id_route_missing <- unique(refs_route_arret[refs_route_arret$id_stop_area_new==ident,]$id_route)
  missing_ctr <- coordinates(ctr_stop_area_new[ctr_stop_area_new$id_stop_area_new==ident,])
  track_route_missing <- track_route[track_route$id_route %in% id_route_missing,]
  point_track_missing <- point_track[point_track$id_track %in% track_route_missing$id_track,]
  point_track_missing <- point_track_missing[abs(point_track_missing$x - missing_ctr[,"xctr"]) <= 1000,]
  point_track_missing <- point_track_missing[abs(point_track_missing$y - missing_ctr[,"yctr"]) <= 1000,]
  
  point_track_route_missing <- merge(track_route_missing,point_track_missing,by="id_track")
  point_track_route_missing$dist <- sqrt((point_track_route_missing$x - missing_ctr[,"xctr"])**2 + (point_track_route_missing$y - missing_ctr[,"yctr"])**2)
  point_track_route_missing <- point_track_route_missing[order(point_track_route_missing$dist),]
  proxy_node <- point_track_route_missing[!duplicated(point_track_route_missing$id_route),c("id_node","id_route","dist","x","y")]
  if(nrow(proxy_node)< length(id_route_missing)){
    proxy_node_missing <- data.frame(id_route = id_route_missing[!(id_route_missing %in% proxy_node$id_route)],id_node = "MISSING",dist = 99999,x=NA,y=NA)
    proxy_node = rbind(proxy_node,proxy_node_missing)
  }
  proxy_node$id_stop_area_new <- ident 
  
  
  #ET PERMETTRE DE PROJETTER UNE LIGNE QUAND LE POINT EST TROP LOIN 
  
  return(proxy_node)
}

search_stop_track <- lapply(unique(missing_stop_etape1$id_stop_area_new),find_proxy_node)
search_stop_track <- do.call(rbind,search_stop_track)

found_stop <-  search_stop_track[search_stop_track$id_node != "MISSING",] 
definitely_missing_stop <- search_stop_track[search_stop_track$id_node == "MISSING",] 

stop_route <- rbind(refs_route_arret[refs_route_arret$is_stop==TRUE,c("id_stop_area_new","id_route","id_node","x","y")],
                    found_stop[found_stop$dist<100,c("id_stop_area_new","id_node","id_route","x","y")])

stop_route <- rename(stop_route,replace = c(id_node = "id_stop",id_stop_area_new = "id_stop_area"))

name_stop <- count(refs_route_arret,c("id_stop_area_new","name"))
name_stop <- rename(name_stop,replace = c(id_stop_area_new = "id_stop_area"))
name_stop <- name_stop[!is.na(name_stop$name),]
name_stop <- name_stop[order(name_stop$id_stop_area,-name_stop$freq),]
name_stop <- name_stop[!duplicated(name_stop$id_stop_area),] 

stop_route$name_stop <- name_stop[match(stop_route$id_stop_area,name_stop$id_stop_area),"name"]

map_stop <- stop_route[!duplicated(stop_route$id_stop),c("id_stop_area","id_stop","name_stop","x","y")]
row.names(map_stop) <- map_stop$id_stop
map_stop <- SpatialPointsDataFrame(map_stop[,c("x","y")],map_stop,proj4string = CRS("+init=epsg:2154"))

write.csv(data_route,"W:/Bureau/OSM/Data/final/data_route.csv",row.names = FALSE)
write.csv(track_route,"W:/Bureau/OSM/Data/final/track_route.csv",row.names = FALSE)
write.csv(stop_route[,c("id_stop_area","id_route","id_stop")],"W:/Bureau/OSM/Data/final/stop_route.csv",row.names = FALSE)
writeOGR(map_track,"W:/Bureau/OSM/Data/final/map_track.shp",layer = "map_track", driver="ESRI Shapefile",overwrite_layer = TRUE) 
writeOGR(map_stop,"W:/Bureau/OSM/Data/final/map_stop.shp",layer = "map_stop", driver="ESRI Shapefile",overwrite_layer = TRUE)

#########################################################################################





region <- readOGR("W:/Bureau/OSM/Data/ADE/REGION.shp")
region <- region[!(region$NOM_REG %in% c("La RÃ©union","Martinique","Guadeloupe","Mayotte","Guyane")),]
region <- spTransform(region, CRS("+init=epsg:3035"))
region <- gSimplify(region, tol=100)


pdf("W:/Bureau/OSM/Map/essai_tram.pdf")

plot(region,lwd = 0.05, border = "lightgrey")
plot(map_track,lwd = 0.05,col = "grey",add=TRUE)
plot(map_track[map_track$id_track %in% track_route[track_route$id_route %in% data_route[data_route$route == 'tram',"id_route"],"id_track"],],lwd = 0.02,col="blue",add=TRUE)
plot(map_track[map_track$id_track %in% track_route[track_route$id_route %in% data_route[data_route$route == 'light_rail',"id_route"],"id_track"],],add=TRUE,col="green",lwd = 0.02)
plot(map_track[map_track$id_track %in% track_route[track_route$id_route %in% data_route[data_route$route == 'subway',"id_route"],"id_track"],],add=TRUE,col = "red",lwd = 0.02)

plot(map_stop[map_stop$id_stop %in% stop_route[stop_route$id_route %in% data_route[data_route$route %in% c('tram','light_rail','subway'),"id_route"],"id_stop"],],add=TRUE,cex = 0.02,lwd = 0.02)

dev.off()

pdf("W:/Bureau/OSM/Map/essai_train.pdf")

plot(region,lwd = 0.05, border = "lightgrey")
plot(map_track[map_track$id_track %in% track_route[track_route$id_route %in% data_route[data_route$route == 'train',"id_route"],"id_track"],],lwd = 0.02,add=TRUE)
plot(map_stop[map_stop$id_stop %in% stop_route[stop_route$id_route %in% data_route[data_route$route %in% c('train'),"id_route"],"id_stop"],],add=TRUE,cex = 0.05,lwd = 0.05,col="red")

dev.off()







##############
#ROUTE MASTER#
##############

#Il manque des 

#liste_route <- unique(stop_route$id_route)

#dist_routes <- function(route1){
#  id_stop_route1 <- unique(stop_route[stop_route$id_route == route1,"id_stop"])
#  stop_route1 <- map_stop[map_stop$id_stop %in% id_stop_route1,]

#  dist_route <- function(route2){
#    id_stop_route2 <- unique(stop_route[stop_route$id_route == route2,"id_stop"])
#    stop_route2 <- map_stop[map_stop$id_stop %in% id_stop_route2,]

#    dist_route <- gDistance(stop_route1,stop_route2 , byid = TRUE )
#    dist_min <- do.call(rbind,lapply(row.names(dist_route),function(x) min(dist_route[x,])))
#    dist_min <- data.frame(dist_min  = dist_min, route1 = route1, route2 = route2, stop_route = row.names(dist_route))
#    return(dist_min)
#  }
#  dist_route1 <- do.call(rbind,lapply(unique(stop_route$id_route),dist_route))
#  return(dist_route1)
#}

#dist_stop_routes <- do.call(rbind,lapply(unique(stop_route$id_route),dist_routes))
#dist_stop_routes <- rbind(dist_stop_routes,rename(dist_stop_routes,replace=c(route1="route2",route2="route1"))) 
#dist_stop_routes <- dist_stop_routes[dist_stop_routes$route1 > dist_stop_routes$route2, ]
#dist_stop_routes_max <-  aggregate(data = dist_stop_routes,dist_min ~ route1 + route2,FUN =max)
#dist_stop_routes_max$name1 <- data_route[match(dist_stop_routes_max$route1, data_route$id_route),"name"]
#dist_stop_routes_max$name2 <- data_route[match(dist_stop_routes_max$route2, data_route$id_route),"name"]
##Routes les plus proches
#routes_proches <- dist_stop_routes_max[dist_stop_routes_max$dist_min < 200 & dist_stop_routes_max$route1 != dist_stop_routes_max$route2,]

#map_stop[map_stop$id_stop %in% unique(stop_route[stop_route$id_route == "7902381","id_stop"]),]
#map_stop[map_stop$id_stop %in% unique(stop_route[stop_route$id_route == "6786089","id_stop"]),]


###########
#STOP AREA#
###########

#writeOGR(map_track, "W:/Bureau/OSM/map/track.shp",layer="map_track",driver="ESRI Shapefile")


#Average track
#https://wiki.openstreetmap.org/wiki/R_(programming_language)/Average_tracks