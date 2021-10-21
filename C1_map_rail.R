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
source("Z_functions.R")

options(scipen = 15)

osm_france <- read_oms_table("Rail/rail_osm_france")

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

#Listes utiles dans tous le programme
list_route_type <- c("train","tram","subway","light_rail")
list_track_type <- c("rail","tram","subway","light_rail","construction","narrow_gauge","disused","psv")

#Coordonnées des points en LAEA

osm_france$nodes$coords[,c('x','y')] <- st_coordinates(st_transform(st_as_sf(osm_france$nodes$coords,coords=c("lon","lat"),crs = 4326),crs=3035))

################
#Donnees tracks#
################

#Données srur les voies
data_track <- osm_france$ways$tags[osm_france$ways$tags$k %in% c("usage","train","service","railway","importance","name","id"),]
data_track <- dcast(data=as.data.table(data_track),id~k,value.var = "v")
data_track <- data_track[data_track$railway %in% list_track_type,]
data_track <- plyr::rename(data_track,replace = c(id="id_trk"))

#Nom des lignes de chemin de fer
name_lines <-  osm_france$relations$tags[osm_france$relations$tags$k %in% c("route","name"),]
name_lines <-  dcast(data=as.data.table(name_lines),id~k,value.var = "v")
name_lines <- name_lines[name_lines$route=="railway",]
name_lines <- rename(name_lines,replace = c("id"="id_line","name"="name_line")) 

ref_track <- osm_france$relations$refs[route_rw$relations$refs$type=="way",c("id","ref")]
ref_track <- rename(ref_track,replace = c("ref"='id_trk',"id"="id_line")) 
name_lines <- left_join(name_lines,ref_track)
name_lines <- subset(name_lines,select=c("id_line","id_trk","name_line"))
data_track[,c("name_line",'id_line')] <- name_lines[match(data_track$id_trk,name_lines$id_trk),c("name_line",'id_line')]

#Carte 
map_track <- osm_france$ways$refs
map_track <- map_track[map_track$id %in% data_track$id_trk,]
map_track <- plyr::rename(map_track,c("id"="id_trk"))
map_track[,c('x','y')] <- osm_france$nodes$coords[match(map_track$ref, osm_france$nodes$coords$id),c('x','y')]
map_track <- split(map_track[,c("x","y")],map_track$id_trk)
map_track <- lapply(map_track,as.matrix)
id_map_track <- names(map_track)
map_track <- lapply(map_track,st_linestring)
map_track <- st_sfc(map_track, crs = 3035)
map_track <- st_sf(data.table(id_trk=as.character(id_map_track)), geometry = map_track)
map_track <- left_join(map_track,data_track,by="id_trk")
map_track <- cbind(map_track,do.call(rbind,lapply(st_geometry(map_track),st_bbox)))

map_track$len_trk <- as.numeric(st_length(map_track))
map_track <- map_track[map_track$len_trk>0,]
data_track <- data_track[data_track$id_trk %in% map_track$id_trk,]
###############
#Donnees route#
###############

####Données sur les routes
data_route <- osm_france$relations$tags
data_route <- data_route[data_route$k %in% c("from","to","route","name","operator","network","id"),]
data_route <- dcast(data=as.data.table(data_route),id ~ k,value.var = "v")

#Seulement les routes tram, train, métro et train léger 
data_route <- data_route[data_route$route %in% list_route_type,]
data_route <- plyr::rename(data_route,replace = c("id"="id_route"))

###Référence des objets routes 
refs_route <- osm_france$relations$refs
refs_route <- rename(refs_route,replace = c(id="id_route"))

#Données des noeuds 
data_node <- osm_france$nodes$tags
data_node <- data_node[data_node$k %in% c("railway","public_transport","highway","building","name"),]
data_node <- dcast(data=as.data.table(data_node),id ~ k,value.var = "v")
#Données des ways 
data_way <- osm_france$ways$tags
data_way <- data_way[data_way$k %in% c("railway","public_transport","highway","building","name"),]
data_way <- dcast(data=as.data.table(data_way),id ~ k,value.var = "v")

#Données sur les références des routes 
refs_route <- left_join(refs_route,plyr::rename(rbind.fill(list(data_node,data_way)),c("id"="ref")),by="ref")

####Sélection des routes renvoyant à des tracks (et retrait des bus)
#Longueur des tracks
refs_route <- left_join(refs_route,plyr::rename(st_drop_geometry(map_track)[,c("id_trk","len_trk")],c("id_trk"="ref")))
refs_route[is.na(refs_route$len_trk),"len_trk"] <- 0
#Type de track 
refs_route$routier <- as.numeric(!is.na(refs_route$highway) & refs_route$highway %in% c("primary","residential","secondary","tertiary"))
refs_route$ferroviaire <- as.numeric(!is.na(refs_route$railway) & refs_route$railway %in% list_track_type)
#Longueur des voies de chaque type 
stat_route <- aggregate(data=refs_route,cbind(len_trk,routier,ferroviaire)~id_route,FUN=sum)
stat_route <- stat_route[stat_route$len_trk > 0 & stat_route$ferroviaire > stat_route$routier/4 ,]
#Sélection des route
refs_route <- refs_route[refs_route$id_route %in% stat_route$id_route,]
data_route <- data_route[data_route$id_route %in% stat_route$id_route,]

#Carte des tracks 
track_route <- refs_route[refs_route$type=="way"& refs_route$ref %in% map_track$id_trk,c("id_route","ref")]
track_route <- rename(track_route,replace=c(ref="id_trk"))

#Suppression des tracks sans route (notamment les voies de service et de fret)
map_track <- map_track[map_track$id_trk %in% unique(track_route$id_trk),]
data_track <- data_track[data_track$id_trk %in% unique(track_route$id_trk),]

################
#Données Lignes#
################
#Master route
#id_route_master <- find(route_master_network,relation(tags(k=="route_master")))
#route_master <- subset(route_master_network,relation_ids = id_route_master)
#Selection des masters routes 
#refs_route_master <- route_master$relations$refs[route_master$relations$refs$ref %in% unique(data_route$id_route),]  
#Donn?es des masters routes 
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
#R?cup?ration des points des ways
refs_route_arret_way <- osm_france$ways$refs[osm_france$ways$refs$id %in% unique(refs_route_arret$ref),]
refs_route_arret_way <- rename(refs_route_arret_way,replace = c(id = "ref",ref = "id_node"))
refs_route_arret_way <- merge(refs_route_arret_way,refs_route_arret[refs_route_arret$type=='way',], by = "ref")

#Pour les tram et les m?tros, r?cup?ration de tous les arr?ts trouv?s sur voie (on part du principe que tout arr?t travers? est desservi pour ce type de transport)
refs_track_arret <- osm_france$ways$refs[osm_france$ways$refs$id %in% unique(map_track[map_track$railway %in% c("subway","tram"),]$id_trk),]
refs_track_arret <- rename(refs_track_arret,replace= c(id = "id_trk"))
tags_track_arret <- osm_france$nodes$tags[osm_france$nodes$tags$id %in% unique(refs_track_arret$ref), ]
tags_track_arret <- tags_track_arret[tags_track_arret$k %in% c("id","railway","public_transport","highway","name"),]
tags_track_arret <- data.frame(cast(tags_track_arret, id ~ k,value = "v"))
tags_track_arret <- tags_track_arret[tags_track_arret$railway %in% c("halt","station","stop","tram_stop","buffer_stop") |
                                       tags_track_arret$public_transport %in% c("stop_position","station","platform"),]
refs_track_arret <- refs_track_arret[refs_track_arret$ref %in% unique(tags_track_arret$id),] #On ne retient que les points des voies correspondant ? un arr?t
tags_points_track <- osm_france$nodes$tags[osm_france$node$tags$k %in% c("railway","public_transport","name"),]
tags_points_track <- as.data.frame(cast(tags_points_track[tags_points_track$id %in% unique(refs_track_arret$ref),], id ~ k,value = "v"))

refs_track_arret[,c("railway","public_transport","name")] <- tags_points_track[match(refs_track_arret$ref,
                                                                                     tags_points_track$id),
                                                                               c("railway","public_transport","name")]
#On retire les arr?ts identifi?s dans des routes
refs_track_arret <- refs_track_arret[!(refs_track_arret$ref %in% unique(refs_route_arret$ref)) ,] 

#Ajout des identifiants route
refs_route_track_arret <- track_route[track_route$id_trk %in% unique(refs_track_arret$id_trk),c("id_route","id_trk")] 
if(nrow(refs_route_track_arret)>0){
  refs_route_track_arret <- merge(refs_route_track_arret,refs_track_arret,by = "id_trk")
  refs_route_track_arret <- subset(refs_route_track_arret, select = c("id_route","ref","railway","public_transport","name"))
  refs_route_track_arret$type = 'track'
  refs_route_track_arret$role = ""
  refs_route_track_arret$highway = ""
  refs_route_track_arret$building = NA
  refs_route_track_arret$id_node = refs_route_track_arret$ref
}

#Fusion des arr?ts node, way et type_node
refs_route_arret <- rbind.fill(list(refs_route_arret_node,refs_route_arret_way,refs_route_track_arret))

#Recherche d'un stop area pour tous les arr?ts
id_sarea <- osm_france$relations$tags[osm_france$relations$tags$k=="public_transport" & osm_france$relations$tags$v=="stop_area","id"]
refs_stop_area <- osm_france$relations$refs 
refs_stop_area <- refs_stop_area[refs_stop_area$id %in% id_sarea,]
refs_stop_area <- plyr::rename(refs_stop_area,replace= c(id="id_sarea"))
refs_route_arret[,"id_sarea"] <- refs_stop_area[match(refs_route_arret$ref,refs_stop_area$ref),c("id_sarea")]

#Coordonn?es des arr?ts 
refs_route_arret[,c("x","y")] <- osm_france$nodes$coords[match(refs_route_arret$id_node,osm_france$nodes$coords$id),c("x","y")]

#Ajout d'un centroide pour chaque objet de type way
centroid_refs_route_arret <- aggregate(cbind(x,y) ~ ref, data =  refs_route_arret[refs_route_arret$type=="way",],FUN = mean,na.rm= TRUE)
centroid_refs_route_arret <- merge(centroid_refs_route_arret,
                                   refs_route_arret[refs_route_arret$type=="way" & duplicated(refs_route_arret$ref)==FALSE,
                                                    c("id_route","ref","railway","public_transport","highway","building","role","type","id_sarea","name"),],
                                   by = "ref")                                                            
centroid_refs_route_arret$id_node <- paste("CRT",centroid_refs_route_arret$ref,sep="")
refs_route_arret <- rbind.fill(list(refs_route_arret,centroid_refs_route_arret))

#REPERAGE DES CLUSTER D'ARRET (PSEUDO STOP_AREA)
#Rep?rage des objets "arr?ts" proches les uns des autres sur une ligne (clusters) 
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
new_stop_area <- count(refs_route_arret,c('id_node','id_cluster','id_sarea','ref'))
id_sarea_new <- data.frame(id_cluster = unique(refs_route_arret$id_cluster))
id_sarea_new$id_sarea_new <- paste("FICSA",str_pad(row.names(id_sarea_new), 6, pad = "0"),sep="")
new_stop_area$id_sarea_new <- id_sarea_new[match(new_stop_area$id_cluster,id_sarea_new$id_cluster),"id_sarea_new"]

#replacement de l'identifiant fictif par un identifiant existant 
new_stop_area$id_sarea_new <- ifelse(is.na(new_stop_area$id_sarea),
                                         new_stop_area$id_sarea_new,
                                         new_stop_area$id_sarea)

#Un ref (node ou way) est dans un seul id_sarea_new
for(i in 1:999){
  print(i)
  unique_id <- split(new_stop_area$id_sarea_new,new_stop_area[,"ref"])
  unique_id <- lapply(unique_id,unique)
  unique_id <- unique_id[unlist(lapply(unique_id,length)) > 1]
  if(length(unique_id)==0){break}
  unique_id <- lapply(unique_id,function(x) x[order(x)])
  unique_id <- unique_id[!duplicated(unique_id)] 
  unique_id <- lapply(unique_id,function(x) data.frame(id_sarea_unique = x[[1]],id_sarea_new = x,stringsAsFactors = FALSE))
  unique_id <- do.call(rbind,unique_id)
  new_stop_area$id_sarea_unique <- unique_id[match(new_stop_area$id_sarea_new,unique_id$id_sarea_new),
                                                 "id_sarea_unique"]
  new_stop_area$id_sarea_new <- ifelse(is.na(new_stop_area$id_sarea_unique),
                                           new_stop_area$id_sarea_new,
                                           new_stop_area$id_sarea_unique)
}

new_stop_area <- new_stop_area[!duplicated(new_stop_area$id_node),c("id_node","id_sarea_new")]
refs_route_arret$id_sarea_new <- new_stop_area[match(refs_route_arret$id_node,new_stop_area$id_node),"id_sarea_new"]

#refs_route_arret[refs_route_arret$id_sarea_new == "FICSA007496",]
#FICSA007509,4764967

#SUPPRIMER LES DOUBLONS DE ROUTE -> MEMES STOPS DANS LE MEME ORDRE 

#SELECTION DES STOP POUR CHAQUE CLUSTER

#ETAPE 1 : Recherche d'un "stop" dans chaque id_sarea_new  (Pr?voir ult?rieurement le cas o? toutes les stations sont dans un stop area)
refs_route_arret$public_transport <- ifelse(is.na(refs_route_arret$public_transport),"",as.character(refs_route_arret$public_transport))
refs_route_arret$railway <- ifelse(is.na(refs_route_arret$railway),"",as.character(refs_route_arret$railway))
refs_route_arret$is_point_track <- refs_route_arret$id_node %in% osm_france$ways$refs[osm_france$ways$refs$id %in% unique(map_track$id_trk),]$ref 
refs_route_arret$is_stop <- ((refs_route_arret$railway %in% c("stop","tram_stop","buffer_stop") | 
                                refs_route_arret$public_transport == "stop_position" |
                                substr(refs_route_arret$role,1,4)=="stop") &
                               refs_route_arret$is_point_track == TRUE)

nb_stop <- plyr::count(refs_route_arret,c("id_sarea_new","id_route","is_stop"))
nb_stop <- data.frame(cast(nb_stop, id_sarea_new + id_route ~ is_stop,value="freq"))
nb_stop <- rename(nb_stop,replace=c("TRUE."="nb_stop"))
nb_stop <- subset(nb_stop,select=c("id_sarea_new","id_route","nb_stop"))
nb_stop[is.na(nb_stop$nb_stop),"nb_stop"] <- 0
refs_route_arret <- merge(refs_route_arret,nb_stop[,c("id_sarea_new","id_route","nb_stop")],by = c("id_sarea_new","id_route"))

missing_stop_etape1 <- nb_stop[nb_stop$nb_stop == 0,]

#ETAPE 2 : Si toujours pas de "stop", on choisit le point appartenant aux tracks le plus proche du centroid du cluster
ctr_stop_area_new <- aggregate(cbind(x,y) ~ id_sarea_new , data =  refs_route_arret,FUN = mean )
row.names(ctr_stop_area_new) <- ctr_stop_area_new$id_sarea_new
ctr_stop_area_new <- rename(ctr_stop_area_new, replace= c(x="xctr",y="yctr"))
ctr_stop_area_new <- SpatialPointsDataFrame(ctr_stop_area_new[,c("xctr","yctr")],ctr_stop_area_new,proj4string = CRS("+init=EPSG:3035"))

#Fichier cartographique des points des tracks 

point_track <- osm_france$ways$refs[osm_france$ways$refs$id %in% track_route$id_trk,]
point_track <- rename(point_track,replace=c(id="id_trk",ref="id_node"))
#RENDRE PLUS RAPIDE AVEC UN MATCH
#point_track[,c("x","y")] <- osm_france$nodes$coords[as.character(point_track$id_node),c("x","y")]
point_track[,c("x","y")] <- osm_france$nodes$coords[match(point_track$id_node,osm_france$nodes$coords$id),c("x","y")]

#Fonction de recherche des points des stop_area_new sans stop
find_proxy_node <- function(ident){
  #print(ident)
  #ident <- unique(missing_stop_etape1$id_sarea_new)[2]
  #ident <- "6363919"  # ->  CORRIGER LES RELATIONS AVEC DES TROUS
  #ident <- "6564104"
  #ident <- "FICSA013229"
  
  id_route_missing <- unique(refs_route_arret[refs_route_arret$id_sarea_new==ident,]$id_route)
  missing_ctr <- coordinates(ctr_stop_area_new[ctr_stop_area_new$id_sarea_new==ident,])
  track_route_missing <- track_route[track_route$id_route %in% id_route_missing,]
  point_track_missing <- point_track[point_track$id_trk %in% track_route_missing$id_trk,]
  point_track_missing <- point_track_missing[abs(point_track_missing$x - missing_ctr[,"xctr"]) <= 1000,]
  point_track_missing <- point_track_missing[abs(point_track_missing$y - missing_ctr[,"yctr"]) <= 1000,]
  
  point_track_route_missing <- merge(track_route_missing,point_track_missing,by="id_trk")
  point_track_route_missing$dist <- sqrt((point_track_route_missing$x - missing_ctr[,"xctr"])**2 + (point_track_route_missing$y - missing_ctr[,"yctr"])**2)
  point_track_route_missing <- point_track_route_missing[order(point_track_route_missing$dist),]
  proxy_node <- point_track_route_missing[!duplicated(point_track_route_missing$id_route),c("id_node","id_route","dist","x","y")]
  if(nrow(proxy_node)< length(id_route_missing)){
    proxy_node_missing <- data.frame(id_route = id_route_missing[!(id_route_missing %in% proxy_node$id_route)],id_node = "MISSING",dist = 99999,x=NA,y=NA)
    proxy_node = rbind(proxy_node,proxy_node_missing)
  }
  proxy_node$id_sarea_new <- ident 
  
  
  #ET PERMETTRE DE PROJETTER UNE LIGNE QUAND LE POINT EST TROP LOIN 
  
  return(proxy_node)
}

search_stop_track <- lapply(unique(missing_stop_etape1$id_sarea_new),find_proxy_node)
search_stop_track <- do.call(rbind,search_stop_track)

found_stop <-  search_stop_track[search_stop_track$id_node != "MISSING",] 
definitely_missing_stop <- search_stop_track[search_stop_track$id_node == "MISSING",] 

stop_route <- rbind(refs_route_arret[refs_route_arret$is_stop==TRUE,c("id_sarea_new","id_route","id_node","x","y")],
                    found_stop[found_stop$dist<100,c("id_sarea_new","id_node","id_route","x","y")])

stop_route <- rename(stop_route,replace = c(id_node = "id_stop",id_sarea_new = "id_sarea"))

name_stop <- count(refs_route_arret,c("id_sarea_new","name"))
name_stop <- rename(name_stop,replace = c(id_sarea_new = "id_sarea"))
name_stop <- name_stop[!is.na(name_stop$name),]
name_stop <- name_stop[order(name_stop$id_sarea,-name_stop$freq),]
name_stop <- name_stop[!duplicated(name_stop$id_sarea),] 

stop_route$name_stop <- name_stop[match(stop_route$id_sarea,name_stop$id_sarea),"name"]

map_stop <- stop_route[!duplicated(stop_route$id_stop),c("id_sarea","id_stop","name_stop","x","y")]
row.names(map_stop) <- map_stop$id_stop
map_stop <- SpatialPointsDataFrame(map_stop[,c("x","y")],map_stop,proj4string = CRS("+init=epsg:3035"))

#fwrite(data_route,"Rail/data_route.csv")
fwrite(track_route,"Rail/track_route.csv")
fwrite(stop_route[,c("id_sarea","id_route","id_stop")],"Rail/stop_route.csv")
#st_write(map_track,"Rail/map_track.shp",delete_dsn=TRUE) 
map_track <- plyr::rename(map_track,c("importance"="import"))
map_track$name <- with(map_track,ifelse(is.na(name),name_line,name))
map_track <- subset(map_track,select=-name_line)
writeOGR(as(map_track,"Spatial"),"Rail/map_track.shp",layer = "map_stop", driver="ESRI Shapefile",overwrite_layer = TRUE)
writeOGR(map_stop,"Rail/map_stop.shp",layer = "map_stop", driver="ESRI Shapefile",overwrite_layer = TRUE)

#########################################################################################
map_track <- st_read("Rail/map_track.shp")
map_stop <- st_read("Rail/map_stop.shp")
track_route <- fread("Rail/track_route.csv",colClasses = c("id_trk"="character","id_route"="character"))
stop_route <- fread("Rail/stop_route.csv",colClasses = "character")

path_map_ign <- "ADMIN-EXPRESS-COG_2-1__SHP__FRA_2020-11-20/ADMIN-EXPRESS-COG/1_DONNEES_LIVRAISON_2020-11-20/ADE-COG_2-1_SHP_WGS84G_FRA"
download_regions_ign()
region <- st_read(paste0("IGN/",path_map_ign,"/REGION.shp"))
region <- region[!(region$NOM_REG %in% c("La Réunion","Martinique","Guadeloupe","Mayotte","Guyane")),]
region <- st_transform(region,crs=3035)
region <- st_simplify(region,dTolerance = 100)

pdf("Map/essai_tram.pdf")

plot(st_geometry(region),lwd = 0.05, border = "lightgrey")
plot(st_geometry(map_track),lwd = 0.05,col = "grey",add=TRUE)

plot(st_geometry(map_track[map_track$id_trk %in% track_route[track_route$id_route %in% data_route[data_route$route == 'tram',"id_route"],"id_trk"],]),lwd = 0.02,col="blue",add=TRUE)
plot(st_geometry(map_track[map_track$id_trk %in% track_route[track_route$id_route %in% data_route[data_route$route == 'light_rail',"id_route"],"id_trk"],]),add=TRUE,col="green",lwd = 0.02)
plot(st_geometry(map_track[map_track$id_trk %in% track_route[track_route$id_route %in% data_route[data_route$route == 'subway',"id_route"],"id_trk"],]),add=TRUE,col = "red",lwd = 0.02)

plot(st_geometry(map_stop[map_stop$id_stop %in% stop_route[stop_route$id_route %in% data_route[data_route$route %in% c('tram','light_rail','subway'),"id_route"],"id_stop"],]),add=TRUE,cex = 0.02,lwd = 0.02)

dev.off()

plot(st_geometry(map_track[map_track$id_trk %in% track_route[track_route$id_route %in% data_route[data_route$route == 'subway',"id_route"],"id_trk"],]),col = "red",lwd = 0.02)


pdf("Map/essai_train.pdf")

plot(st_geometry(region),lwd = 0.05, border = "lightgrey")
plot(st_geometry(map_track[map_track$id_trk %in% track_route[track_route$id_route %in% data_route[data_route$route == 'train',"id_route"],"id_trk"],],lwd = 0.02,add=TRUE))
plot(st_geometry(map_stop[map_stop$id_stop %in% stop_route[stop_route$id_route %in% data_route[data_route$route %in% c('train'),"id_route"],"id_stop"],],add=TRUE,cex = 0.05,lwd = 0.05,col="red"))

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