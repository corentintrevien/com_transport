library(stringr)
library(plyr)
list.files("OpenDataTransport")
source("Z_functions.R")


#Données Transport.data.gouv.fr : tous les réseaux ferroviaires/tram/métro de France 
#(sauf tram de Genève et tram train Sarrebuck)

list_files <- list.files("OpenDataTransport")
list_files <- list_files[list_files != "SNCF"]
f <- "Intercites"

import_stops <- lapply(list_files,function(f){
  print(f)
  #Liaisons
  routes <- fread(paste0("OpenDataTransport/",f,"/routes.txt"))
  #Correction des données
  if(f == "TER"){routes$route_type <- 2} 
  if(f == "Bordeaux"){
    routes <- routes[route_short_name %in% c("A","B","C","D"),]
    routes$route_type <- 0} 
  if(f == "Clermont"){
    routes <- routes[route_short_name %in% c("A"),]
    routes$route_type <- 0} 
  if(f == "Tours"){
    routes <- routes[route_short_name %in% c("Tram A"),]
    routes$route_type <- 0} 
  if(f == "Saint-Etienne"){routes <- routes[route_short_name %in% c("T1","T2","T3"),]} 
  if(f == "Caen"){routes <- routes[routes$route_type == 0,]} 
  if(f == "Rouen"){routes <- routes[routes$route_type != 0,]} 
  if(f %in% c("Rouen",'Reims','Aubagne')){routes[route_type == 1,"route_type"] <- 0}
  
  #Sélection des liaisons ferrovaires 
  routes <- routes[route_type %in% 0:2,]
  
  #Trajets
  trips <- fread(paste0("OpenDataTransport/",f,"/trips.txt"))
  #Sélection des trajets ferrovaires 
  trips <- right_join(trips,routes[,c("route_id","route_type")])
  #Horaires
  stop_times <- fread(paste0("OpenDataTransport/",f,"/stop_times.txt"),colClasses = c("stop_id"="character"))
  #Sélection des stations desservies par un train
  stop_times <- right_join(stop_times,trips[,c("trip_id","route_type")])
  stop_times <- stop_times[,c("stop_id","route_type")]
  stop_times <- stop_times[!duplicated(stop_times),]
  #Stations
  stops <- fread(paste0("OpenDataTransport/",f,"/stops.txt"),colClasses = c("stop_id"="character"))
  #Stations ferroviaires
  stops <- right_join(stops,stop_times)
  if(f %in% c("TER","Intercites")){stops$stop_id <- unlist(lapply(strsplit(stops$stop_id,"-"),function(s) s[[length(s)]]))}
  stops$stop_id <- unlist(lapply(strsplit(stops$stop_id,":"),function(s) s[[length(s)]]))
  stops <- subset(stops,select=c(stop_id,stop_name,stop_lat,stop_lon,route_type))
  stops <- stops[!duplicated(stops[,c("stop_id","route_type")]),]
  stops <- stops[!duplicated(stops[,c("stop_name","route_type")]),]
  stops <- stops[!duplicated(stops[,c("stop_lat","stop_lon")]),]
  stops <- stops[!is.na(stop_lat),]
  stops$source <- f
  return(stops)
})

stops <- rbind.fill(import_stops)
stops <- setDT(stops)

#Conversion au format cartographique
map_stops <- st_as_sf(stops, coords = c("stop_lon", "stop_lat"), crs = 4326, agr = "constant")

#sélection des stations en France
region <- readOGR(paste0("IGN/",path_map_ign,"/REGION.shp"))
region <- region[!region$NOM_REG %in% c("Guadeloupe","Martinique","Mayotte","La Réunion","Guyane"),]
region <- st_as_sf(region)

map_stops$region <- as.numeric(st_intersects(map_stops,region))
map_stops$region <- region$NOM_REG[map_stops$region ]
map_stops <- map_stops[!is.na(map_stops$region),]

#Cartes de vérification
plot(st_geometry( map_stops[map_stops$route_type ==0,]),pch=".")
plot(st_geometry( map_stops[map_stops$route_type ==1,]),pch=".")
plot(st_geometry( map_stops[map_stops$route_type ==2,]),pch=".")

#Distance max entre les stations ayant le même identifiant
map_stops <- map_stops[map_stops$source != "Transilien",]
distance_stations <- st_distance(map_stops,map_stops)
quantile(apply(distance_stations,2,function(d) min(d[d != 0 ])),probs=0:10/10)

#Données SNCF open data
url_sncf <- "https://ressources.data.sncf.com/explore/dataset/referentiel-gares-voyageurs/download/?format=geojson&timezone=Europe/Berlin&lang=fr"
resp <- GET(url_sncf, encoding = "UTF-8")
gares_json <- fromJSON(content(resp, "text", encoding = "UTF-8"))

gares_data <- lapply(gares_json$features,function(g){ 
  coord <- g$geometry$coordinates
  if(is.null(coord)){coord <- NA}
  return(data.frame(stop_id = g$properties$uic_code,code_gare = g$properties$code_gare,
             stop_lon = coord[1],stop_lat = coord[2],
             stop_name = g$properties$gare_alias_libelle_noncontraint))})

gares_data <- rbindlist(gares_data)
gares_data$stop_id <- as.character(as.numeric(gares_data$stop_id))
gares_data <- gares_data[!is.na(stop_lon),]
#Conversion au format cartographique
map_gares <- st_as_sf(gares_data, coords = c("stop_lon", "stop_lat"), crs = 4326, agr = "constant")


plot(st_geometry(map_gares),pch=".")
plot(st_geometry(map_gares[!map_gares$stop_id %in% unique(stops[route_type==2,]$stop_id),]),pch=".",add=T,col="red")







library(geojsonR)
test <- FROM_GeoJson(url_file_string = )
#Données SNCF (en complément)
gares_SNCF  <- st_read("OpenDataTransport/SNCF/referentiel-gares-voyageurs.shp")
gares_SNCF



dist_id <- split(stops[source != "Transilien",],stops[source != "Transilien",]$stop_id)


dist_id <- lapply(dist_id, st_as_sf,coords = c("stop_lon", "stop_lat"), crs = 4326, agr = "constant")

pdf("Verif.pdf")
for(s in list_files){
  print(s)
  plot(st_geometry(map_stops[map_stops$source==s,]),pch = ".",col="white",main=s)
  plot(st_geometry(map_stops[map_stops$source==s & map_stops$route_type ==0,]),pch = 19,col="blue",add=T)
  plot(st_geometry(map_stops[map_stops$source==s & map_stops$route_type ==1,]),pch = 19,col="green",add=T)
  plot(st_geometry(map_stops[map_stops$source==s & map_stops$route_type ==2,]),pch = 19,col="red",add=T)
}
dev.off()

##############################################################################################################################

#Données ferroviaires OpenStreetMap 
map_stop_OSM <- st_read("Rail/map_stop.shp")
st_crs(map_stop_OSM) <- 3035
map_stop_OSM <- st_transform(map_stop_OSM,crs=4326)

route <- inner_join(fread("Rail/data_route.csv")[,c("id_route","route")],fread("Rail/stop_route.csv",colClasses = "character")[,c("id_stop","id_route")])
route <- route[route %in% c("train","light_rail"), ]
map_stop_OSM <- map_stop_OSM[map_stop_OSM$id_stop %in% route$id_stop,]

#Région d'appartenance


#Gares SNCF 
distance_SNCF_OSM <- st_distance(gares_SNCF,map_stop_OSM)
gares_SNCF$dist_OSM <- apply(distance_SNCF_OSM,1,min)
  
gares_SNCF$region <- as.numeric(st_intersects(gares_SNCF,region))
gares_SNCF$region <- region$NOM_REG[gares_SNCF$region ]
gares_SNCF <- gares_SNCF[!is.na(gares_SNCF$region),]

map_stop_OSM$dist_SNCF <-  apply(distance_SNCF_OSM,2,min,na.rm=T)
plot(st_geometry(gares_SNCF),pch=".",lwd=.1)
plot(st_geometry(gares_SNCF[gares_SNCF$dist_OSM>=200,]),pch=".",lwd=.2,col="red",add=T)

plot(st_geometry(map_stop_OSM),pch=".",lwd=.1)
plot(st_geometry(map_stop_OSM[map_stop_OSM$dist_SNCF>=200,]),pch=".",lwd=.2,col="red",add=T)
plot(st_geometry(gares_SNCF[gares_SNCF$gare_alias_=="Surdon",]),pch=".",col="green",add=T)


plot(st_geometry(map_stop_OSM[map_stop_OSM$region =="Île-de-France",]),pch=".",lwd=.1)
plot(st_geometry(map_stop_OSM[map_stop_OSM$dist_SNCF>=200 & map_stop_OSM$region =="Île-de-France",]),pch=".",lwd=.2,col="red",add=T)
plot(st_geometry(gares_SNCF[gares_SNCF$dist_OSM>=200 & gares_SNCF$region =="Île-de-France",]),pch=".",lwd=.2,col="green",add=T)

#AJouter les stations de RER RATP 
#Retirer les stations 
  fread("OpenDataTransport/referentiel-gares-voyageurs.csv")
gares_sncf[,c("lon","lat")] <- as.data.frame(lapply(do.call(rbind,str_split(gares_sncf$`WGS 84`,',')),as.numeric))


list_files <- c("Corse","IDFM","Intercites","Provence", "TGV" ,"TER")          
list_files <- c("Corse","IDFM","Intercites","Provence", "TGV" ,"TER")   





#Retrait des gares à l'étranger 
region <- readOGR(paste0("IGN/",path_map_ign,"/REGION.shp"))
region <- region[!region$NOM_REG %in% c("Guadeloupe","Martinique","Mayotte","La Réunion","Guyane"),]
region <- st_as_sf(region)

map_stops$region <- as.numeric(st_intersects(map_stops,region))
map_stops$region <- region$NOM_REG[map_stops$region ]
map_stops <- map_stops[!is.na(map_stops$region),]

mean(duplicated(map_stops[map_stops$source %in% c("Transilien","TER","IDFM"),]$stop_id))

map_stops[map_stops$source %in% "Transilien",]$stop_id[!map_stops[map_stops$source %in% "Transilien",]$stop_id %in% unique(map_stops[map_stops$source != "Transilien",]$stop_id)]
map_stops[map_stops$stop_id == "471039",]
map_stops[map_stops$stop_name == "Melun",]

#En Ile-de-France : les deux lignes de tram de la SNCF 
distance_OSM_OpenData <- st_distance(map_stops,map_stop_OSM)
map_stops$min_dist_OSM <- apply(distance_OSM_OpenData,1,min)
map_stop_OSM$min_dist_OpenData <- apply(distance_OSM_OpenData,2,min)

plot(st_geometry(map_stop_OSM),pch=".",lwd=.1)
plot(st_geometry(map_stop_OSM[map_stop_OSM$min_dist_OpenData>=200,]),pch=".",lwd=.2,col="red",add=T)

quantile(map_stops$min_dist_OSM,probs=90:100/100)
plot(st_geometry(map_stops),pch=".",lwd=.1)
plot(st_geometry(map_stops[map_stops$min_dist_OSM>=200,]),pch=".",lwd=.2,col="red",add=T)

map_stops[map_stops$min_dist_OSM>=200 & map_stops$region == "Île-de-France",]$stop_name
