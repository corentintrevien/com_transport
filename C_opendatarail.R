library(stringr)
library(plyr)
library(rjson)
library(curl)
library(httr)

list.files("OpenDataTransport")
source("Z_functions.R")

#Données Transport.data.gouv.fr : tous les réseaux ferroviaires/tram/métro de France 
#(sauf tram de Genève et tram train Sarrebuck)

list_files <- list.files("OpenDataTransport")
list_files <- list_files[list_files != "SNCF"]
f <- "TER"

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
pdf("Verif.pdf")
for(s in list_files){
  print(s)
  plot(st_geometry(map_stops[map_stops$source==s,]),pch = ".",col="white",main=s)
  plot(st_geometry(map_stops[map_stops$source==s & map_stops$route_type ==0,]),pch = 19,col="blue",add=T)
  plot(st_geometry(map_stops[map_stops$source==s & map_stops$route_type ==1,]),pch = 19,col="green",add=T)
  plot(st_geometry(map_stops[map_stops$source==s & map_stops$route_type ==2,]),pch = 19,col="red",add=T)
}
dev.off()

########################
#Données SNCF open data#
########################

if(!file.exists("Rail/Gares_opendata_sncf.csv.gz")){
  url_sncf <- "https://ressources.data.sncf.com/explore/dataset/referentiel-gares-voyageurs/download/?format=geojson&timezone=Europe/Berlin&lang=fr"
  resp <- GET(url_sncf, encoding = "UTF-8")
  gares_json <- fromJSON(content(resp, "text", encoding = "UTF-8"))
  
  "PARIS LA VILLETTE EST PIERRE"
  
  gares_data <- lapply(gares_json$features,function(g){ 
    print(g$properties$uic_code)
    gdata <- g$properties
    gdata <- gdata[names(gdata) %in% c("uic_code","code_gare","gare_agencegc_libelle","gare_regionsncf_libelle",
                                       "gare_alias_libelle_noncontraint")]
    gdata <- as.data.frame(gdata)
    gdata[,c("stop_lon","stop_lat")] <-  g$geometry$coordinates
    gdata <- plyr::rename(gdata,c("uic_code"="stop_id","gare_alias_libelle_noncontraint"="stop_name",
                                  "gare_agencegc_libelle"="agence_sncf",
                                  "gare_regionsncf_libelle"="region_sncf"))
    return(gdata)})
  
  gares_data <- rbind.fill(gares_data)
  gares_data$stop_id <- as.character(as.numeric(gares_data$stop_id))
  gares_data <- gares_data[!is.na(gares_data$stop_lon),]
  fwrite(gares_data,"Rail/Gares_opendata_sncf.csv.gz")}

gares_data <- fread("Rail/Gares_opendata_sncf.csv.gz")
sncf_map <- st_as_sf(gares_data, coords = c("stop_lon", "stop_lat"), crs = 4326, agr = "constant")

#sélection des stations en France
sncf_map$region <- as.numeric(st_intersects(sncf_map,region))
sncf_map$region <- region$NOM_REG[sncf_map$region ]
sncf_map <- sncf_map[!is.na(sncf_map$region),]

#Manifestement, certains cars TER sont codés en TRAIN, on utilise donc les gares ferroviaires de l'open data SNCF
#Hors IDF pour ne pas faire doublon avec les données IDFM (plus complètes que celle de la SNCF)

map_stops <- map_stops[!map_stops$source %in% c("TER","Intercites","TGV","Transilien") ,]
sncf_map <- sncf_map[sncf_map$region != "Île-de-France" & sncf_map$agence_sncf != "Direction Générale des Gares Île-de-France" ,]
sncf_map$route_type <- 2
sncf_map$source <- "SNCF"

map_station_gare <- rbind(map_stops,subset(sncf_map,select=-c(agence_sncf,region_sncf,code_gare)))
#Cartes de vérification
pdf("Verif2.pdf")
  plot(st_geometry( map_station_gare[map_station_gare$route_type ==0,]),pch=".",main="Tramway")
  plot(st_geometry( map_station_gare[map_station_gare$route_type ==1,]),pch=".",main="Métro")
  plot(st_geometry( map_station_gare[map_station_gare$route_type ==2,]),pch=".",main="Train")
dev.off()

map_station_gare$type <-ifelse(map_station_gare$route_type==1,"Métro","")
map_station_gare$type <-ifelse(map_station_gare$route_type==0,"Tramway",map_station_gare$type)
map_station_gare$type <-ifelse(map_station_gare$route_type==2,"Train",map_station_gare$type)

st_write(map_station_gare,"Rail/station_gare_opendata.shp")
