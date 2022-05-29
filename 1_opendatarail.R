library(stringr)
library(plyr)
library(dplyr)
library(rjson)
library(curl)
library(httr)
library(data.table)
library(archive)
library(sf)
 
####TELECHARGEMENT DES DONNEES####
#Données Open_data.data.gouv.fr : tous les réseaux ferroviaires/tram/métro de France 
#(sauf tram de Genève et tram train Sarrebuck)

#Liste de URL de téléchargement des fichiers GTFS pour chaque réseau urbain
url_transport_data <- 
  c("Angers" = "https://transport-data-gouv-fr-resource-history-prod.cellar-c2.services.clever-cloud.com/b5873d02-043e-447a-8433-0a3be706efc8/b5873d02-043e-447a-8433-0a3be706efc8.20211220.120146.314683.zip",
    "Aubagne" = "https://transport-data-gouv-fr-resource-history-prod.cellar-c2.services.clever-cloud.com/cde22673-d7e4-4cbd-837f-3c8bd9fb2f9b/cde22673-d7e4-4cbd-837f-3c8bd9fb2f9b.20220501.180811.056290.zip",
    "Avignon" = "https://transport-data-gouv-fr-resource-history-prod.cellar-c2.services.clever-cloud.com/556b9c3d-ec50-406b-9c22-7d37e0f6a15b/556b9c3d-ec50-406b-9c22-7d37e0f6a15b.20220114.180639.087673.zip",
    "Besancon" = "https://transport-data-gouv-fr-resource-history-prod.cellar-c2.services.clever-cloud.com/e18e0aeb-8805-47fd-bcdb-c226d21c96fe/e18e0aeb-8805-47fd-bcdb-c226d21c96fe.20220209.120418.283731.zip",
    "Bordeaux" = "https://transport-data-gouv-fr-resource-history-prod.cellar-c2.services.clever-cloud.com/13e7e219-b037-4d60-a3ab-e55d2d3e5291/13e7e219-b037-4d60-a3ab-e55d2d3e5291.20211221.180336.077853.zip",
    "Brest" = "https://transport-data-gouv-fr-resource-history-prod.cellar-c2.services.clever-cloud.com/583d1419-058b-481b-b378-449cab744c82/583d1419-058b-481b-b378-449cab744c82.20211220.180421.613947.zip",
    "Caen" = "https://transport-data-gouv-fr-resource-history-prod.cellar-c2.services.clever-cloud.com/5468825d-1ab2-443a-af86-0f43b31a6bd9/5468825d-1ab2-443a-af86-0f43b31a6bd9.20220414.120433.774551.zip",
    "Clermont_Ferrand" = "https://transport-data-gouv-fr-resource-history-prod.cellar-c2.services.clever-cloud.com/cdb5cde9-a6f4-4805-a66b-1d983a1891d6/cdb5cde9-a6f4-4805-a66b-1d983a1891d6.20211202.145756.989589.zip",
    "Corse" = "https://transport-data-gouv-fr-resource-history-prod.cellar-c2.services.clever-cloud.com/c6f665c6-65d2-4975-ad46-401e600d9e37/c6f665c6-65d2-4975-ad46-401e600d9e37.20220127.181315.656496.zip",
    "Dijon" = "https://transport-data-gouv-fr-resource-history-prod.cellar-c2.services.clever-cloud.com/a87e1796-7f98-467e-91a4-eb0eae3a7478/a87e1796-7f98-467e-91a4-eb0eae3a7478.20211230.120132.499718.zip",
    "IDF" = "https://transport-data-gouv-fr-resource-history-prod.cellar-c2.services.clever-cloud.com/4c13f5c0-ed5f-4916-95fe-1a093e7a0c6a/4c13f5c0-ed5f-4916-95fe-1a093e7a0c6a.20220103.180325.483040.zip",
    "Le_Havre" = "https://transport-data-gouv-fr-resource-history-prod.cellar-c2.services.clever-cloud.com/d7cb95d9-63f5-4a29-a6eb-63dc543a5590/d7cb95d9-63f5-4a29-a6eb-63dc543a5590.20211220.180401.485856.zip",
    "Le_Mans" = "https://transport-data-gouv-fr-resource-history-prod.cellar-c2.services.clever-cloud.com/5339d96c-6d20-4a01-939a-40f7b56d6cc1/5339d96c-6d20-4a01-939a-40f7b56d6cc1.20220127.100656.627519.zip",
    "Lille" = "https://transport-data-gouv-fr-resource-history-prod.cellar-c2.services.clever-cloud.com/7e40b8fe-e390-4d88-a68d-ecb5e7ac9923/7e40b8fe-e390-4d88-a68d-ecb5e7ac9923.20220127.191240.904214.zip",
    "Lyon" = "https://transport-data-gouv-fr-resource-history-prod.cellar-c2.services.clever-cloud.com/a64b0837-b0ab-4bea-8c7a-a0531ae4d2e8/a64b0837-b0ab-4bea-8c7a-a0531ae4d2e8.20220308.060359.167369.zip",
    "Marseille" = "https://transport-data-gouv-fr-resource-history-prod.cellar-c2.services.clever-cloud.com/7eef6ec9-9ebb-44f2-becb-2efc522522d6/7eef6ec9-9ebb-44f2-becb-2efc522522d6.20211231.060328.167270.zip",
    "Montpellier" = "https://transport-data-gouv-fr-resource-history-prod.cellar-c2.services.clever-cloud.com/2ef043c8-3b10-4d87-af5f-65fead127407/2ef043c8-3b10-4d87-af5f-65fead127407.20211221.060728.800951.zip",
    "Nancy" = "https://transport-data-gouv-fr-resource-history-prod.cellar-c2.services.clever-cloud.com/e7e78cd7-e186-4923-a272-9713fbc28b45/e7e78cd7-e186-4923-a272-9713fbc28b45.20211209.180205.545845.zip",
    "Nantes" = "https://transport-data-gouv-fr-resource-history-prod.cellar-c2.services.clever-cloud.com/d2155136-23ba-449c-b98c-f756144c8d9a/d2155136-23ba-449c-b98c-f756144c8d9a.20211216.180254.134491.zip",
    "Nice" = "https://transport-data-gouv-fr-resource-history-prod.cellar-c2.services.clever-cloud.com/aacb4eea-d008-4b13-b17a-848b8ced7e03/aacb4eea-d008-4b13-b17a-848b8ced7e03.20220105.060337.933391.zip",
    "Orleans" = "https://chouette.enroute.mobi/api/v1/datas/keolis_orleans.gtfs.zip",
    "Provence" = "https://transport-data-gouv-fr-resource-history-prod.cellar-c2.services.clever-cloud.com/d86cc177-27b0-4376-9527-045262e58983/d86cc177-27b0-4376-9527-045262e58983.20220402.000137.611843.zip",
    "Reims" = "https://transport-data-gouv-fr-resource-history-prod.cellar-c2.services.clever-cloud.com/93c13187-03ec-4aa4-b968-ac6304ca9518/93c13187-03ec-4aa4-b968-ac6304ca9518.20211207.060010.550533.zip",
    "Rouen" = "https://transport-data-gouv-fr-resource-history-prod.cellar-c2.services.clever-cloud.com/c0ebcf01-954a-4d24-b2d8-a00333ffe937/c0ebcf01-954a-4d24-b2d8-a00333ffe937.20220207.102220.538183.zip",
    "Saint_Etienne" = "https://transport-data-gouv-fr-resource-history-prod.cellar-c2.services.clever-cloud.com/8b3e71e2-1155-4fb0-bf4a-cdddbb496e29/8b3e71e2-1155-4fb0-bf4a-cdddbb496e29.20211206.120134.651632.zip",
    "Strasbourg" = "https://transport-data-gouv-fr-resource-history-prod.cellar-c2.services.clever-cloud.com/eeea9e52-4f8a-459e-aef5-a093a3b05356/eeea9e52-4f8a-459e-aef5-a093a3b05356.20220207.095538.432678.zip",
    "Toulouse" = "https://transport-data-gouv-fr-resource-history-prod.cellar-c2.services.clever-cloud.com/f2c4212d-92f3-4706-95f7-a34183e92f3c/f2c4212d-92f3-4706-95f7-a34183e92f3c.20220207.100806.250187.zip",
    "Tours" = "https://transport-data-gouv-fr-resource-history-prod.cellar-c2.services.clever-cloud.com/e671de39-22d6-44bf-8168-e35b1d3c3b3b/e671de39-22d6-44bf-8168-e35b1d3c3b3b.20211207.060008.426378.zip",
    "Valenciennes"="https://transport-data-gouv-fr-resource-history-prod.cellar-c2.services.clever-cloud.com/05ff2a9e-2584-4991-aa4c-804489591c42/05ff2a9e-2584-4991-aa4c-804489591c42.20211221.180406.253165.zip",
    "TGV"="https://transport-data-gouv-fr-resource-history-prod.cellar-c2.services.clever-cloud.com/d9175b28-b4e4-4a19-8504-a4c02220b226/d9175b28-b4e4-4a19-8504-a4c02220b226.20220127.182909.526884.zip")

dir.create("Open_data",showWarnings = FALSE)

#Boucle de téléchargement et d'enregistrement de chaque fichier GTFS 
for(v in 1:length(url_transport_data)){
  print(names(url_transport_data)[v])
  try({
    #Création du dossier de données
    dir.create(paste0("Open_data/",names(url_transport_data)[v]),showWarnings = FALSE)
    #Téléchargement des données
    curl_download(paste0(url_transport_data[v]),paste0("Open_data/",names(url_transport_data)[v],"/gtfs.zip"), mode="wb")
    #Décompression des données
    unzip(paste0("Open_data/",names(url_transport_data)[v],"/gtfs.zip"),exdir=paste0("Open_data/",names(url_transport_data)[v]))
    #Suppression du fichier zipé 
    file.remove(paste0("Open_data/",names(url_transport_data)[v],"/gtfs.zip"))
  })
}

#Téléchargement des données OPEN DATA SNCF sur les gares voyageurs 
url_sncf <- "https://ressources.data.sncf.com/explore/dataset/referentiel-gares-voyageurs/download/?format=geojson&timezone=Europe/Berlin&lang=fr"
resp <- GET(url_sncf, encoding = "UTF-8")
gares_json <- fromJSON(content(resp, "text", encoding = "UTF-8"))

gares_data <- lapply(gares_json$features,function(g){ 
  gdata <- g$properties
  gdata <- gdata[names(gdata) %in% c("uic_code","code_gare","gare_agencegc_libelle","gare_regionsncf_libelle","gare_alias_libelle_noncontraint")]
  gdata <- as.data.frame(gdata)
  gdata[,c("stop_lon","stop_lat")] <-  g$geometry$coordinates
  gdata <- plyr::rename(gdata,c("uic_code"="stop_id","gare_alias_libelle_noncontraint"="stop_name",
                                "gare_agencegc_libelle"="agence_sncf",
                                "gare_regionsncf_libelle"="region_sncf"),
                        warn_missing = FALSE)
  return(gdata)})

#Enregistrement au format csv
gares_data <- rbind.fill(gares_data)
gares_data$stop_id <- as.character(as.numeric(gares_data$stop_id))
gares_data <- gares_data[!is.na(gares_data$stop_lon),]
dir.create("Open_data/SNCF",showWarnings = FALSE)
fwrite(gares_data,"Open_data/SNCF/Gares_opendata_sncf.csv.gz")

####FICHIER CATOGRAPHIQUE DES STATIONS####
#Carte des régions de France 
download_regions_ign <- function(){
  if(!file.exists(paste0("IGN/",path_map_ign,"/region.shp"))){
    dir.create("IGN",showWarnings = FALSE)
    curl_download("ftp://Admin_Express_ext:Dahnoh0eigheeFok@ftp3.ign.fr/ADMIN-EXPRESS-COG_2-1__SHP__FRA_WGS84G_2020-11-20.7z",
                  "IGN/ADMIN-EXPRESS-COG_2-1__SHP__FRA_WGS84G_2020-11-20.7z", mode="wb")
    archive_extract("IGN/ADMIN-EXPRESS-COG_2-1__SHP__FRA_WGS84G_2020-11-20.7z",dir="IGN",
                    files = paste0(path_map_ign,"/REGION.",c("shp","shx","dbf","prj","cpg")))
    file.remove("IGN/ADMIN-EXPRESS-COG_2-1__SHP__FRA_WGS84G_2020-11-20.7z")
  }}
path_map_ign <- "ADMIN-EXPRESS-COG_2-1__SHP__FRA_2020-11-20/ADMIN-EXPRESS-COG/1_DONNEES_LIVRAISON_2020-11-20/ADE-COG_2-1_SHP_WGS84G_FRA"
#download_regions_ign()

#Mise en forme des réseaux urbains 
list_reseaux <- names(url_transport_data)

import_stops <- lapply(list_reseaux,function(f){
  print(f)
  #Liaisons
  routes <- fread(paste0("Open_data/",f,"/routes.txt"))
  #Correction des données (retraitement de certains classements erronés ou discutables)
  if(f == "Provence"){routes$route_type <- 2} 
  if(f == "Bordeaux"){
    routes <- routes[route_short_name %in% c("A","B","C","D"),]
    routes$route_type <- 0} 
  if(f == "Clermont_Ferrand"){
    routes <- routes[route_short_name %in% c("A"),]
    routes$route_type <- 0} 
  if(f == "Tours"){
    routes <- routes[route_short_name %in% c("Tram A"),]
    routes$route_type <- 0} 
  if(f == "Saint_Etienne"){routes <- routes[route_short_name %in% c("T1","T2","T3"),]} 
  if(f == "Caen"){routes <- routes[routes$route_type == 0,]} 
  if(f == "Rouen"){routes <- routes[routes$route_type != 0,]} 
  if(f %in% c("Rouen",'Reims','Aubagne')){routes[route_type == 1,"route_type"] <- 0}
  #Sélection des liaisons ferrovaires 
  routes <- routes[route_type %in% 0:2,]
  #Trajets
  trips <- fread(paste0("Open_data/",f,"/trips.txt"))
  #Sélection des trajets ferrovaires 
  trips <- right_join(trips,routes[,c("route_id","route_type")])
  #Horaires
  stop_times <- fread(paste0("Open_data/",f,"/stop_times.txt"),colClasses = c("stop_id"="character"))
  #Sélection des stations desservies par un train
  stop_times <- right_join(stop_times,trips[,c("trip_id","route_type")])
  stop_times <- stop_times[,c("stop_id","route_type")]
  stop_times <- stop_times[!duplicated(stop_times),]
  #Stations
  stops <- fread(paste0("Open_data/",f,"/stops.txt"),colClasses = c("stop_id"="character"),encoding = "UTF-8")
  #Stations ferroviaires
  stops <- right_join(stops,stop_times)
  if(f %in% c("TER","Intercites","TGV")){stops$stop_id <- unlist(lapply(strsplit(stops$stop_id,"-"),function(s) s[[length(s)]]))}
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

#sélection des stations localisées en France
path_map_ign <- "ADMIN-EXPRESS-COG_2-1__SHP__FRA_2020-11-20/ADMIN-EXPRESS-COG/1_DONNEES_LIVRAISON_2020-11-20/ADE-COG_2-1_SHP_WGS84G_FRA"
region <- st_read(paste0("IGN/",path_map_ign,"/REGION.shp"))
region <- region[!region$NOM_REG %in% c("Guadeloupe","Martinique","Mayotte","La Réunion","Guyane"),]
map_stops$region <- as.numeric(st_intersects(map_stops,region))
map_stops$region <- region$NOM_REG[map_stops$region]
map_stops <- map_stops[!is.na(map_stops$region),]

#Vérification catographique des données
dir.create("Map",showWarnings = FALSE)
pdf("Map/Carte_station_reseau.pdf")
for(s in list_reseaux){
  print(s)
  plot(st_geometry(map_stops[map_stops$source==s,]),pch = ".",col="white",main=s)
  plot(st_geometry(map_stops[map_stops$source==s & map_stops$route_type ==0,]),pch = 19,col="blue",add=T)
  plot(st_geometry(map_stops[map_stops$source==s & map_stops$route_type ==1,]),pch = 19,col="green",add=T)
  plot(st_geometry(map_stops[map_stops$source==s & map_stops$route_type ==2,]),pch = 19,col="red",add=T)
}
dev.off()

#Chargement des gares voyageur open DATA SNCF 
gares_sncf <- fread("Open_data/SNCF/Gares_opendata_sncf.csv.gz")
gares_sncf <- gares_sncf[!is.na(gares_sncf$stop_lon) & !is.na(gares_sncf$stop_lat),]
gares_sncf <- st_as_sf(gares_sncf, coords = c("stop_lon", "stop_lat"), crs = 4326, agr = "constant")

#Sélection des stations en France
gares_sncf$region <- as.numeric(st_intersects(gares_sncf,region))
gares_sncf$region <- region$NOM_REG[gares_sncf$region]
gares_sncf <- gares_sncf[!is.na(gares_sncf$region),]

#Tag des gares grande ligne 
gares_sncf$TGV <- as.numeric(gares_sncf$stop_id %in% map_stops[map_stops$source == "TGV",]$stop_id)

#Fusion avec suppression des doublons en Île-de-France et dans les gares TGV entre les différentes sources de données
#mean(map_stops[map_stops$source == "TGV",]$stop_id %in% gares_sncf[gares_sncf$TGV==1,]$stop_id)

map_station_gare <- rbind(subset(gares_sncf[!((gares_sncf$TGV==0 & gares_sncf$agence_sncf=="DGIF" & gares_sncf$region=="Île-de-France") | gares_sncf$source == "TGV"),],
                                 select=-c(code_gare,agence_sncf,region_sncf)),
                          map_stops[!(map_stops$source=="IDF" & map_stops$region!="Île-de-France"),])

#Cartes de vérification
pdf("Map/Carte_station_france.pdf")
  plot(st_geometry( map_station_gare[map_station_gare$route_type ==0,]),pch=".",main="Tramway")
  plot(st_geometry( map_station_gare[map_station_gare$route_type ==1,]),pch=".",main="Métro")
  plot(st_geometry( map_station_gare[map_station_gare$route_type ==2,]),pch=".",main="Train")
  plot(st_geometry( map_station_gare[map_station_gare$TGV ==1,]),pch=".",main="TGV")
dev.off()

#Type de réseau
map_station_gare$type <-ifelse(map_station_gare$route_type==0,"tram","")
map_station_gare$type <-ifelse(map_station_gare$route_type==1,"metro",map_station_gare$type )
map_station_gare$type <-ifelse(map_station_gare$route_type==2,"train",map_station_gare$type)

#Enregistrement des données finales sur les stations ferrées 
dir.create("Data_final")
st_write(map_station_gare,"Data_final/station_gare_opendata.shp",delete_dsn=T)