library("data.table")
library("rjson")
library("httr")
library("rgdal")
library("raster")
library("fasterize")
library("sf")
library("dplyr")
library("curl")
library("archive")

#library("gitcreds")
#gitcreds_set()


options(scipen=15)
#method <- "time"|"distance"
#Unités <- secondes (?) ou mètres
#mode <- "car"|"pedestrian"
mode <- 'pedestrian'
method='distance'
coords <- c(2.347820932099209,48.82976328107956)
duradist <- 1000
car_depart <- "N3081800E3831800"


#Carreaux -> coordonnées
from_car_to_gps <- function(car){
  car <- unlist(car)
  coord_car <- lapply(car,function(c) c(x=as.numeric(substr(c,10,16))+100,y =as.numeric(substr(c,2,8))+100))
  coord_car <- lapply(coord_car,st_point)
  coord_gps <- st_transform(st_sfc(coord_car,crs= 3035),crs = 4326)
  coord_gps <- round(st_coordinates(coord_gps),digits=5)
  colnames(coord_gps) <- c("lon","lat")
  if(nrow(coord_gps) == 1){
    coord_gps <- as.numeric(coord_gps)
    names(coord_gps) <- c("lon","lat")}
  return(coord_gps)
} 

#Coordonnée -> carreau
from_gps_to_car<- function(coord){
  coord_pt <- split(coord,1:(length(unlist(coord))/2))
  coord_pt <- lapply(coord_pt,as.numeric)
  coord_pt <- lapply(coord_pt,st_point)
  coord_laea <- st_transform(st_sfc(coord_pt,crs= 4326),crs = 3035)
  coord_laea <- as.data.frame(st_coordinates(coord_laea))
  coord_laea$car <- paste0("N",floor(coord_laea$Y/200)*200,"E",floor(coord_laea$X/200)*200)
  return(coord_laea$car)
} 

#Récupérer les carreaux accessibles depuis un point
get_iso_car <- function(coord_depart,duradist,mode,method){
  coord_depart <- as.numeric(coord_depart)
  coord_depart <- round(coord_depart,digits=5)
  car_depart <- from_gps_to_car(coord_depart)
    
  url_api <- paste0("https://itineraire.ign.fr/simple/1.0.0/isochrone?resource=bdtopo-pgr&profile=",
                    mode,"&costType=",method,"&costValue=",duradist,"&direction=departure&point=",
                    coord_depart[1],",",coord_depart[2],"&constraints=&geometryFormat=geojson")
  
  resp <- GET(url_api, encoding = "UTF-8")
  isochrone_json <- fromJSON(content(resp, "text", encoding = "UTF-8"))
  if(!is.null(isochrone_json$geometry)){
    isochrone_sf <- st_sfc(st_polygon(lapply(isochrone_json$geometry$coordinate,function(x) do.call(rbind,x))), crs = 4326)
    isochrone_sf <- st_sf(geom=isochrone_sf,status="OK")
    isochrone_sf <-st_buffer( st_transform(isochrone_sf,crs=3035),dist = 100) 
    bbox_iso <- floor(st_bbox(isochrone_sf)/200)*200 + c(0,0,200,200) 
    raster_iso <- raster(xmn=bbox_iso["xmin"], xmx=bbox_iso["xmax"], ymn=bbox_iso["ymin"], ymx=bbox_iso["ymax"],crs = 3035,res=200)
    raster_iso <- fasterize(isochrone_sf,raster_iso)
    car_iso <- as.data.frame(raster_iso)
    car_iso[,c("x","y")] <- coordinates(raster_iso)
    car_iso <- car_iso[!is.na(car_iso$layer),c("x","y")]
    if(nrow(car_iso)>0){
      car_iso$car <- paste0("N",car_iso$y-100,"E",car_iso$x-100)
      #Carreau de départ
      car_iso$car_dep <- car_depart
      car_iso <- subset(car_iso,select = -c(x,y))
    }else{car_iso <- data.frame(car_dep=car_depart,car=NA)}
  }else{car_iso <- data.frame(car_dep=car_depart,car=NA)}
  
  car_iso$lon_dep <- coord_depart[1]
  car_iso$lat_dep <- coord_depart[2]
  
  return(car_iso)
}

#get_iso_car(c(2.347820932099209,48.82976328107956),duradist=1000,method='distance',mode='pedestrian')

test <- get_iso_car(c(2.347820932099209,48.82976328107956),duradist=1000,method='distance',mode='pedestrian')
test <- get_iso_car(c(2.347820932099209,48.82976328107956),duradist=600,method='time',mode='pedestrian')
test <- get_iso_car(c(2.347820932099209,48.82976328107956),duradist=600,method='time',mode='car')



#Récupérer une liste de carreaux depuis un point (en entrée : une table avec deux colonnes)
iso_data_coord <- function(data_coord,duradist,mode="pedestrian",method="distance",dirout,file){
  #Mise en forme de la table
  colnames(data_coord) <- c("lon_dep","lat_dep")
  data_coord <- as.data.table(data_coord[,1:2])
  data_coord <- round(data_coord,digits = 5)
    
  #Sélection des lignes matchées 
  if(file.exists(paste0(dirout,"/",file,".csv.gz"))){
    deja_charge <- fread(paste0(dirout,"/",file,".csv.gz"))
    #Décompression du fichier
    fwrite(deja_charge,paste0(dirout,"/",file,".csv"))
    dep_deja_charge <- deja_charge[!duplicated(deja_charge[,c("lon_dep","lat_dep")]),]
    data_coord <- full_join(data_coord,dep_deja_charge,by = c("lon_dep","lat_dep"))
    data_coord <- data_coord[is.na(car_dep),c("lon_dep","lat_dep")]
  }
  
  print(paste("Nombre d'isochrones/isodistances à charger :",nrow(data_coord)))
  
  c <- 1
  while(c<=nrow(data_coord)){
    print(c)
    try({iso_car <- get_iso_car(coord_depart = data_coord[c+1,],duradist=duradist,mode=mode,method=method)
    write.table(iso_car,paste0(dirout,"/",file,".csv"),row.names = F, append=T,sep=",",
                col.names=!file.exists(paste0(dirout,"/",file,".csv")))})
    c <- c + 1
    if(c %%100 == 0){print(paste(c,"/",nrow(data_coord)))}
  }
  
  #Compression
  deja_charge <- fread(paste0(dirout,"/",file,".csv"))
  fwrite(deja_charge,paste0(dirout,"/",file,".csv.gz"))
  file.remove(paste0(dirout,"/",file,".csv"))
  
}
  
#Réécriture du programme avec des données OpenData (SNCF et transport.data.gouv.fr)
map_station_gare <- st_read("Rail/station_gare_opendata.shp")
map_station_gare[,c("lon","lat")] <- st_coordinates(map_station_gare)
map_station_gare$lon <- round(map_station_gare$lon,digits = 5)
map_station_gare$lat <- round(map_station_gare$lat,digits = 5)

coords_station_gare <- st_drop_geometry(map_station_gare[,c("lon","lat")])
coords_station_gare <- coords_station_gare[!duplicated(coords_station_gare),]
  
#Ne pas hésiter à faire tourner deux fois si certaines requêtes ne sont pas passées 
iso_data_coord(coords_station_gare,duradist=600,mode="pedestrian",method="time",dirout="Iso",file="isodist_gares_pieton_10min")
iso_data_coord(coords_station_gare,duradist=600,mode="car",method="time",dirout="Iso",file="isodist_gares_voiture_10min")

iso_data_coord(coords_station_gare,duradist=1200,mode="pedestrian",method="time",dirout="Iso",file="isodist_gares_pieton_20min")

#Mise en forme des distances pieton 
car_dist_pieton <- fread(paste0("Iso/isodist_gares_pieton_10min.csv.gz"))
car_dist_pieton <- car_dist_pieton[car != "NNaNENaN",]
car_dist_pieton <- left_join(car_dist_pieton,plyr::rename(st_drop_geometry(map_station_gare),c("lon"="lon_dep","lat"="lat_dep")))
car_dist_pieton <- subset(car_dist_pieton,select=c(type,car))
car_dist_pieton <- car_dist_pieton[!duplicated(car_dist_pieton),]
#car_dist_pieton <- dcast(data=car_dist_pieton,car~paste0('10min_pieton_',type),fun.aggregate = length)
car_dist_pieton$type <- paste0("pieton_",car_dist_pieton$type)

car_dist_voiture <- fread(paste0("Iso/isodist_gares_voiture_10min.csv.gz"))
car_dist_voiture <- car_dist_voiture[car != "NNaNENaN",]
car_dist_voiture <- left_join(car_dist_voiture,plyr::rename(st_drop_geometry(map_station_gare),c("lon"="lon_dep","lat"="lat_dep")))
car_dist_voiture <- subset(car_dist_voiture,select=c(type,car))
car_dist_voiture <- car_dist_voiture[!duplicated(car_dist_voiture),]
car_dist_voiture$type <- paste0("voiture_",car_dist_voiture$type)

car_dist_pieton_voiture <- rbind(car_dist_pieton,car_dist_voiture)
car_dist_pieton_voiture <- dcast(data=car_dist_pieton_voiture,car~paste0('10min_',type),fun.aggregate = length)
fwrite(car_dist_pieton_voiture,"Iso/isodist_gares_voiture_pieton_10min.csv.gz")

#car_dist_pieton <- fread(paste0("Iso/isodist_gares_pieton_10min.csv.gz"))
#car_dist <- car_dist[car != "NNaNENaN",]
#car_dist <- left_join(car_dist,plyr::rename(st_drop_geometry(map_station_gare),c("lon"="lon_dep","lat"="lat_dep")))

#car_dist_voiture <- fread(paste0("Iso/isodist_gares_voiture_10min.csv.gz"))

# 
# #Carreaux des stations
# map_stop <- st_read("Rail/map_stop.shp")
# st_crs(map_stop) <- 3035
# map_stop <- st_transform(map_stop,crs=4326)
# map_stop$car <- from_gps_to_car(st_coordinates(map_stop))
# map_stop[,c("lon","lat")] <- st_coordinates(map_stop)
# 
# plot(st_geometry(map_stop),pch=".")
# #Etape 1 : géolocalisation par carreau
# car_stop <- unique(map_stop$car)
# coord_car_stop <- from_car_to_gps(car_stop)
# 
# #Chargement des isochrones pour différentes distances (à partir des carreaux)
# iso_data_coord(coord_car_stop,duradist=1000,mode="pedestrian",method="distance",dirout="Iso",file="isodist_gares_pieton_1000m")
# iso_data_coord(coord_car_stop,duradist=1500,mode="pedestrian",method="distance",dirout="Iso",file="isodist_gares_pieton_1500m")
# 
# #Chargement des isochrones pour différentes distances (à partir des coordonnées exactes)
# iso_data_coord(st_coordinates(map_stop),duradist=500,mode="pedestrian",method="distance",dirout="Iso",file="isodist_xy_gares_pieton_500m")
# iso_data_coord(st_coordinates(map_stop),duradist=1000,mode="pedestrian",method="distance",dirout="Iso",file="isodist_xy_gares_pieton_1000m")
# iso_data_coord(st_coordinates(map_stop),duradist=1500,mode="pedestrian",method="distance",dirout="Iso",file="isodist_xy_gares_pieton_1500m")
# 
# #Nombre carreaux accessibles (agrégation des résultats )
# dist_car_xy <- lapply(c(500,1000,1500),function(dist){
#   car_dist <- fread(paste0("Iso/isodist_xy_gares_pieton_",dist,"m.csv.gz"))
#   car_dist <- car_dist[!duplicated(car_dist),]
#   car_dist$dist <- dist
#   return(car_dist)
# })
# dist_car_xy <- rbindlist(dist_car_xy)
# dist_car_xy <- subset(dist_car_xy, car != "NNaNENaN" & car_dep != "", select=-c(lon_dep,lat_dep))
# dist_car_xy <- dcast(data=dist_car_xy,car+car_dep~1,value.var = "dist", fun.aggregate =min)
# dist_car_xy <- plyr::rename(dist_car_xy,c("."="dist_car_xy"))
# 
# #Isochrone à partir du carreau de la station
# dist_car <- lapply(c(500,1000,1500),function(dist){
#   car_dist <- fread(paste0("Iso/isodist_gares_pieton_",dist,"m.csv.gz"))
#   car_dist <- car_dist[!duplicated(car_dist),]
#   car_dist$dist <- dist
#   return(car_dist)
# })
# dist_car <- rbindlist(dist_car)
# dist_car <- subset(dist_car, car != "NNaNENaN" & car_dep != "", select=-c(lon_dep,lat_dep))
# dist_car <- dcast(data=dist_car,car+car_dep~1,value.var = "dist", fun.aggregate =min)
# dist_car <- plyr::rename(dist_car,c("."="dist_car"))
# 
# #Réunion des deux méthodes
# dist_car_full <- full_join(dist_car,dist_car_xy)
# dist_car_full[is.na(dist_car_full)] <- 9999
# dist_car_full$dist <- with(dist_car_full,pmin(dist_car,dist_car_xy))
# 
# #Distance des carreaux à chaque moyen de transport 
# stop_route <- inner_join(st_drop_geometry(map_stop)[,c("id_stop","car")],
#                          fread("Rail/stop_route.csv",colClasses = "character")[,c("id_stop","id_route")])
# stop_route <- inner_join(stop_route,fread("Rail/data_route.csv")[,c("id_route","route")])
# stop_route <- stop_route[,c("car","route")]
# stop_route <- stop_route[!duplicated(stop_route),]
#   
# stop_route_iso <- inner_join(plyr::rename(stop_route,c("car"="car_dep")),dist_car_full[,c("car_dep","car","dist")])
# 
# fwrite(stop_route_iso,"Iso/stop_route_iso.csv.gz")

#Comparaison des deux méthodes 
# count_car <- full_join(plyr::rename(plyr::count(dist_car_full[,c("car_dep","dist_car")]),c("dist_car"="dist")),
#                        plyr::rename(plyr::count(dist_car_full[,c("car_dep","dist_xy")]),c("dist_xy"="dist","freq"="freq_xy")))
# count_car <- count_car[count_car$dist != 9999,]
# count_car[is.na(count_car)] <- 0
# aggregate(data=count_car,cbind(freq,freq_xy)~dist,FUN= function(x) c(mean=mean(x),quantile(x,probs=0:10/10)))
# 
# setDT(count_car)
# count_car[,freq_dec := ceiling(rank(freq)*10/.N) ,by="dist"]
# count_car[,freq_xy_dec := ceiling(rank(freq_xy)*10/.N) ,by="dist"]
# aggregate(data=count_car,cbind('diff'=freq-freq_xy)~dist,FUN= function(x) c(mean=mean(x),quantile(x,probs=0:10/10)))
# aggregate(data=count_car,cbind('d'=freq-freq_xy)~freq_dec+dist,FUN= function(x) round(c(quantile(x,probs=0:10/10))))

