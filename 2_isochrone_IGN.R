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

options(scipen=15)
#method <- "time"|"distance"
#Unités <- secondes ou mètres
#mode <- "car"|"pedestrian"
mode <- 'pedestrian'
method='distance'
coords <- c(2.347820932099209,48.82976328107956)
duradist <- 1000
car_depart <- "N3081800E3831800"

dir.create('Iso',showWarnings = FALSE)

#Fonction de conversion : Coordonnée -> carreau Insee de rattachement 
from_gps_to_car<- function(coord){
  coord_pt <- split(coord,1:(length(unlist(coord))/2))
  coord_pt <- lapply(coord_pt,as.numeric)
  coord_pt <- lapply(coord_pt,st_point)
  coord_laea <- st_transform(st_sfc(coord_pt,crs= 4326),crs = 3035)
  coord_laea <- as.data.frame(st_coordinates(coord_laea))
  coord_laea$car <- paste0("N",floor(coord_laea$Y/200)*200,"E",floor(coord_laea$X/200)*200)
  return(coord_laea$car)
} 

#Récupérer les carreaux Insee accessibles depuis un point donné selon un mode et un temps donné
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

#Renvoie une liste de carreaux depuis un point (en entrée : une table avec deux colonnes, longitude et latitude)
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
  #Ajout des lignes non matchées
  c <- 1
  while(c<=nrow(data_coord)){
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
  
#Chargement des données OpenData (SNCF et transport.data.gouv.fr)
map_station_gare <- st_read("Data_final/station_gare_opendata.shp")
map_station_gare[,c("lon","lat")] <- st_coordinates(map_station_gare)
map_station_gare$lon <- round(map_station_gare$lon,digits = 5)
map_station_gare$lat <- round(map_station_gare$lat,digits = 5)

coords_station_gare <- st_drop_geometry(map_station_gare[,c("lon","lat")])
coords_station_gare <- coords_station_gare[!duplicated(coords_station_gare),]
  
#Ne pas hésiter à faire tourner deux fois si certaines requêtes ne sont pas passées 
iso_data_coord(coords_station_gare,duradist=600,mode="pedestrian",method="time",dirout="Iso",file="isodist_gares_pieton_10min")
iso_data_coord(coords_station_gare,duradist=600,mode="car",method="time",dirout="Iso",file="isodist_gares_voiture_10min")
iso_data_coord(coords_station_gare,duradist=1200,mode="pedestrian",method="time",dirout="Iso",file="isodist_gares_pieton_20min")

#Mise en forme des distances pieton (10 min)
car_dist_pieton <- fread(paste0("Iso/isodist_gares_pieton_10min.csv.gz"))
car_dist_pieton <- car_dist_pieton[car != "NNaNENaN",]
car_dist_pieton <- left_join(car_dist_pieton,plyr::rename(st_drop_geometry(map_station_gare),c("lon"="lon_dep","lat"="lat_dep")))
car_dist_pieton <- subset(car_dist_pieton,select=c(type,car))
car_dist_pieton <- car_dist_pieton[!duplicated(car_dist_pieton),]
car_dist_pieton$type <- paste0("10min_pieton_",car_dist_pieton$type)

#Mise en forme des distances pieton (20min)
car_dist_pieton20 <- fread(paste0("Iso/isodist_gares_pieton_20min.csv.gz"))
car_dist_pieton20 <- car_dist_pieton20[car != "NNaNENaN",]
car_dist_pieton20 <- left_join(car_dist_pieton20,plyr::rename(st_drop_geometry(map_station_gare),c("lon"="lon_dep","lat"="lat_dep")))
car_dist_pieton20 <- subset(car_dist_pieton20,select=c(type,car))
car_dist_pieton20 <- car_dist_pieton20[!duplicated(car_dist_pieton20),]
car_dist_pieton20$type <- paste0("20min_pieton_",car_dist_pieton20$type)

#Mise en forme des distances voiture (10min)
car_dist_voiture <- fread(paste0("Iso/isodist_gares_voiture_10min.csv.gz"))
car_dist_voiture <- car_dist_voiture[car != "NNaNENaN",]
car_dist_voiture <- left_join(car_dist_voiture,plyr::rename(st_drop_geometry(map_station_gare),c("lon"="lon_dep","lat"="lat_dep")))
car_dist_voiture <- subset(car_dist_voiture,select=c(type,car))
car_dist_voiture <- car_dist_voiture[!duplicated(car_dist_voiture),]
car_dist_voiture$type <- paste0("10min_voiture_",car_dist_voiture$type)

car_dist_pieton_voiture <- rbind(car_dist_pieton,car_dist_pieton20,car_dist_voiture)

car_dist_pieton_voiture <- dcast(data=car_dist_pieton_voiture,car~type,fun.aggregate = length)
fwrite(car_dist_pieton_voiture,"Data_final/isochone_gares_voiture_pieton.csv.gz")
