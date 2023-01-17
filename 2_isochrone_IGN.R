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



iso_data_coord <- function(data_coord,duradist,mode="pedestrian",method="distance",dirout,file){
  #Mise en forme de la table
  colnames(data_coord) <- c("lon_dep","lat_dep")
  data_coord <- as.data.table(data_coord[,1:2])
  data_coord <- round(data_coord,digits = 5)
  print(paste("Nombre total d'isochrones/isodistances :",nrow(data_coord)))
  
  #Sélection des observations déjà traitées et enregistrées
  if(file.exists(paste0(dirout,"/",file,".csv.gz"))){
    deja_charge <- fread(paste0(dirout,"/",file,".csv.gz"))
    #Sélection des isochrones non-chargés
    deja_charge <- subset(deja_charge,!is.na(car),select=c("lon_dep","lat_dep")) 
    deja_charge <- round(deja_charge,digits = 5)
    deja_charge <- subset(deja_charge,!duplicated(deja_charge))
    print(paste("Nombre d'isochrones/isodistances déjà chargés :",nrow(deja_charge)))
    data_coord <- anti_join(data_coord,deja_charge,by = c("lon_dep","lat_dep"))
  }
  
  print(paste("Nombre d'isochrones/isodistances à charger :",nrow(data_coord)))
  #Ajout des observations non matchées au fichiers d'isochrones
  #t <- 0
  for(t in 0:(nrow(data_coord)%/% 100)){
    first_iso <- t*100+1
    last_iso <- min(t*100+100,nrow(data_coord))
    print(paste(first_iso,"/",nrow(data_coord)))
    #Chargement de la tranche de 100 isochrones
    iso_car <- lapply(first_iso:last_iso,function(i){ try({get_iso_car(coord_depart = data_coord[i,],duradist=duradist,mode=mode,method=method)})})
    iso_car <- rbindlist(iso_car)

    #Enregistrement 
    if(file.exists(paste0(dirout,"/",file,".csv.gz"))){
      deja_charge <- fread(paste0(dirout,"/",file,".csv.gz"))
      deja_charge <- subset(deja_charge,!is.na(car),select=c("car_dep","car","lon_dep","lat_dep"))
      deja_charge <- rbindlist(list(deja_charge,iso_car))
      fwrite(deja_charge,paste0(dirout,"/",file,".csv.gz"))
    }else{
      fwrite(iso_car,paste0(dirout,"/",file,".csv.gz"))
      }

  }
}
  
#Chargement des données cartographiques des stations
map_station_gare <- st_read("Data_final/station_gare_opendata.shp")
map_station_gare[,c("lon","lat")] <- st_coordinates(map_station_gare)
map_station_gare$lon <- round(map_station_gare$lon,digits = 5)
map_station_gare$lat <- round(map_station_gare$lat,digits = 5)

#Suppression des doublons (points où se trouvent plusieurs stations)
coords_station_gare <- st_drop_geometry(map_station_gare[,c("lon","lat")])
coords_station_gare <- coords_station_gare[!duplicated(coords_station_gare),]
  
#Ne pas hésiter à faire tourner deux fois si certaines requêtes ne sont pas passées 
#Récupération des isochrones selon 3 modalités : 10 min à pied, 20 min à pied, 10 min en voiture
iso_data_coord(coords_station_gare,duradist=600,mode="pedestrian",method="time",dirout="Iso",file="isodist_gares_pieton_10min")
iso_data_coord(coords_station_gare,duradist=600,mode="car",method="time",dirout="Iso",file="isodist_gares_voiture_10min")
iso_data_coord(coords_station_gare,duradist=1200,mode="pedestrian",method="time",dirout="Iso",file="isodist_gares_pieton_20min")

###MISE EN FORME DES DONNES 
#TYPE DE RESEAU SELON LE CARREAU DE DEPART
map_station_gare$car_dep <- from_gps_to_car(st_drop_geometry(map_station_gare)[,c("lon","lat")])
car_station_gare <- st_drop_geometry(map_station_gare)[,c("type","car_dep")]
car_station_gare <- car_station_gare[!duplicated(car_station_gare),]

#Mise en forme des distances selon le mode et la distance pieton (10 min)
car_dist_pieton <- fread(paste0("Iso/isodist_gares_pieton_10min.csv.gz"))
car_dist_pieton <- car_dist_pieton[!is.na(car),]
car_dist_pieton <- left_join(car_dist_pieton,car_station_gare, by = "car_dep")
car_dist_pieton <- subset(car_dist_pieton,select=c(type,car))
car_dist_pieton <- car_dist_pieton[!duplicated(car_dist_pieton),]
car_dist_pieton$mode <- "pieton"
car_dist_pieton$minute <- 10

#Mise en forme des distances pieton (20min)
car_dist_pieton20 <- fread(paste0("Iso/isodist_gares_pieton_20min.csv.gz"))
car_dist_pieton20 <- car_dist_pieton20[!is.na(car),]
car_dist_pieton20 <- left_join(car_dist_pieton20,car_station_gare, by = "car_dep")
car_dist_pieton20 <- subset(car_dist_pieton20,select=c(type,car))
car_dist_pieton20 <- car_dist_pieton20[!duplicated(car_dist_pieton20),]
car_dist_pieton20$mode <- "pieton"
car_dist_pieton20$minute <- 20

#Mise en forme des distances voiture (10min)
car_dist_voiture <- fread(paste0("Iso/isodist_gares_voiture_10min.csv.gz"))
car_dist_voiture <- car_dist_voiture[!is.na(car),]
car_dist_voiture <- left_join(car_dist_voiture,car_station_gare, by = "car_dep")
car_dist_voiture <- subset(car_dist_voiture,select=c(type,car))
car_dist_voiture <- car_dist_voiture[!duplicated(car_dist_voiture),]
car_dist_voiture$mode <- "voiture"
car_dist_voiture$minute <- 10

#Réunion de l'ensemble des données dans une table unique 
car_dist_pieton_voiture <- rbind(car_dist_pieton,car_dist_pieton20,car_dist_voiture)

fwrite(car_dist_pieton_voiture,"Data_final/isochone_gares_voiture_pieton.csv.gz")
