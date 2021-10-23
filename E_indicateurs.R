library("data.table")
library("sf")
library("plyr")
library("dplyr")

iso_gares_pieton_1000m <- fread("Iso/iso_gares_pieton_1000m.csv.gz")
iso_gares_pieton_1000m$dist  <- 1000
iso_gares_pieton_500m <- fread("Iso/iso_gares_pieton_500m.csv.gz")
iso_gares_pieton_500m$dist  <- 500
iso_gares_pieton_1500m <- fread("Iso/iso_gares_pieton_1500m.csv.gz")
iso_gares_pieton_1500m$dist  <- 1500
iso_gares_pieton <- rbindlist(list(iso_gares_pieton_500m,iso_gares_pieton_1000m,iso_gares_pieton_1500m))
#Carreaux des stations
map_stop <- st_read("Rail/map_stop.shp")
st_crs(map_stop) <- 3035
map_stop <- st_transform(map_stop,crs=4326)
map_stop$car <- paste0("N",with(map_stop,paste(floor(y/200)*200,floor(x/200)*200,sep = "E")))

#Stat d'accessibilité par station (vérification)
stat_station <- dcast(data=iso_gares_pieton,car_dep~paste0("car_dist_",dist),fun.aggregate = length)
map_stop <- left_join(map_stop,plyr::rename(stat_station,c("car_dep"="car")))
map_stop <- map_stop[order(map_stop$car_dist_500),]
quantile(map_stop,map_stop$)

 c(5.716888 45.19882)
  
car_depart <- 
get_iso_car(car_depart="N2463600E3984000",duradist= 1000,method='distance',mode='pedestrian')
get_iso_car(car_depart="N2463600E3984000",duradist= 1000,method='distance',mode='pedestrian')
get_iso_car(car_depart="N2463600E3984000",duradist= 1000,method='distance',mode='pedestrian')
get_iso_car("N2463600E3984000",1000,method='distance',mode='pedestrian')

 
lon_lat_depart <- c(x=as.numeric(substr(car_depart,10,16))+100,y =as.numeric(substr(car_depart,2,8))+100)
