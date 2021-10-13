library(rgdal)
library(raster)
library(fasterize)
library(plyr)
library(sf)
library(reshape)
plot <- raster::plot

map_stop <- readOGR("L:/Mobilité France peu dense/Donnees/Geo/TC/map_stop.shp")
map_track <- readOGR("L:/Mobilité France peu dense/Donnees/Geo/TC/map_track.shp")
data_route <- read.csv("L:/Mobilité France peu dense/Donnees/Geo/TC/data_route.csv",stringsAsFactors = FALSE)
stop_route <- read.csv("L:/Mobilité France peu dense/Donnees/Geo/TC/stop_route.csv",stringsAsFactors = FALSE)
track_route <- read.csv("L:/Mobilité France peu dense/Donnees/Geo/TC/track_route.csv",stringsAsFactors = FALSE)

raster_france <- raster(extent(3209600,4284600,2029200,3135400), res=200)
crs(raster_france) <- "+init=epsg:3035" 
raster_paris <- raster(extent(3741000,3781000,2871000,2911000), res=200)
crs(raster_paris) <- "+init=epsg:3035" 

###################
#Carreaux Communes#
###################

commune <- readOGR("L:/Mobilité France peu dense/Donnees/Geo/Admin_express_2017/COMMUNE.shp")
commune = spTransform(commune, CRS("+init=epsg:3035"))
commune@data <- rename(commune@data,c("INSEE_COM"="depcom"))
commune$depcom_num = ifelse(substr((commune$depcom),1,2) %in% c('2A','2B'),
                            20000+as.numeric(as.character(substr((commune$depcom),3,5))),
                            as.numeric(as.character(commune$depcom)))
raster_communes <- fasterize(st_as_sf(commune),raster_france,"depcom_num")
raster_communes_paris <- crop(raster_communes,raster_paris)

########################
#Carreaux Filosofi 2015#
########################
carreaux_pop <- read.csv("L:/Mobilité France peu dense/Donnees/Geo/Carreaux/Filosofi2015_carreaux_200m_metropole.csv")

#Calcul des coordonnees centrales 
carreaux_pop$x <- as.integer(substr(carreaux_pop$IdINSPIRE,24,30))+100
carreaux_pop$y <- as.integer(substr(carreaux_pop$IdINSPIRE,16,22))+100
carreaux_pop$z=ifelse(is.na(carreaux_pop$Ind),0,carreaux_pop$Ind)

#Conversion en raster
raster_pop <- subset(carreaux_pop,select=c(x,y,z))
raster_pop <- rasterFromXYZ(raster_pop)
raster_pop <- extend(raster_pop,extent(raster_france))
raster_pop[is.na(raster_pop)] <- 0 

raster_pop_paris <- crop(raster_pop,raster_paris)
  
plot(raster_pop)
##########
#Stations#
##########

route_type <- c("train","tram","subway","light_rail")
#route_type <- c("train","tram","subway")

station <- lapply(route_type,function(t) {
  stop <- map_stop@data[map_stop$id_stop %in% stop_route[stop_route$id_route %in% data_route[data_route$route == t,"id_route"],"id_stop"],c("x","y","id_stop")]
  stop$idINSPIRE <- paste("CRS3035RES200mN",as.character(as.integer(floor(stop$y/200)*200)),"E",as.character(as.integer(floor(stop$x/200)*200)),sep="")
  return(stop)
})

names(station) <-route_type

raster_stop <- lapply(station,function(stop){
  #stop = station[[2]]
  stop <- stop[!duplicated(stop[,"idINSPIRE"]),]
  stop$x <- as.integer(substr(stop$idINSPIRE,24,30))+100
  stop$y <- as.integer(substr(stop$idINSPIRE,16,22))+100
  stop$z <- 1
  stop <- stop[,c("x","y","z")]
  stop <- rasterFromXYZ(stop)
  #stop <- crop(stop,raster_france)
  #stop <- extend(stop,raster_france)
  stop <- crop(stop,raster_paris)
  stop <- extend(stop,raster_paris)
  return(stop)
})


plot(raster_stop[[2]])

#Carreaux dans un cercle de 800 mètre de rayon
square_circle = data.frame(x = rep(0:8,each=9)*200, y = rep(0:8,times=9)*200)
square_circle$dist <- sqrt((square_circle$x - 800)**2 + (square_circle$y - 800)**2)
square_circle$circle <- ifelse(square_circle$dist <= 800,1,0)
circle5 <- matrix(square_circle$circle,nc=9, nr=9)

#Carreaux à moins de 800 mètres d'une station
vois_raster_stop <- lapply(raster_stop,function(stop){
  #stop <- raster_stop[[1]]
  stop[is.na(stop)] <- 0
  stop <- focal(stop,w=circle5,FUN=any)
  stop <- stop >= 1
  #stop <- crop(stop,raster_france)
  stop <- crop(stop,raster_paris)
  return(stop)
})


#Population à moins de 800 mètres d'une station
raster_communes_tc <- raster_communes*10 + (vois_raster_stop[[1]] + vois_raster_stop[[2]] + vois_raster_stop[[3]] + vois_raster_stop[[4]] >= 1)
stat_communes_tc <- zonal(raster_pop, raster_communes_tc, 'sum')
stat_communes_tc <- data.frame(stat_communes_tc)
stat_communes_tc$depcom <- stat_communes_tc$zone %/% 10 
stat_communes_tc$tc <- stat_communes_tc$zone %% 10
stat_communes_tc <- cast(stat_communes_tc,depcom ~ tc, value = "sum")
#Population à moins de 800 mètres d'une station


raster_communes_paris_tc <- raster_communes_paris*10 + (vois_raster_stop[[1]] + vois_raster_stop[[2]] + vois_raster_stop[[3]] + vois_raster_stop[[4]] >= 1)
stat_communes_paris_tc <- zonal(raster_pop_paris, raster_communes_paris_tc, 'sum')
stat_communes_paris_tc <- data.frame(stat_communes_paris_tc)
stat_communes_paris_tc$depcom <- stat_communes_paris_tc$zone %/% 10 
stat_communes_paris_tc$tc <- stat_communes_paris_tc$zone %% 10
stat_communes_paris_tc <- cast(stat_communes_paris_tc,depcom ~ tc, value = "sum")


