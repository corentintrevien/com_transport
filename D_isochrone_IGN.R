library("data.table")
library("rjson")
library("httr")
library("sf")

#method <- "time"|"distance"
#mode <- "car"|"pedestrian"
mode <- 'pedestrian'
method='distance'
coords <- c(2.347820932099209,48.82976328107956)
duradist <- 1000

get_iso_poly <- function(coords,duradist,mode,method){
  coords <- as.numeric(coords)
  
  #Carreau de départ
  dep_car <- floor(st_coordinates(st_transform(st_sfc(st_point(coords),crs=4326),crs = 3035))/200)*200
  dep_car <- paste0("N",dep_car[,"Y"],"E",dep_car[,"X"])
  
  
  url_api <- paste0("https://itineraire.ign.fr/simple/1.0.0/isochrone?resource=bdtopo-pgr&profile=",
                    mode,"&costType=",method,"&costValue=",duradist,"&direction=departure&point=",
                    coords[1],",",coords[2],"&constraints=&geometryFormat=geojson")
  
  resp <- GET(url_api, encoding = "UTF-8")
  isochrone_json <- fromJSON(content(resp, "text", encoding = "UTF-8"))
  if(!is.null(isochrone_json$geometry)){
    isochrone_sf <- st_sfc(st_polygon(lapply(isochrone_json$geometry$coordinate,function(x) do.call(rbind,x))), crs = 4326)
    isochrone_sf <- st_sf(geom=isochrone_sf,status="OK")
    isochrone_sf <- st_transform(isochrone_sf,crs=3035)
    bbox_iso <- floor(st_bbox(isochrone_sf)/200)*200 + c(0,0,200,200) 
    raster_iso <- raster(xmn=bbox_iso["xmin"], xmx=bbox_iso["xmax"], ymn=bbox_iso["ymin"], ymx=bbox_iso["ymax"],crs = 3035,res=200)
    raster_iso <- fasterize(isochrone_sf,raster_iso)
    car_iso <- as.data.frame(raster_iso)
    car_iso[,c("x","y")] <- coordinates(raster_iso)
    car_iso <- car_iso[!is.na(car_iso$layer),c("x","y")]
    car_iso$car <- paste0("N",car_iso$y-100,"E",car_iso$x-100)
    #Carreau de départ
    car_iso$dep_car <- dep_car
    car_iso <- subset(car_iso,select = -c(x,y))
  }else{car_iso <- data.frame(dep_car=dep_car,car=NA)}
  return(car_iso)
}

#Carreaux des stations
map_stop <- st_read("Rail/map_stop.shp")
st_crs(map_stop) <- 3035
map_stop <- st_transform(map_stop,crs=4326)
map_stop$car <- paste0("N",with(map_stop,paste(floor(y/200)*200,floor(x/200)*200,sep = "E")))
car_stop <- unique(map_stop$car)

#Coordonnées WGS84 du centre du carreau 
unique_car_stop <- data.frame(car= car_stop,y =as.numeric(substr(car_stop,2,8)),x=as.numeric(substr(car_stop,10,16)))
unique_car_stop <- st_as_sf(unique_car_stop,coords=c("x","y"),crs=3035)
unique_car_stop <- st_transform(unique_car_stop,crs=4326)
unique_car_stop[,c("lon","lat")] <- st_coordinates(unique_car_stop)
unique_car_stop <- st_drop_geometry(unique_car_stop)

#Chargement des isochrones
get_iso_poly(c(unique_car_stop[1,]$lon,unique_car_stop[1,]$lat),1000,method='distance',mode='pedestrian')

get_iso_poly(c(2.347820932099209,48.82976328107956),1000,method='distance',mode='pedestrian')

