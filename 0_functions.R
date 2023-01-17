#Fonction de conversion : Coordonnée WGS84 vers carreau Insee de rattachement 
from_gps_to_car<- function(coord){
  coord_pt <- split(coord,1:(length(unlist(coord))/2))
  coord_pt <- lapply(coord_pt,as.numeric)
  coord_pt <- lapply(coord_pt,st_point)
  coord_laea <- st_transform(st_sfc(coord_pt,crs= 4326),crs = 3035)
  coord_laea <- as.data.frame(st_coordinates(coord_laea))
  coord_laea$car <- paste0("N",floor(coord_laea$Y/200)*200,"E",floor(coord_laea$X/200)*200)
  return(coord_laea$car)
}

#Fonction permettant de récupérer les carreaux Insee accessibles depuis un point donné selon un mode et un temps donné
get_iso_car <- function(coord_depart,duradist,mode,method){
  coord_depart <- as.numeric(coord_depart)
  coord_depart <- round(coord_depart,digits=5)
  car_depart <- from_gps_to_car(coord_depart)
  
  url_api <- paste0("https://itineraire.ign.fr/simple/1.0.0/isochrone?resource=bdtopo-pgr&profile=",
                    mode,"&costType=",method,"&costValue=",duradist,"&direction=departure&point=",
                    coord_depart[1],",",coord_depart[2],"&constraints=&geometryFormat=geojson")
  
  resp <- GET(url_api, encoding = "UTF-8")
  isochrone_json <- fromJSON(content(resp, "text", encoding = "UTF-8"))
  
  if(!is.null(isochrone_json$geometry$type)){
    if(isochrone_json$geometry$type == "Polygon"){
      isochrone_sf <- st_sfc(st_polygon(lapply(isochrone_json$geometry$coordinates,function(x) do.call(rbind,x))), crs = 4326)
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
        car_iso <- subset(car_iso,select = c("car_dep","car"))
      }else{car_iso <- data.frame(car_dep=car_depart,car=NA)}
    }else{car_iso <- data.frame(car_dep=car_depart,car=NA)}
  }else{car_iso <- data.frame(car_dep=car_depart,car=NA)}
  
  car_iso$lon_dep <- coord_depart[1]
  car_iso$lat_dep <- coord_depart[2]
  
  return(car_iso)
}

#Carte des régions de France 
#layer<-"COMMUNE"
path_map_ign <- "ADMIN-EXPRESS-COG_3-0__SHP__FRA_2021-05-19/ADMIN-EXPRESS-COG/1_DONNEES_LIVRAISON_2021-05-19/ADECOG_3-0_SHP_LAMB93_FR"
download_ign_admin_express <- function(layer){
  if(!file.exists(paste0("IGN/",path_map_ign,"/region.shp"))){
    dir.create("IGN",showWarnings = FALSE)
    curl_download("http://files.opendatarchives.fr/professionnels.ign.fr/adminexpress/ADMIN-EXPRESS-COG_3-0__SHP__FRA_L93_2021-05-19.7z",
                  "IGN/ADMIN-EXPRESS-COG_3-0__SHP__FRA_L93_2021-05-19.7z", mode="wb")
    archive_extract("IGN/ADMIN-EXPRESS-COG_3-0__SHP__FRA_L93_2021-05-19.7z",dir="IGN",files = paste0(path_map_ign,"/",layer,".",c("shp","shx","dbf","prj","cpg")))
    #file.remove("IGN/ADMIN-EXPRESS-COG_3-0__SHP__FRA_L93_2021-05-19.7z")
  }}

