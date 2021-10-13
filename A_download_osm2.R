library(curl)
library(osmdata)
library(osmar)
library(xml2)
library(reshape)
library(rgdal)
library(rgeos)
library(sp)
library(sf)
library(stringi)
library(stringr)
library(raster)
library(data.table)
library(zip)
library(archive)
library(fasterize)
library(RPostgreSQL)
options(timeout=1800)

#set_overpass_url("https://lz4.overpass-api.de/api/interpreter")
#set_overpass_url("https://z.overpass-api.de/api/interpreter")
set_overpass_url("https://overpass.kumi.systems/api/interpreter")
get_overpass_url()

source("Z_functions.R")

#Liste des régions
list_regions <- c("Bretagne","Hauts-de-France","Pays-de-la-Loire","Normandie","Île-de-France","Grand-Est",
                  "Bourgogne-Franche-Comté","Nouvelle-Aquitaine","Occitanie","Provence-Alpes-Côte-d'Azur",
                  "Auvergne-Rhône-Alpes","Corse","Centre-Val-de-Loire")

#Création des dossier de téléchargement des données OSM
dir.create("Rail")
dir.create("Rail/download")

#Fonction de téléchargement des données brutes OSM de transport ferré pour une région 
download_osm_rail_region <- function(region){
  #region <- "Corse"
  print(region)
  plain_region <- plain_str(region)

  #Objets descendant railway  
  file_railway_des <- paste("Rail/download/railway_des_",plain_region,".osm",sep="")
  if(!file.exists(file_railway_des)){
    print("Railway_des")
    try({
      query <- opq(region, timeout = 900,memsize = 1073741824)
      query <- add_osm_feature (query, key="railway", value = c('tram','subway','rail','light_rail',"abandoned",
                                                                "construction","disused","preserved","halt",
                                                                "stop_position","platform","station","tram_stop"))
      query <- osmdata_xml (query)
      write_xml(query,file_railway_des)
    })
  }
 
  #Objets ascendant railway  
  file_railway_asc <- paste("Rail/download/railway_asc_",plain_region,".osm",sep="")
  if(!file.exists(file_railway_asc)){
    print("Railway_asc")
    try({
      query <- opq (region, timeout = 900,memsize = 1073741824)
      query <- add_osm_feature (query, key="railway", value = c('tram','subway','rail','light_rail',"abandoned",
                                                                "construction","disused","preserved","halt",
                                                                "stop_position","platform","station","tram_stop"))
      query$suffix <- ");\n(._;<<;);\nout body;"
      query <- osmdata_xml (query)
      write_xml(query,file_railway_asc)
    })
  }
      
  #Objets public transport 
  file_public <- paste("Rail/download/public_",plain_region,".osm",sep="")
  if(!file.exists(file_public)){
    print("Public")
    try({
      query <- opq (region, timeout = 900,memsize = 1073741824)
      query <- add_osm_feature (query, key="public_transport", value = c('stop_area','station','group_stop_area'))
      query <- osmdata_xml (query)
      write_xml(query,file_public)
    })
  }
  
  #Objets route descendant 
  file_route_des <- paste("Rail/download/route_des_",plain_region,".osm",sep="")
  if(!file.exists(file_route_des)){
    print("Route_des")
    try({
      query <- opq (region, timeout = 900,memsize = 1073741824)
      query <- add_osm_feature (query, key="route", value = c('tram','subway','rail','light_rail',"train"))
      query <- osmdata_xml (query)
      write_xml(query,file_route_des)
    })
  }
  
  #Objets route ascendant 
  file_route_asc <- paste("Rail/download/route_asc_",plain_region,".osm",sep="")
  if(!file.exists(file_route_asc)){
    print("Route_asc")
    try({
      query <- opq (region, timeout = 900,memsize = 1073741824)
      query <- add_osm_feature (query, key="route", value = c('tram','subway','rail','light_rail',"train"))
      query$suffix <- ");\n(._;<<;);\nout body;"
      query <- osmdata_xml (query)
      write_xml(query,file_route_asc)
    })
  }

} 

#A lancer plusieurs fois si certains téléchargements on échoué 
for(region in list_regions){download_osm_rail_region(region)}

#Compression des fichiers 
if(!file.exists("Rail/download/OMS.zip")){
  list_download <- list.files('Rail/download') 
  zip("Rail/download/OSM.zip",paste0("Rail/download/",list_download), mode ="cherry-pick")
  file.remove(paste0("Rail/download/",list_download))
}



