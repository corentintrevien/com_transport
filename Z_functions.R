library(curl)
library(osmdata)
library(osmar)
library(xml2)
library(reshape)
library(rgdal)
library(rgeos)
library(raster)
library(fasterize)
library(sp)
library(sf)
library(dplyr)
library(plyr)
library(Rfast)
library(stringr)
library(stringi)
library(maptools)
#http://www.?pnvkarte.de
options(scipen = 15)

subset_osm_table <- function(osm_table,id_nodes=c(),id_ways=c(),id_relations=c()){
  osm_table$nodes$coords <- osm_table$nodes$coords[osm_table$nodes$coords[,"id"] %in% id_nodes,]
  osm_table$nodes$tags <- osm_table$nodes$tags[osm_table$nodes$tags[,"id"] %in% id_nodes,]
  osm_table$ways$refs <- osm_table$ways$refs[osm_table$ways$refs[,"id"] %in% id_ways,]
  osm_table$ways$tags <- osm_table$ways$tags[osm_table$ways$tags[,"id"] %in% id_ways,]
  osm_table$relations$refs <- osm_table$relations$refs[osm_table$relations$refs[,"id"] %in% id_relations,]
  osm_table$relations$tags <- osm_table$relations$tags[osm_table$relations$tags[,"id"] %in% id_relations,]
  return(osm_table)
}

#Fonction de suppréssion des accents 
plain_str <- function(str){
  plain <- str_replace_all(stri_trans_general(tolower(str),"Latin-ASCII"), "[:punct:]", "_") 
  return(plain)
}


osm_parse <- function(doc){
  
  parse_tags <- function(xml,keys){
    #xml = relation
    #keys <- c('railway','name','building','route')
    #keys <- paste("./tag[",paste(lapply(keys,function (x) paste("@k = '",x,"'",sep="")),collapse=" or "),"]",sep="")
    #tag <- lapply(xml, function(x)  do.call(rbind,lapply(getNodeSet(x,keys),xmlAttrs)))
    tag <- lapply(xml, function(x)  do.call(rbind,lapply(getNodeSet(x,"./tag"),xmlAttrs)))
    id_xml <- unlist(lapply(xml,function(x) xmlAttrs(x)[["id"]]))
    nb_tag <- unlist(lapply(tag, function(x){
      y = nrow(x)
      if(length(y)==0){y=0}
      return(y)
    }))
    id_tag <- unlist(lapply(1:length(tag),function(x) rep(id_xml[[x]],nb_tag[[x]])))
    tag <- cbind(id = id_tag,do.call(rbind,tag))
    return(tag)
  }
  
  #NODES
  nodes <- getNodeSet(doc,"//node")
  if(length(nodes)>0){
    nodes_coords <- lapply(nodes,xmlAttrs)
    nodes_coords <- lapply(nodes_coords,function(x) x[c("id","lat","lon")])
    nodes_coords <- do.call(rbind,nodes_coords) 
    #tags
    taged_nodes <- getNodeSet(doc, "//node[tag]")
    nodes_tags <- parse_tags(taged_nodes,c('railway','name','building','route'))
  }else{
    nodes_coords <- NULL
    nodes_tags <- NULL
  }
  
  #WAYS
  ways <- getNodeSet(doc,"//way")
  if(length(ways)>0){
    id_ways <- unlist(lapply(ways,xmlAttrs))
    #Ref des ways 
    ways_refs <- lapply(ways, function(x)  do.call(rbind,lapply(getNodeSet(x,"./nd"),xmlAttrs)))
    id_refs <- unlist(lapply(1:length(ways_refs),function(x) rep(id_ways[[x]],nrow(ways_refs[[x]]))))
    ways_refs <- cbind(id = id_refs,do.call(rbind,ways_refs))
    #Tags des ways 
    ways_tags <- parse_tags(ways,c('railway','name','building','route'))
  }else{
    ways_refs <- NULL
    ways_tags <- NULL
  }
  
  #RELATIONS
  relations <- getNodeSet(doc,"//relation")
  if(length(relations)>0){
    id_relations <- unlist(lapply(relations,xmlAttrs))
    #Ref des relations
    relations_refs <- lapply(relations, function(x)  do.call(rbind,lapply(getNodeSet(x,"./member"),xmlAttrs)))
    id_refs <- unlist(lapply(1:length(relations_refs),function(x) rep(id_relations[[x]],nrow(relations_refs[[x]]))))
    relations_refs <- cbind(id = id_refs,do.call(rbind,relations_refs))
    #Tags des relations 
    relations_tags <- parse_tags(relations,c('railway','name','building','route'))
  }else{
    relations_refs <- NULL
    relations_tags <- NULL
  }
  
  osmtables <- list(nodes = list(coords = nodes_coords, tags = nodes_tags),
                    ways = list(tags = ways_tags, refs = ways_refs),
                    relations = list(tags = relations_tags, refs = relations_refs))
  
  return(osmtables)
}


osm_table_fusion <- function(osm_list){
  osm_new <- osm_list[[1]]
  types_members <- list(c("nodes","coords"),c("nodes","tags"),c("ways","tags"),c("ways","refs"),
                        c("relations","tags"),c("relations","refs"))

  for (i in 2:length(osm_list)){
    #print(i)
    for(tm in types_members){
      type <- tm[1]
      member <- tm[2]
      #print(type)
      #print(member)
      if(is.null(osm_list[[i]][[type]][[member]]) == FALSE){
        osm_new[[type]][[member]] <- rbind(osm_new[[type]][[member]],
                                           osm_list[[i]][[type]][[member]][!(osm_list[[i]][[type]][[member]][,"id"] %in% unique(osm_new[[type]][[member]][,"id"])),])
      }
    }
  }
  return(osm_new)
}


read_oms_table <- function(path){
  osm_tab = list()
  types_members <- list(c("nodes","coords"),c("nodes","tags"),c("ways","tags"),c("ways","refs"),
                        c("relations","tags"),c("relations","refs"))
  for(tm in types_members){
    osm_tab[[tm[1]]][[tm[2]]] <- read.csv(paste(path,'_',tm[1],'_',tm[2],'.csv',sep=""),stringsAsFactors = FALSE)
  }
  return(osm_tab)
}

write_oms_table <- function(osm_tab,path){
  types_members <- list(c("nodes","coords"),c("nodes","tags"),c("ways","tags"),c("ways","refs"),
                        c("relations","tags"),c("relations","refs"))
  for(tm in types_members){
    type <- tm[1]
    member <- tm[2]
    write.csv(osm_tab[[type]][[member]],paste(path,'_',type,'_',member,'.csv',sep=""),row.names = FALSE)  
  }
}

#

path_map_ign <- "ADMIN-EXPRESS-COG_2-1__SHP__FRA_2020-11-20/ADMIN-EXPRESS-COG/1_DONNEES_LIVRAISON_2020-11-20/ADE-COG_2-1_SHP_WGS84G_FRA"

#Téléchargement des données IGN sur le site
download_regions_ign <- function(){
  
  if(!file.exists(paste0("IGN/",path_map_ign,"/region.shp"))){
    dir.create("IGN")
    curl_download("ftp://Admin_Express_ext:Dahnoh0eigheeFok@ftp3.ign.fr/ADMIN-EXPRESS-COG_2-1__SHP__FRA_WGS84G_2020-11-20.7z",
                  "IGN/ADMIN-EXPRESS-COG_2-1__SHP__FRA_WGS84G_2020-11-20.7z", mode="wb")
    archive_extract("IGN/ADMIN-EXPRESS-COG_2-1__SHP__FRA_WGS84G_2020-11-20.7z",dir="IGN",
                    files = paste0(path_map_ign,"/REGION.",c("shp","shx","dbf","prj","cpg")))
    file.remove("IGN/ADMIN-EXPRESS-COG_2-1__SHP__FRA_WGS84G_2020-11-20.7z")
    }
}
test <- function(){
}
test()


#Carreaux carte de france
make_squared_france <-function(){
  download_regions_ign()
  region <- readOGR(paste0("IGN/",path_map_ign,"/REGION.shp"))
  region <- region[!(region$NOM_REG %in% c("La Réunion","Martinique","Guadeloupe","Mayotte","Guyane")),]
  region <- spTransform(region, CRS("+init=epsg:3035"))
  region <- gSimplify(region, tol=100)
  france <- gBuffer(region, byid=FALSE, width=400)
  france <- gSimplify(france, tol=100)
  france <- gBuffer(france, byid=FALSE, width=0)
  bbox_france <- bbox(france)
  france <- spChFIDs(france, c("France"))
  france <- SpatialPolygonsDataFrame(france,data.frame(row.names = "France", france=c(1)))
  france_sf <- st_as_sf(france)
  #Identifiants carreaux
  raster_france <- raster(extent(floor(bbox_france/10000)*10000+c(0,0,10000,10000)), res=100)
  crs(raster_france) <- "+init=epsg:3035" 
  raster_france <- fasterize(france_sf,raster_france,"france")
  raster_france <- as.data.frame(raster_france,xy=TRUE)
  raster_france <- raster_france[!is.na(raster_france$layer),]
  #Recherche des carreaux frontaliers
  raster_france$id_square_100 <- (raster_france$x-50)*10000000+(raster_france$y-50)
  raster_france$id_square_1000 <- floor(raster_france$x/1000)*1000*10000000+ floor(raster_france$y/1000)*1000
  raster_france$id_square_10000 <- floor(raster_france$x/10000)*10000*10000000+ floor(raster_france$y/10000)*10000
  #Mise en forme
  count_square_10000 <- count(raster_france[,"id_square_10000"])
  france_10000 <- count_square_10000[count_square_10000$freq == 10000,]$x
  check_10000 <- count_square_10000[count_square_10000$freq < 10000,]$x
  count_square_1000 <- count(raster_france[raster_france$id_square_10000 %in% check_10000,"id_square_1000"])
  france_1000 <- count_square_1000[count_square_1000$freq == 100,]$x
  check_1000 <- count_square_1000[count_square_1000$freq < 100,]$x
  count_square_100 <- count(raster_france[raster_france$id_square_1000 %in% check_1000,"id_square_100"])
  france_100 <- count_square_100$x
  squared_france <- list(france_10000=france_10000,check_10000=check_10000,france_1000=france_1000,check_1000=check_1000,france_100=france_100)
  return(squared_france)
  }

#Vérification de la localisation en France
check_nodes_france <- function(osm_table){
  #Carte de France
  if(!exists("squared_france")){assign("squared_france",make_squared_france())}
  
  points <- data.frame(x = as.numeric(osm_table$nodes$coords[,"lon"]),y = as.numeric(osm_table$nodes$coords[,"lat"]))
  row.names(points) <- osm_table$nodes$coords[,'id']
  points <- SpatialPoints(points,proj4string = CRS("+init=EPSG:4326"))
  points <- spTransform(points, CRS("+init=epsg:3035"))
  points <- data.frame(x= points$x, y = points$y,
                       id_square_10000= floor(points$x/10000)*10000*10000000+ floor(points$y/10000)*10000,
                       id_square_1000= floor(points$x/1000)*1000*10000000+ floor(points$y/1000)*1000,
                       id_square_100= floor(points$x/100)*100*10000000+ floor(points$y/100)*100,
                       row.names = row.names(points))
  #Repérage des points en France
  points$france <- ifelse(points$id_square_100 %in% squared_france$france_100 | 
                            points$id_square_1000 %in% squared_france$france_1000 |
                            points$id_square_10000 %in% squared_france$france_10000,1,0)
  osm_table$nodes$coords <- cbind(osm_table$nodes$coords,as.matrix(points[,c("france","x","y")]))
  return(osm_table)
}

#Permet de sélectionner les objets OSM présents partiellement ou totalement en France
select_france <- function(osm_table){
  #osm_table <- osm_france
  if(!exists("squared_france")){assign("squared_france",make_squared_france())}
  
  #Sélection des nodes
  id_nodes_france <- osm_table$nodes$coords[osm_table$nodes$coords[,"france"]==1,"id"]
  #Sélection des ways
  count_ways <- count(osm_table$ways$refs[osm_table$ways$refs[,"ref"] %in% id_nodes_france ,"id"])
  id_ways_france <- count_ways[count_ways$freq>1,"x"]
  #S?lection des relations
  count_relations <- count(osm_table$relations$refs[osm_table$relations$refs[,"ref"] %in% id_nodes_france |
                                                      osm_table$relations$refs[,"ref"] %in% id_ways_france |
                                                      osm_table$relations$refs[,"type"] == "relation","id"])
  id_relations_france <- count_relations[count_relations$freq>1,"x"]
  #Nouvelles tables
  osm_table <- subset_osm_table(osm_table, id_nodes = id_nodes_france, id_ways = id_ways_france, id_relations = id_relations_france)
  osm_table$ways$refs <- osm_table$ways$refs[osm_table$ways$refs[,"ref"] %in% id_nodes_france,]
  osm_table$relations$refs <- osm_table$relations$refs[osm_table$relations$refs[,"ref"] %in% id_nodes_france |
                                                         osm_table$relations$refs[,"ref"] %in% id_ways_france,]
  
  return(osm_table)
}