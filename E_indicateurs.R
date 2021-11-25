library("curl")
library("archive")
library("dplyr")
library("data.table")
library("plyr")
library("purrr")
<<<<<<< HEAD

#Téléchargement des carreaux Insee 

if(!file.exists(paste0("Insee/Filosofi2015_carreaux_200m_csv.zip"))){
  dir.create("Insee")
  curl_download("https://www.insee.fr/fr/statistiques/fichier/4176290/Filosofi2015_carreaux_200m_csv.zip",
                "Insee/Filosofi2015_carreaux_200m_csv.zip", mode="wb")
}

unzip("Insee/Filosofi2015_carreaux_200m_csv.zip",
      files="Filosofi2015_carreaux_200m_metropole_csv.7z",exdir="Insee")
archive_extract("Insee/Filosofi2015_carreaux_200m_metropole_csv.7z",dir="Insee")
car_filosofi_metro <- fread("Insee/Filosofi2015_carreaux_200m_metropole.csv")
colnames(car_filosofi_metro) <- tolower(colnames(car_filosofi_metro))
car_filosofi_metro <- plyr::rename(car_filosofi_metro,c("ind"='pop'))
file.remove("Insee/Filosofi2015_carreaux_200m_metropole_csv.7z")
file.remove("Insee/Filosofi2015_carreaux_200m_metropole.csv")

=======

#Téléchargement des carreaux Insee 

if(!file.exists(paste0("Insee/Filosofi2015_carreaux_200m_csv.zip"))){
  dir.create("Insee")
  curl_download("https://www.insee.fr/fr/statistiques/fichier/4176290/Filosofi2015_carreaux_200m_csv.zip",
                "Insee/Filosofi2015_carreaux_200m_csv.zip", mode="wb")
}

unzip("Insee/Filosofi2015_carreaux_200m_csv.zip",
      files="Filosofi2015_carreaux_200m_metropole_csv.7z",exdir="Insee")
archive_extract("Insee/Filosofi2015_carreaux_200m_metropole_csv.7z",dir="Insee")
car_filosofi_metro <- fread("Insee/Filosofi2015_carreaux_200m_metropole.csv")
colnames(car_filosofi_metro) <- tolower(colnames(car_filosofi_metro))
car_filosofi_metro <- plyr::rename(car_filosofi_metro,c("ind"='pop'))
file.remove("Insee/Filosofi2015_carreaux_200m_metropole_csv.7z")
file.remove("Insee/Filosofi2015_carreaux_200m_metropole.csv")

>>>>>>> 456c69d0219f94eecded5116c67c67499d462d8b
#Indicateurs communaux
com_filosofi <- dcast(data=car_filosofi_metro ,depcom~ 1,value.var = c("pop","men","men_pauv"),fun.aggregate = sum)
  
#Carreaux transports
stop_route_iso <- fread("Iso/stop_route_iso.csv.gz")
stop_route_iso$idinspire <- paste0("CRS3035RES200m",stop_route_iso$car)
stop_route_iso <- stop_route_iso[,c("route","idinspire","dist")]
stop_route_iso$route <- revalue(as.factor(stop_route_iso$route),c("subway"="metro","light_rail"="train"))
#Tram et métro réunis 
stop_route_iso2 <- stop_route_iso
stop_route_iso2$route <- revalue(as.factor(stop_route_iso2$route),c("metro"="metro_tram","tram"="metro_tram"))
stop_route_iso2 <- aggregate(data=stop_route_iso2,dist~route+idinspire,FUN=min)
#Tous modes rénunis 
stop_route_iso3 <- stop_route_iso2
stop_route_iso3$route <- revalue(as.factor(stop_route_iso3$route),c("metro_tram"="tcf","train"="tcf"))
stop_route_iso3 <- aggregate(data=stop_route_iso3,dist~route+idinspire,FUN=min)
#Réunion des trois tables
stop_route_iso <- rbind(stop_route_iso,stop_route_iso2,stop_route_iso3)
#Population des carreaux
stop_route_iso <- inner_join(stop_route_iso,car_filosofi_metro[,c("idinspire","depcom","pop","men","men_pauv")])
#Indicateurs communaux 
com_filosofi_trans <- dcast(data=stop_route_iso ,depcom~route+dist ,value.var = c("pop","men","men_pauv"),fun.aggregate = sum)
com_filosofi_trans <- full_join(com_filosofi_trans,com_filosofi)
com_filosofi_trans[is.na(com_filosofi_trans)] <- 0  
#Part de la population communale 
list_var <- lapply(cross2(c("train","tcf","tram","metro","metro_tram"),c("500","1000","1500")),paste,collapse="_")
for(v in list_var){
  print(v)
  com_filosofi_trans[[paste0("men_",v)]] <- round(com_filosofi_trans[[paste0("men_",v)]]/com_filosofi_trans[["men"]],3)
  com_filosofi_trans[[paste0("men_pauv_",v)]] <- round(com_filosofi_trans[[paste0("men_pauv_",v)]]/com_filosofi_trans[["men_pauv"]],3)
  com_filosofi_trans[[paste0("pop_",v)]] <- round(com_filosofi_trans[[paste0("pop_",v)]]/com_filosofi_trans[["pop"]],3)
}
#Enregistrement 
fwrite(com_filosofi_trans,"Rail/indicateurs_communaux.csv")
<<<<<<< HEAD
=======

>>>>>>> 456c69d0219f94eecded5116c67c67499d462d8b

