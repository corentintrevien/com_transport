library("curl")
library("dplyr")
library("zip")
library("data.table")
library("plyr")
library("purrr")
library("archive")
library("rmapshaper")

#install.packages("dplyr",dependencies=TRUE)
source('0_functions.R')
#Téléchargement des carreaux de population Insee 
if(!file.exists(paste0("Insee/Filosofi2017_carreaux_200m_csv.zip"))){
  dir.create("Insee")
  #curl_download("https://www.insee.fr/fr/statistiques/fichier/4176290/Filosofi2015_carreaux_200m_csv.zip",
  #              "Insee/Filosofi2015_carreaux_200m_csv.zip", mode="wb")
  curl_download("https://www.insee.fr/fr/statistiques/fichier/6215138/Filosofi2017_carreaux_200m_csv.zip",
                "Insee/Filosofi2017_carreaux_200m_csv.zip", mode="wb")
}

#Décompression des carreaux de population Insee
unzip("Insee/Filosofi2017_carreaux_200m_csv.zip",files="Filosofi2017_carreaux_200m_csv.7z",exdir="Insee")

#Chargement des carreaux de population 
archive_extract("Insee/Filosofi2017_carreaux_200m_csv.7z",dir="Insee")
car_filosofi_metro <- fread("Insee/Filosofi2017_carreaux_200m_met.csv")
car_filosofi_metro <- car_filosofi_metro
colnames(car_filosofi_metro) <- tolower(colnames(car_filosofi_metro))
car_filosofi_metro <- subset(car_filosofi_metro,select=c("lcog_geo","ind","men","idcar_200m"))
file.remove("Insee/Filosofi2017_carreaux_200m_met.csv")

#Carreaux à cheval sur plusieurs communes 
car_filosofi_metro[,ncom := nchar(lcog_geo)/5]
max(car_filosofi_metro$ncom)
#Répartition des individus et des ménages 
car_filosofi_metro[,ind := ind/ncom]
car_filosofi_metro[,men := men/ncom]
#Extraction des différents codes communes
car_filosofi_metro[,codgeo1 := substr(lcog_geo,1,5)]
car_filosofi_metro[ncom>=2,codgeo2 := substr(lcog_geo,6,10)]
car_filosofi_metro[ncom>=3,codgeo3 := substr(lcog_geo,11,15)]
car_filosofi_metro[ncom>=4,codgeo4 := substr(lcog_geo,16,20)]
car_filosofi_metro[ncom>=5,codgeo5 := substr(lcog_geo,21,25)]

#Une ligne par commune
car_filosofi_metro <- melt(data= car_filosofi_metro, idvars= c("ind","men","idcar_200m"), measure.vars = paste0("codgeo",1:5))
car_filosofi_metro <- subset(car_filosofi_metro,!is.na(value),select=-variable)
car_filosofi_metro <- plyr::rename(car_filosofi_metro,c("value"="depcom"))

#Calcul des population communales : ménages, ménages pauvres et population totale par commune 
com_filosofi <- data.table::dcast(data=car_filosofi_metro,depcom~ 1,value.var = c("ind","men"),fun.aggregate = sum)

#Chargement carreaux transports
car_dist_pieton_voiture <- fread("Data_final/isochone_gares_voiture_pieton.csv.gz")
car_dist_pieton_voiture <- car_dist_pieton_voiture[type != "",]
car_dist_pieton_voiture$idcar_200m <- paste0("CRS3035RES200m",car_dist_pieton_voiture$car)
#Agrégation des données Tram et Métro (temps minimal d'acc§s)
car_dist_pieton_voiture[type=="tram","type"] <- "tram_metro"
car_dist_pieton_voiture[type=="metro","type"] <- "tram_metro"
car_dist_pieton_voiture <- dcast(data=car_dist_pieton_voiture,type+idcar_200m+mode~1,value.var = "minute",fun.aggregate = min)
colnames(car_dist_pieton_voiture)[4] <- "minute"

#Fusion des données Population/transport
car_dist_trans_pop <- inner_join(car_dist_pieton_voiture,car_filosofi_metro, by = "idcar_200m")
car_dist_trans_pop <- dcast(data=car_dist_trans_pop,depcom~paste0(minute,"min")+mode+type,value.var=c("ind","men"),fun.aggregate=sum)

#Stat par commune
car_dist_trans_pop <- left_join(com_filosofi,car_dist_trans_pop,by="depcom")
car_dist_trans_pop[is.na(car_dist_trans_pop)] <- 0

#Ajout des stats globales Paris Lyon Marseille
car_dist_trans_pop_plm <- car_dist_trans_pop[depcom %in% c(13201:13216,69381:69389,75101:75120),]
car_dist_trans_pop_plm[depcom %in% 13201:13216,"depcom"] <- 13055
car_dist_trans_pop_plm[depcom %in% 69381:69389,"depcom"] <- 69123
car_dist_trans_pop_plm[depcom %in% 75101:75120,"depcom"] <- 75056
car_dist_trans_pop_plm <- dcast(data=car_dist_trans_pop_plm,depcom~1,
                                value.var= colnames(car_dist_trans_pop_plm)[colnames(car_dist_trans_pop_plm) != "depcom"],
                                fun.aggregate = sum)
car_dist_trans_pop <- rbind(car_dist_trans_pop_plm,car_dist_trans_pop)

#Pourcentages de la population concernées
indic_ind <- colnames(car_dist_trans_pop)[substr(colnames(car_dist_trans_pop),1,4)=="ind_"] 
indic_men <- colnames(car_dist_trans_pop)[substr(colnames(car_dist_trans_pop),1,4)=="men_"] 

car_dist_trans_pop[,indic_ind] <- lapply(indic_ind,function(v) round(ifelse(car_dist_trans_pop[[v]]>0, car_dist_trans_pop[[v]]/car_dist_trans_pop$ind,0),3))
car_dist_trans_pop[,indic_men] <- lapply(indic_men,function(v) round(ifelse(car_dist_trans_pop[[v]]>0, car_dist_trans_pop[[v]]/car_dist_trans_pop$men,0),3))

#Enregistrement des données
fwrite(car_dist_trans_pop,"Data_final/trans_com.csv.gz")

#Carte de visualisation de l'accessibilité communale 
download_ign_admin_express("COMMUNE")
commune_map <- st_read(paste0("IGN/",path_map_ign,"/COMMUNE.shp"))
#Carte d'accessibilité par commune
pdf("Map/Carte_accessibilite_france.pdf")
  commune_map <- left_join(commune_map,
                           car_dist_trans_pop[,c("depcom","ind_10min_pieton_tram_metro","ind_10min_pieton_train",
                                                 "ind_10min_voiture_tram_metro","ind_10min_voiture_train")],
                           by=c("INSEE_COM"="depcom"))
  commune_map$ind_10min_pieton_tram_metro <- commune_map$ind_10min_pieton_tram_metro*100
  commune_map$ind_10min_pieton_train <- commune_map$ind_10min_pieton_train*100
  commune_map$ind_10min_voiture_tram_metro <- commune_map$ind_10min_voiture_tram_metro*100
  commune_map$ind_10min_voiture_train <- commune_map$ind_10min_voiture_train*100

  commune_map <- split(commune_map,commune_map$INSEE_REG)
  
  for(reg in names(commune_map)){
    print(reg)
    com_reg <- commune_map[[reg]]
    print("Simplification")
    com_reg <-  ms_simplify(com_reg,keep_shapes=TRUE,keep = 0.001)
    com_reg <- plyr::rename(com_reg, c("ind_10min_pieton_tram_metro"=">10 min à pied/Tram ou métro",
                                               "ind_10min_pieton_train"=">10 min à pied/Gare ferroviaire",
                                               "ind_10min_voiture_tram_metro"=">10 min en voiture/Tram ou métro",
                                               "ind_10min_voiture_train"=">10 min en voiture/Gare ferroviaire"))
    print("Carte")
    plot(subset(com_reg,select=c(">10 min à pied/Tram ou métro",">10 min à pied/Gare ferroviaire",
                            ">10 min en voiture/Tram ou métro",">10 min en voiture/Gare ferroviaire")),
                key.pos	=1,border =NA)
  }
dev.off()
