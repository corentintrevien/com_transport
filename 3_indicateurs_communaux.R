library("curl")
library("dplyr")
library("zip")
library("data.table")
library("plyr")
library("purrr")
library("archive")
#install.packages("dplyr",dependencies=TRUE)

#Téléchargement des carreaux de population Insee 
if(!file.exists(paste0("Insee/Filosofi2015_carreaux_200m_csv.zip"))){
  dir.create("Insee")
  curl_download("https://www.insee.fr/fr/statistiques/fichier/4176290/Filosofi2015_carreaux_200m_csv.zip",
                "Insee/Filosofi2015_carreaux_200m_csv.zip", mode="wb")
}

#Décompression des carreaux de population Insee
unzip("Insee/Filosofi2015_carreaux_200m_csv.zip",files="Filosofi2015_carreaux_200m_csv.7z",exdir="Insee")
archive_extract("Insee/Filosofi2015_carreaux_200m_csv.7z",dir="Insee")

#Chargement des carreaux de population 
car_filosofi_metro <- fread("Insee/Filosofi2015_carreaux_200m_metropole.csv")
car_filosofi_metro <- car_filosofi_metro
colnames(car_filosofi_metro) <- tolower(colnames(car_filosofi_metro))
car_filosofi_metro <- subset(car_filosofi_metro,select=c("depcom","ind","men","men_pauv","idinspire"))
file.remove("Insee/Filosofi2015_carreaux_200m_csv.7z")

#Calcul des population communales : ménages, ménages pauvres et population totale par commune 
com_filosofi <- data.table::dcast(data=car_filosofi_metro,depcom~ 1,value.var = c("ind","men","men_pauv"),fun.aggregate = sum)

#Chargement carreaux transports
car_dist_pieton_voiture <- fread("Data_final/isochone_gares_voiture_pieton.csv.gz")
car_dist_pieton_voiture <- car_dist_pieton_voiture[type != "",]
car_dist_pieton_voiture$idinspire <- paste0("CRS3035RES200m",car_dist_pieton_voiture$car)
#Agrégation des données Tram et Métro
car_dist_pieton_voiture[type=="tram","type"] <- "tram_metro"
car_dist_pieton_voiture[type=="metro","type"] <- "tram_metro"
car_dist_pieton_voiture <- dcast(data=car_dist_pieton_voiture,type+idinspire+mode~1,value.var = "minute",fun.aggregate = min)
colnames(car_dist_pieton_voiture)[4] <- "minute"

#Fusion des données Population/transport
car_dist_trans_pop <- inner_join(car_dist_pieton_voiture,car_filosofi_metro, by = "idinspire")
car_dist_trans_pop <- dcast(data=car_dist_trans_pop,depcom~paste0(minute,"min")+mode+type,value.var=c("ind","men","men_pauv"),fun.aggregate=sum)

#Stat par commune
car_dist_trans_pop <- left_join(com_filosofi,car_dist_trans_pop,"depcom",)
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
indic_men_pauv <- colnames(car_dist_trans_pop)[substr(colnames(car_dist_trans_pop),1,9)=="men_pauv_"] 

car_dist_trans_pop[,indic_ind] <- lapply(indic_ind,function(v) ifelse(car_dist_trans_pop[[v]]>0, car_dist_trans_pop[[v]]/car_dist_trans_pop$ind,0))
car_dist_trans_pop[,indic_men] <- lapply(indic_men,function(v) ifelse(car_dist_trans_pop[[v]]>0, car_dist_trans_pop[[v]]/car_dist_trans_pop$men,0))
car_dist_trans_pop[,indic_men_pauv] <- lapply(indic_men_pauv,function(v) ifelse(car_dist_trans_pop[[v]]>0, car_dist_trans_pop[[v]]/car_dist_trans_pop$men_pauv,0))

#Enregistrement des données
fwrite(car_dist_trans_pop,"Data_final/trans_com.csv")


