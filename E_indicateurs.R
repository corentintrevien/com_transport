library("curl")
library("dplyr")
library("zip")
library("data.table")
library("plyr")
library("purrr")
library("archive")
#install.packages("dplyr",dependencies=TRUE)

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

#Indicateurs communaux
com_filosofi <- dcast(data=car_filosofi_metro ,depcom~ 1,value.var = c("pop","men","men_pauv"),fun.aggregate = sum)
  
#Carreaux transports
car_dist_pieton_voiture <- fread("Iso/isodist_gares_voiture_pieton_10min.csv.gz")
car_dist_pieton_voiture[,c("10min_pieton_tram_metro")] <- 
  pmax(car_dist_pieton_voiture$`10min_pieton_tram`,car_dist_pieton_voiture$`10min_pieton_metro`)
car_dist_pieton_voiture[,c("10min_voiture_tram_metro")] <- 
  pmax(car_dist_pieton_voiture$`10min_voiture_tram`,car_dist_pieton_voiture$`10min_voiture_metro`)

car_dist_pieton_voiture$idinspire <- paste0("CRS3035RES200m",car_dist_pieton_voiture$car)
car_dist_trans_pop <- inner_join(car_dist_pieton_voiture,
                                     car_filosofi_metro[,c("idinspire","depcom","pop","men","men_pauv")])

#Croisement carreaux Insee/transport
list_mode_station <- lapply(cross2(c("pieton","voiture"),c("tram","train","metro","tram_metro")),paste,collapse="_")
list_mode_station <- paste0("10min_",list_mode_station)
list_var_insee <- c("pop","men","men_pauv")
list_mode_station_var <- cross2(list_var_insee,list_mode_station)
list_mode_station_var <- lapply(list_mode_station_var,unlist)
  
for(v in list_mode_station_var){
  car_dist_trans_pop[[paste(v,collapse ='_')]] <- car_dist_trans_pop[[v[1]]]*car_dist_trans_pop[[v[2]]]
}
  
#Stat par commune
car_dist_trans_pop <- dcast(data=car_dist_trans_pop,depcom~1,
                            value.var= unlist(lapply(list_mode_station_var,paste,collapse ='_')),
                            fun.aggregate = sum)
  
car_dist_trans_pop <- full_join(car_dist_trans_pop,com_filosofi)
car_dist_trans_pop[is.na(car_dist_trans_pop)] <- 0
#Ajout des stats globales Paris Lyon Marseille
car_dist_trans_pop_plm <- car_dist_trans_pop[depcom %in% c(13201:13216,69381:69389,75101:75120),]
car_dist_trans_pop_plm[depcom %in% 13201:13216,"depcom"] <- 13055
car_dist_trans_pop_plm[depcom %in% 69381:69389,"depcom"] <- 69123
car_dist_trans_pop_plm[depcom %in% 75101:75120,"depcom"] <- 75056
car_dist_trans_pop_plm <- dcast(data=car_dist_trans_pop_plm,depcom~1,
                                value.var= c(list_var_insee,unlist(lapply(list_mode_station_var,paste,collapse ='_'))),
                                fun.aggregate = sum)
car_dist_trans_pop <- rbind(car_dist_trans_pop_plm,car_dist_trans_pop)
#Pourcentages
vi <- "pop"
vr <- "pop_10min_pieton_tram"
for(vi in list_var_insee){for(vt in list_mode_station){
  print(paste(vi,vt,sep="_"))
  car_dist_trans_pop[[paste(vi,vt,sep="_")]] <- 
    ifelse(car_dist_trans_pop[[paste(vi,vt,sep="_")]]>0,
           car_dist_trans_pop[[paste(vi,vt,sep="_")]]/car_dist_trans_pop[[vi]],0)
  print(quantile(car_dist_trans_pop[[paste(vi,vt,sep="_")]],probs=0:10/10))
}}

 
fwrite(subset(car_dist_trans_pop,select= c("depcom",unlist(lapply(list_mode_station_var,paste,collapse="_")))),
       "Iso/trans_com.csv")


