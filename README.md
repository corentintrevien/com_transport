# com_transport
Indicateurs communaux d'accessibililité aux transports en commun à partir : 
- de données en open data sur l'emplacement des stations de transport en commun sur les sites [transport.data.gouv.fr](https://transport.data.gouv.fr/) et [data.sncf.com](https://ressources.data.sncf.com/explore/dataset/referentiel-gares-voyageurs/)
- de l'API de calcul d'isochrones piétons et routiers de l'IGN (plus de détails [ici](https://geoservices.ign.fr/documentation/services/api-et-services-ogc/isochrones))
- des données carroyées (200m) de population de l'Insee (disponibles [ici](https://www.insee.fr/fr/statistiques/4176290))

## Fichiers générés 
Le dossier Data_final contient l'ensemble des données finales utiles. Les dossiers Iso, Open_data, IGN contiennent des fichiers intermédiaires et peuvent être détruits quand les trois programmes ont tourné. 

### Gares et stations des réseaux férrés français 

Le fichier shapefile station_gare_opendata.shp recense l'ensemble des stations et gares des réseaux ferrés en France métropolitaine (hors tramways frontaliers de Genève et Bâle) et contient les variables suivantes : 
- stop_name : nom de l'arrêt
- source : source des données (SNCF, TGV, réseau urbain ou réseau ferroviaire secondaire) 
- stop_id : identifiant de l'arrêt 
- type : type de réseau (tram, métro ou train) 
- TGV : indicatrice de desserte par le TGV (0 ou 1, uniquement pour les gares ferroviaires) 

### Isochrones piétons et routiers des gares et stations 

Le fichier isochone_gares_voiture_pieton.csv.gz recense l'ensemble des carreaux de 200m de France métropolitaine situés à moins de 10 minutes ou en voiture ou 20 minutes à pied d'une station ou gare ferroviaire. Il comprend les variables suivantes : 
- car : identifiant carreau défini selon la méthode de carroyage de l'Insee (plus de détail [ici](https://www.insee.fr/fr/statistiques/4176290))
- time : temps d'accès en minutes (10 ou 20)
- mode : mode de transport utilisé (voiture ou pieton)
- type : type de station (tram, métro ou train) 

### Indicateurs communaux d'accessibilité aux transports en commun

Le fichier trans_com.csv contient la part de la population d'une commune ayant accès aux transports en commun ferrés. Il comprend les variables suivantes : 
- codgeo : code commune 
- ind : nombre d'individus résidant dans la commune
- men : nombre de ménages résidant dans la commune 
- men_pauv : nombre de ménages pauvres résidant dans la commune 
- [pop]\_[time]\_[mode]\_[type] : part de la population [pop] (ind = individus, men = ménages, men_pauv = ménages pauvres) à moins de [time] (10min = 10 minutes, 20min = 20 minutes) en [mode] (voiture ou pieton) d'une station [type] (metro_tram = station de métro ou tram, train = gare ferroviaire). Part exemple : men_10min_pieton_metro_tram indique la part des ménages d'une commune à moins de 10 minutes à pied d'une station de métro ou de tram.

### Cartes

Des cartes permettant de contrôler la qualité des données produites sont générées et enregistrées dans dossier Map 

## Programmes
Les indicateurs sont consitués avec trois programmes successifs : 
- 1_opendatarail.R télécharge les données des réseaux de transports collectif urbain et les gares desservies par le TGV sur le site transport.data.gouv.fr (format GTFS) ansi que la liste sur les gares voyageurs du réseau SNCF (format json), les harmonise et les enregistre dans un format cartographique shapefile. 
- 2_isochrone_IGN.R détermine les isochrones de chaque station du réseau (10 ou 20 minutes à pied, 10 minutes en voiture) et les enregistre sous forme de données carroyées.
- 3_indicateurs_communaux apparie les isochrones carroyées avec la population (nombre d'individus, de ménages et de ménages pauvres) de chaque carreau, les données sont agrégées au niveau communal pour déterminer la part de la population ayant accès au réseau de transports en commun.
