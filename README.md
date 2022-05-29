# com_transport
Indicateurs communaux d'accessibililité aux transports en commun à partir : 
- de données en open data sur l'emplacement des stations de transport en commun sur les sites https://transport.data.gouv.fr/ et https://ressources.data.sncf.com/pages/accueil/
- de l'API de calcul d'isochrones piétons et routiers de l'IGN https://geoservices.ign.fr/documentation/services/api-et-services-ogc/isochrones
- des données carroyées (200m) de population de l'Insee https://www.insee.fr/fr/statistiques/4176290?sommaire=4176305

## Fichiers générés 
Le dossier Data_final contient l'ensemble des données finales utiles. Les dossiers Iso, Open_data, IGN contiennent des fichiers intermédiaires et peuvent être détruis à l'issue des  

### Gares et stations des réseaux férrés français 

Le fichier shapefile station_gare_opendata.shp recense l'ensemble des stations et gares des réseaux ferrés en France métropolitaine (hors tramways frontaliers de Genève et Bâle) et contient les variables suivantes : 
- stop_name : nom de l'arrêt
- source : source des données (SNCF, TGV, réseau urbain ou réseau ferroviaire secondaire) 
- stop_id : identifiant de l'arrêt 
- type : type de réseau (tram, métro ou train) 
- TGV : indicatrice de desserte par le TGV (0 ou 1, uniquement pour les gares ferroviaires) 

### Isochrones piétons et routiers des gares et stations 

### Indicateurs communaux d'accessibilité aux transports en commun

### Cartes
Des cartes permettant de contrôler la qualité des données produites sont générées : 
- 

## Programmes
Les indicateurs sont consitués avec trois programmes successifs : 
- 1_opendatarail.R télécharge les données des réseaux de transports collectif urbain et les gares desservies par le TGV sur le site transport.data.gouv.fr (format GTFS) ansi que la liste sur les gares voyageurs du réseau SNCF (format csv), les harmonise et les convertit au format cartographique shapefile. 
- 2_ 
