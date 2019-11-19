##############################################
# Big Data - Traitement de Donnees de Masse
##############################################
#   Encadrants
#       Gilles Tredan - Roberto Pasqua
#   -----------
#   Etudiants
#       Thomas Bobillot
#       Raphael Perrochat
#       5 IR | SDBD B
#   -----------
#   INSA Toulouse
#   -----------
#   Datasets
#   
#   https://github.com/Bobillot/R_Project/blob/master/listings.csv   
#   https://github.com/Bobillot/R_Project/blob/master/crime.csv     
#   https://github.com/Bobillot/R_Project/blob/master/paris.csv
###############################################


### Packages requis

library(ggplot2)                  # creation de graphiques
library(plyr)                     # manipulation de donnees
library(leaflet)                  # creation de cartes
library(mapview)                  # exportation de carte en png
library(dplyr)                    # traitement conditionnel 

crime <- read.csv(file="C:/Users/user/Desktop/5A/R/crime.csv", header=TRUE, sep=",")            # Crimes à Boston entre 2015 et 2018, 327 820 lignes sur 17 colonnes
airbnb <- read.csv(file="C:/Users/user/Desktop/5A/R/listings.csv", header=TRUE, sep=",")        # Locations Airbnb à Boston en 2016, 3585 lignes sur 95 colonnes
paris <- read.csv(file="C:/Users/user/Desktop/5A/R/paris.csv", header=TRUE, sep=",")            # Locations Airbnb à Paris en 2018, 6251 lignes sur 95 colonnes

###################################################################################################
######  1) Cartographie des disparitions et des locations AirBnb à Boston. ########################
###################################################################################################

boston=airbnb[(which(airbnb$city=="Boston")),]      ## supprime les données hors de Boston

houseIcon <- makeIcon(
  iconUrl = "https://zupimages.net/up/19/46/iqnl.png",
  iconWidth = 10, iconHeight = 10)

sharedaptIcon <- makeIcon(
  iconUrl = "https://zupimages.net/up/19/46/z0s6.png",
  iconWidth = 10, iconHeight = 10)
                                                                                        # Définitions de marqueurs personnalisés pour affichage de la carte
aptIcon <- makeIcon(
  iconUrl = "https://zupimages.net/up/19/46/831a.png",
  iconWidth = 10, iconHeight = 10)

crimeIcon <- makeIcon(
  iconUrl = "https://zupimages.net/up/19/46/04q8.png",
  iconWidth = 12, iconHeight = 12)

bostonhouses=boston[(which(boston$room_type=="Entire home/apt")),]        
bostonpvrooms=boston[(which(boston$room_type=="Private room")),]                        # isolation des locations par type
bostonshrooms=boston[(which(boston$room_type=="Shared room")),]

plothouses = ddply(bostonhouses,.(room_type),summarize, lat=latitude,long=longitude)
plotpvrooms = ddply(bostonpvrooms,.(room_type),summarize, lat=latitude,long=longitude)  # Suppression des colonnes superflues
plotshrooms = ddply(bostonshrooms,.(room_type),summarize, lat=latitude,long=longitude)


truecrime=crime[(which(crime$Lat!="-1")),]                                              # Suppression des données de localisation erronées
truecrime=truecrime[(which(crime$Long!="-1")),]             

truecrime=subset(truecrime, Lat!="NA")
truecrime=subset(truecrime, Long!="NA")

truecrime=truecrime[(which(truecrime$YEAR=="2016")),]
truecrime=truecrime[(which(truecrime$OFFENSE_CODE_GROUP=="Missing Person Reported")),]  # Reduction du jeu de données aux disparitions de 2016

html_legend <- "
<img src='https://zupimages.net/up/19/46/04q8.png'style='width:11px;height:11px;'> Missing Person Reported<br/>
<img src='https://zupimages.net/up/19/46/z0s6.png' style='width:10px;height:10px;'> AirBnb Shared Appartment<br/>         # Creation d'une légende pour 
<img src='https://zupimages.net/up/19/46/831a.png' style='width:10px;height:10px;'> AirBnb Appartment<br/>                # l'intégration des marqueurs personnalisés
<img src='https://zupimages.net/up/19/46/iqnl.png' style='width:10px;height:10px;'> AirBnb Entire Home
"                                                                           

map <- leaflet()
map <- addTiles(map)
map <- addMarkers(map, lng=plothouses$long, lat=plothouses$lat,icon=houseIcon)                            # affichage en 4 temps des maisons, appartements (partagés ou non)
map <- addMarkers(map, lng=plotpvrooms$long, lat=plotpvrooms$lat,icon=aptIcon)                            # et des disparitions de 2016
map <- addMarkers(map, lng=plotshrooms$long, lat=plotshrooms$lat,icon=sharedaptIcon)
map <- addMarkers(map, lng=truecrime$Long, lat=truecrime$Lat, icon = crimeIcon)
map <- addProviderTiles(map,"CartoDB.Positron")                                                           # modification du calque de fond de carte (plus clair)
map <- addControl(map, html = html_legend, position = "bottomright")                                      # ajout de la légende personnalisée
map <- setView(map, lng=-71.080318, lat=42.319596,zoom = 13)                                              # centrage du rendu final 
#mapshot(map, file = "C:/Users/user/Desktop/5A/R/crimeNhouses.png")                                       # exportation en png              
map

###################################################################################################
######  2) Quels sont les prix des locations favorises par les consommateurs?  ####################
###################################################################################################

boston=airbnb[(which(airbnb$city=="Boston")),]  

boston$price <- sub('\\.00', '', boston$price)
boston$price <- sub(',', '',  boston$price)
boston$price <- as.integer(sub('\\$', '',  boston$price))                                         # remaniement de la colonne Prix. Un nombre '$1,000.00' va devenir '1000'

pricecat=boston$price

pricecat= case_when(
  pricecat >= 500 ~ ">500",
  pricecat>=300 & pricecat<500 ~ "300-500",
  pricecat>=250 & pricecat<300 ~ "250-300",                                                       # Creation d'une nouvelle colonne qui catégorise les prix
  pricecat>=200 & pricecat<250 ~ "200-250",
  pricecat>=150 & pricecat<200 ~ "150-200",
  pricecat>=100 & pricecat<150 ~ "100-150",
  pricecat>=75 & pricecat<100 ~ "75-100",
  pricecat>=50 & pricecat<75 ~ "50-75",
  pricecat>=25 & pricecat<50 ~ "25-50",
  pricecat<25 ~ "<25"
)

boston2= cbind(boston,pricecat)                                                                  # insertion de cette nouvelle colonne

ordered = factor(boston2$pricecat, levels=c("<25","25-50","50-75","75-100","100-150","150-200","200-250","250-300","300-500",">500"), ordered=TRUE)   #tri personnalisé

boston2$pricecat=ordered

tographboston = ddply(boston2,.(pricecat),summarize, number_occur=length(price), Ville="Boston")       # extraction des données de Boston nécessaires au ggplot

paris=paris[(which(paris$city=="Paris"& !is.na(paris$price))),]                                       # Les memes opérations sont réalisées pour Paris

paris$price <- sub('\\.00', '', paris$price)
paris$price <- sub(',', '',  paris$price)
paris$price <- as.integer(sub('\\$', '',  paris$price))

pricecat=paris$price

pricecat= case_when(
  pricecat >= 500 ~ ">500",
  pricecat>=300 & pricecat<500 ~ "300-500",
  pricecat>=250 & pricecat<300 ~ "250-300",
  pricecat>=200 & pricecat<250 ~ "200-250",
  pricecat>=150 & pricecat<200 ~ "150-200",
  pricecat>=100 & pricecat<150 ~ "100-150",
  pricecat>=75 & pricecat<100 ~ "75-100",
  pricecat>=50 & pricecat<75 ~ "50-75",
  pricecat>=25 & pricecat<50 ~ "25-50",
  pricecat<25 ~ "<25"
)

paris2= cbind(paris,pricecat)
paris2=head(paris2,length(boston2$pricecat))                                                      # reduction du jeu de données de Paris pour en avoir autant qu'à Boston

ordered = factor(paris2$pricecat, levels=c("<25","25-50","50-75","75-100","100-150","150-200","200-250","250-300","300-500",">500"), ordered=TRUE)

paris2$pricecat=ordered

tographparis = ddply(paris2,.(pricecat),summarize, number_occur=length(price), Ville="Paris")

new=rbind(tographboston,tographparis)
ggplot(data=new,aes(x=pricecat,y=number_occur,fill=Ville))+                                       # Creation du graph comparatif des prix par nuit selon la ville
  geom_bar (stat="identity", position ="dodge")+
  ggtitle('Repartition des locations à Boston et Paris en fonction du prix par nuit')+
  labs(x="Prix des AirBnb à la nuit ($)", y="Nombre d'annonces")

###################################################################################################
######  3) Quels sont les quartiers les plus chers?  ##############################################
###################################################################################################

boston=airbnb[(which(airbnb$city=="Boston")),]

boston$price <- sub('\\.00', '', boston$price)
boston$price <- sub(',', '',  boston$price)
boston$price <- as.integer(sub('\\$', '',  boston$price))

pricecat=boston$price

pricecat= case_when(
  pricecat >= 500 ~ ">500",
  pricecat>=200 & pricecat<500 ~ "200-500",
  pricecat>=100 & pricecat<200 ~ "100-200",                                                     # meme opération que dans la partie 2), mais réduction du nombre de catégories
  pricecat>=50 & pricecat<100 ~ "50-100",                                                       # pour un affichage plus clair et plus lisible
  pricecat<50 ~ "<50"
)

boston2= cbind(boston,pricecat)

ordered = factor(boston2$pricecat, levels=c("<50","50-100","100-200","200-500",">500"), ordered=TRUE)

boston2$pricecat=ordered

tographboston = ddply(boston2,.(pricecat),summarize, lat=latitude, long=longitude)

factpal <- colorFactor(c('#BCCF02','#5BB12F','#73C5FF','#9B539C','#EB65A0'), tographboston$pricecat)    # creation d'une échelle de couleur personnalisée

map <- leaflet()
map <- addTiles(map)
map <- addCircleMarkers(map, lng=tographboston$long, lat=tographboston$lat, radius=4,stroke = TRUE, opacity=1,color = factpal(pricecat))    #affichage
map <- addProviderTiles(map,"CartoDB.Positron")
map <- setView(map, lng=-71.080318, lat=42.319596,zoom = 13)
map <- addLegend(map,position = 'bottomright', values = pricecat, opacity = 1,title="Prix par nuit ($)", colors=c('#BCCF02','#5BB12F','#73C5FF','#9B539C','#EB65A0'),labels=c("<50","50-100","100-200","200-500",">500"))
#mapshot(map, file = "C:/Users/user/Desktop/5A/R/HousesPrice.png")
map

###################################################################################################
######  4) Quels sont les crimes les plus commis au cours d'une semaine?  #########################
###################################################################################################

#On retourne les crimes avec leur nombre par jour
crimePeriode = ddply(crime, .(DAY_OF_WEEK, OFFENSE_DESCRIPTION, Long, Lat), summarize, nb=length(OFFENSE_CODE_GROUP %in% OFFENSE_CODE_GROUP))
crimePeriode
#On ordonne les crimes les plus commis
crimePeriode = crimePeriode[order(-crimePeriode$nb),]
crimePeriode

#On retourne les 5 premiers crimes les plus commis chaque jour
CP = ddply(crimePeriode, .(DAY_OF_WEEK), function(x){head(arrange(x, -nb),5)})

#On ordonne la data frame precedente dans l'ordre des jours de la semaine
jours = factor(CP$DAY_OF_WEEK, levels=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday", "Sunday"), ordered=TRUE)
crimeTrie=CP
crimeTrie$DAY_OF_WEEK=jours
crimeTrie[order(crimeTrie$DAY_OF_WEEK),]

#On affiche le nombre de crimes total parmi les 5 plus commis par jour de la semaine
ggplot(as.data.frame(crimeTrie),aes(x=DAY_OF_WEEK, y=nb, fill=OFFENSE_DESCRIPTION))+
  geom_bar(stat="identity", position=position_dodge())+
  xlab("Jours de la semaine") +
  ylab("Nombre de crimes
       ") +
  labs(fill="Types de crime") +
  ggtitle("5 crimes les plus commis chaque jour")

#On enregistre notre graphique
#ggsave("D:/Documents/cours/INSA/5A/Analyse Exp_R/Projet/crime_plage.pdf",width=25,height=3)


###################################################################################################
######  5) Cartographie des 10 crimes les plus commis à Boston ####################################
###################################################################################################

category = head(unique(localisation$OFFENSE_DESCRIPTION),10)
category

#On récupère uniquement les localisations des 10 crimes les plus commis
trieCrime=factor(localisation$OFFENSE_DESCRIPTION, category, ordered=TRUE)
middle = localisation
middle$OFFENSE_DESCRIPTION = trieCrime
middle = subset(middle, OFFENSE_DESCRIPTION!="<NA>")
#On se limite à récupérer l'information sur 5 000 crimes afin d'avoir une cartographie la lisible
middle = head(middle, 5000)
middle

#On réalise la cartographie des 10 crimes les plus commis
col = c("blue","red","green","orange","brown","black","magenta","pink","grey","purple")
pal = colorFactor(col, domain=category)

map <- leaflet()
map <- addTiles(map)
map <- addCircleMarkers(map, lng=middle$Long, lat=middle$Lat, radius=1, color=pal(middle$OFFENSE_DESCRIPTION))
map <- addLegend(map, position="bottomright", colors=col, labels=category)
map <- addProviderTiles(map,"CartoDB.Positron")
map


