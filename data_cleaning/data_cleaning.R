library(dplyr)
library(readr)

### Charger les datasets bruts 

#1. La rubrique CARACTERISTIQUES qui décrit les circonstances générales 
#de l’accident
# première lecture
caracteristiques.2019 <- read.csv2("data/caracteristiques-2019.csv")
# correction des type de colones
cara_ctype = cols(
  Num_Acc = col_double(),
  jour = col_double(),
  mois = col_double(),
  an = col_double(),
  hrmn = col_time(format = ""),
  lum = col_factor(levels = c(" -1",1:5)),
  dep = col_character(),
  com = col_character(),
  agg = col_double(),
  int = col_factor(levels = c(" -1",1:9)),
  atm = col_factor(),
  col = col_factor(),
  adr = col_character(),
  lat = col_character(),
  long = col_character())


caracteristiques.2019 = read_delim("data/caracteristiques-2019.csv",delim=";",col_types = cara_ctype)
levels(caracteristiques.2019$lum)=c("Non renseigné","Plein jour","Crépuscule ou aube","Nuit sans éclairage public","Nuit avec éclairage public non allumé","Nuit avec éclairage public allumé")
levels(caracteristiques.2019$int)=c("Non renseigné","Hors intersection","Intersection en X","Intersection en T","Intersection en Y", "Intersection à plus de 4 branches","Giratoire","Place", "Passage à niveau", "Autre intersection")
caracteristiques.2019 = caracteristiques.2019 %>% mutate(lat=as.numeric(gsub(",",".",lat)),long=as.numeric(gsub(",",".",long)))


#2. La rubrique LIEUX qui décrit le lieu principal de l’accident
#même si celui-ci s’est déroulé à une intersection

lieux.2019 <- read_delim("data/lieux-2019.csv", delim=";")

ctype_lieux = cols(
  Num_Acc = col_double(),
  catr = col_factor(as.character(c(1:7,9))),
  voie = col_character(),
  v1 = col_double(),
  v2 = col_character(),
  circ = col_character(),
  nbv = col_character(),
  vosp = col_character(),
  prof = col_character(),
  pr = col_character(),
  pr1 = col_character(),
  plan = col_character(),
  lartpc = col_double(),
  larrout = col_double(),
  surf = col_character(),
  infra = col_character(),
  situ = col_character(),
  vma = col_character()
)
# A corriger
lieux.2019 <- read_delim("data/lieux-2019.csv", delim=";", col_types = ctype_lieux)
levels(lieux.2019$catr)=c("Autoroute","Route nationale","Route Départementale","Voie Communales","Hors réseau public", "Parc de stationnement ouvert à la circulation publique","Routes de métropole urbaine","Autre") 
unique(lieux.2019$catr)
#3. La rubrique VEHICULES impliqués
vehicules.2019 <- read_delim("../data/vehicules-2019.csv", delim=";")

ctype_vehi = cols(
  Num_Acc = col_double(),
  id_vehicule = col_character(),
  num_veh = col_character(),
  senc = col_factor(),
  catv = col_factor(as.character(c(0,1:3,7,10, 13:17,20, 21, 30:43, 50, 60, 80, 99))),
  obs = col_character(),
  obsm = col_character(),
  choc = col_character(),
  manv = col_factor(),
  motor = col_character(),
  occutc = col_double()
)
vehicules.2019 <- read_delim("data/vehicules-2019.csv", delim=";",col_types = ctype_vehi)

levels_vehicules = c("Indéterminable",
"Bicyclette",
"Cyclomoteur <50cm3",
"Voiturette",
"VL seul",
"VU seul",
"PL seul 3,5T <PTCA <= 7,5T",
"PL seul > 7,5T",
"PL > 3,5T + remorque",
"Tracteur routier seul",
"Tracteur routier + semi-remorque",
"Engin spécial",
"Tracteur agricole",
"Scooter < 50 cm3",
"Motocyclette > 50 cm 3 et <= 125 cm 3",
"Scooter > 50 cm 3 et <= 125 cm 3",
"Motocyclette > 125 cm 3",
"Scooter > 125 cm 3",
"Quad léger <= 50 cm 3",
"Quad lourd > 50 cm 3",
"Autobus",
"Autocar",
"Train",
"Tramway",
"3RM <= 50 cm3",
"3RM > 50 cm3 <= 125 cm3",
"3RM > 125 cm3",
"EDP à moteur",
"EDP sans moteur",
"VAE",
"Autre véhicule")
levels(vehicules.2019$catv)=levels_vehicules


#4. La rubrique USAGERS impliqués
usagers.2019 <- read_delim("data/usagers-2019.csv",
                         delim=";")

ctype_usagers = cols(
  Num_Acc = col_double(),
  id_vehicule = col_character(),
  num_veh = col_character(),
  place = col_double(),
  catu = col_double(),
  grav = col_double(),
  sexe = col_factor(c("1","2")),
  an_nais = col_double(),
  trajet = col_character(),
  secu1 = col_factor(),
  secu2 = col_character(),
  secu3 = col_character(),
  locp = col_character(),
  actp = col_character(),
  etatp = col_character()
)
usagers.2019 <- read_delim("data/usagers-2019.csv",
                           delim=";",col_types = ctype_usagers)
levels(usagers.2019$sexe)=c("Masculin","Féminin")


### Constituer le dataset du cours/TP
accidents.2019 <- 
  usagers.2019 %>%
  select(Num_Acc,id_vehicule,grav,sexe,an_nais) %>%
  left_join(
    caracteristiques.2019 %>%
      select(Num_Acc, lat, long, com, int, lum)
    ) %>%
  left_join(
    lieux.2019 %>% 
      select(Num_Acc,voie,catr)
    ) %>%
  left_join(
    vehicules.2019 %>% 
      select(Num_Acc,id_vehicule,catv)
  )

accidents.2019.paris <- accidents.2019 %>% filter(substr(com,1,2)==75)
#saveRDS(accidents.2019.paris,"lecture/data/accidents2019_paris.RDS")

# Transformation en objet sf
library(sf)
accidents.2019.paris <- st_as_sf(accidents.2019.paris,
                                 coords = c("long", "lat"),
                                 crs = 4226, agr = "constant") %>% 
  st_transform(2154)
#st_write(accidents.2019.paris,"exercises/data/accidents2019_paris.geojson",delete_dsn = TRUE)

######## Extraction du fond de carte des iris (IGN)
#https://geoservices.ign.fr/documentation/diffusion/telechargement-donnees-libres.html#contoursiris
#donnees 2019
iris.fra <- st_read("data/shp_iris/CONTOURS-IRIS.shp", stringsAsFactors = F) 
iris.75 <- iris.fra[substr(iris.fra$INSEE_COM, 1,2)=="75",c("CODE_IRIS","INSEE_COM")]
#st_write(iris.75,"lecture/data/iris_75.shp")
#st_write(iris.75,"exercises/data/iris_75.shp")
