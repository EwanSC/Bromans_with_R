# Cleaning Epig Scrape Data
## ESC 27/04/2020
# Aim is to clean the most recent data from EDCS via Lat-Epig scraper https://github.com/mqAncientHistory/Lat-Epig

library(tidyverse)

# get the data using premade function

RawEpigData <- load_epig_data("data/2022-04-26-EDCS_via_Lat_Epig-prov_Dalmatia-10140.json")

str(RawEpigData)

head(RawEpigData)

# lets count by place with the RwData

SumRawED <-  RawEpigData %>%
group_by(place) %>%
  count(place)

# lets remove unnecessary columns

SelectEpigData <- RawEpigData %>%
  select(`EDCS-ID`,publication,province,place,`dating_from`,`dating_to`,status,inscription,`inscription_interpretive_cleaning`,latitude,longitude)

# lets clean 'place'
# first count it

SumEDPlace <-  SelectEpigData %>%
  group_by(place) %>%
  count(place)

#lets practice making all major sites one location with ifelse

CleanerEDPlace <- SelectEpigData %>%
  mutate(`cleaned_place`=ifelse(place %in% c('Burnum',
                                             'Ivosevci / Burnum',
                                             'Karlovac / Karlstadt / Burnum',
                                             'Kistanje / Burnum','Knin / Burnum',
                                             'Mokro Polje / Burnum',
                                             'Puljane / Pugliane / Burnum',
                                             'Strmca / Stermizza / Burnum'),
                                'Burnum', place)) %>%
  mutate(`cleaned_place`=ifelse(place %in% c('Gardun / Tilurium',
                                             'Trilj / Tilurium',
                                             'Vojnic Sinjski / Tilurium'),
                                'Tilurium', `cleaned_place`)) %>%
  mutate(`cleaned_place`=ifelse(place %in% c('Kastel Sucurac / Salona',
                                             'Manastirine / Salona',
                                             'Pazdigrad / Pazdigrada / Salona',
                                             'Solin / Salona',
                                             'Vrlika / Vrlica / Salona',
                                             'Dugopolje / Salona'),
                                'Salona', `cleaned_place`)) %>%
  mutate(`cleaned_place`=ifelse(place %in% c('Asseria',
                                             'Benkovac / Asseria',
                                             'Podgrade / Asseria'),
                                'Asseria', `cleaned_place`)) %>%
  mutate(`cleaned_place`=ifelse(place %in% c('Beli / Caisole / Crexa',
                                             'Cres / Crexa'),
                                'Crexa', `cleaned_place`)) %>%
  mutate(`cleaned_place`=ifelse(place %in% c('Biograd na Moru / Zaravecchia / Nedinum',
                                             'Nadin / Nedinum',
                                             'Skabrnja / Nedinum'),
                                'Nedinum', `cleaned_place`)) %>%
  mutate(`cleaned_place`=ifelse(place %in% c('Bastasi / Municipium Salvium',
                                             'Bosansko Grahovo / Municipium Salvium',
                                             'Glamoc / Municipium Salvium',
                                             'Glavica / Municipium Salvium','Podgradina / Municipium Salvium'),
                                'Municipium Salvium', `cleaned_place`)) %>%
  mutate(`cleaned_place`=ifelse(place %in% c('Gornji Muc / Andetrium'),
                                'Andetrium', `cleaned_place`)) %>%
  mutate(`cleaned_place`=ifelse(place %in% c('Bigeste',
                                             'Hardomilje / Bigeste',
                                             'Humac / Bigeste',
                                             'Ljubuski / Mlade / Bigeste',
                                             'Veljaci / Bigeste'),
                                'Bigeste', `cleaned_place`)) %>%
  mutate(`cleaned_place`=ifelse(place %in% c('Bihac / Raetinium',
                                             'Golubic / Raetinium',
                                             'Pritoka / Raetinium',
                                             'Ribic / Raetinium',
                                             'Jezerine, Golubic'),
                                'Raetinium', `cleaned_place`)) %>%
  mutate(`cleaned_place`=ifelse(place %in% c('Brac, Otok / Brattia',
                                             'Skrip / Brattia'),
                                'Brattia', `cleaned_place`)) %>%
  mutate(`cleaned_place`=ifelse(place %in% c('Bribir / Bribirska Glavica / Varvaria',
                                             'Lastve, Bribir'),
                                'Varvaria', `cleaned_place`)) %>%
  mutate(`cleaned_place`=ifelse(place %in% c('Budva / Buthoe'),
                                'Buthoe', `cleaned_place`)) %>%
  mutate(`cleaned_place`=ifelse(place %in% c('Cacak / Malvesiatium',
                                             'Rudo / Municipium Malvesatium'),
                                'Municipium Malvesatium', `cleaned_place`)) %>%
  mutate(`cleaned_place`=ifelse(place %in% c('Caska / Cissa'),
                                'Cissa', `cleaned_place`)) %>%
  mutate(`cleaned_place`=ifelse(place %in% c('Cavtat / Epidaurum'),
                                'Epidaurum', `cleaned_place`)) %>%
  mutate(`cleaned_place`=ifelse(place %in% c('Citluk / Aequum'),
                                'Aequum', `cleaned_place`)) %>%
  mutate(`cleaned_place`=ifelse(place %in% c('Danilo Gornje / Rider',
                                             'Sibenik / Rider',
                                             'Grusine, Sibenik'),
                                'Rider', `cleaned_place`)) %>%
  mutate(`cleaned_place`=ifelse(place %in% c('Dol / Pharia',
                                             'Hvar / Lesina / Pharia'),
                                'Pharia', `cleaned_place`)) %>%
  mutate(`cleaned_place`=ifelse(place %in% c('Donje Biljane / Iader',
                                             'Galovac / Iader',
                                             'Zadar / Iader'),
                                'Iader', `cleaned_place`)) %>%
  mutate(`cleaned_place`=ifelse(place %in% c('Dubrava / Diluntum',
                                             'Stolac / Rotimlja / Megjina / Diluntum'),
                                'Diluntum', `cleaned_place`)) %>%
  mutate(`cleaned_place`=ifelse(place %in% c('Duklja / Duklje / Rusevine / Doclea'),
                                'Doclea', `cleaned_place`)) %>%
  mutate(`cleaned_place`=ifelse(place %in% c('Gradina / Domavium'),
                                'Domavium', `cleaned_place`)) %>%
  mutate(`cleaned_place`=ifelse(place %in% c('Janjina / Peljesac / Sabbioncello / Narona',
                                             'Metkovic / Narona',
                                             'Vid / Narona'),
                                'Narona', `cleaned_place`)) %>%
  mutate(`cleaned_place`=ifelse(place %in% c('Kapitul - Knin'),
                                'Kapitul', `cleaned_place`)) %>%
  mutate(`cleaned_place`=ifelse(place %in% c('Karin Gornji / Corinium'),
                                'Corinium', `cleaned_place`)) %>%
  mutate(`cleaned_place`=ifelse(place %in% c('Klis / Clissa'),
                                'Cliss', `cleaned_place`)) %>%
  mutate(`cleaned_place`=ifelse(place %in% c('Kljake / Municipium Magnum',
                                             'Ruzic / Balijna Glavica / Municipium Magnum'),
                                'Municipium Magnum', `cleaned_place`)) %>%
  mutate(`cleaned_place`=ifelse(place %in% c('Kolovrat / Municipium Splonistarum',
                                             'Prijepolje / Municipium Splonistarum'),
                                'Municipium Splonistarum', `cleaned_place`)) %>%
  mutate(`cleaned_place`=ifelse(place %in% c('Koplik / Cinna'),
                                'Cinna', `cleaned_place`)) %>%
  mutate(`cleaned_place`=ifelse(place %in% c('Korcula / Nigra Corcyra'),
                                'Corcyra Nigra', `cleaned_place`)) %>%
  mutate(`cleaned_place`=ifelse(place %in% c('Kornat, Otok'),
                                'Kornat', `cleaned_place`)) %>%
  mutate(`cleaned_place`=ifelse(place %in% c('Kotor / Acruvium'),
                                'Acruvium', `cleaned_place`)) %>%
  mutate(`cleaned_place`=ifelse(place %in% c('Krk, Otok / Curictae'),
                                'Curictae', `cleaned_place`)) %>%
  mutate(`cleaned_place`=ifelse(place %in% c('Krklic, Pakostane'),
                                'Pakostane', `cleaned_place`)) %>%
  mutate(`cleaned_place`=ifelse(place %in% c('Krug, Jesenice'),
                                'Jesenice', `cleaned_place`)) %>%
  mutate(`cleaned_place`=ifelse(place %in% c('Labin / Alvona'),
                                'Alvona', `cleaned_place`)) %>%
  mutate(`cleaned_place`=ifelse(place %in% c('Lastovo / Lagosta / Ladesta'),
                                'Lastovo', `cleaned_place`)) %>%
  mutate(`cleaned_place`=ifelse(place %in% c('Lezhe / Lest / Lissus'),
                                'Lissus', `cleaned_place`)) %>%
  mutate(`cleaned_place`=ifelse(place %in% c('Listani / Lastani'),
                                'Listani', `cleaned_place`)) %>%
  mutate(`cleaned_place`=ifelse(place %in% c('Livno / Bariduum / Delminium',
                                             'Prisoje / Delminium',
                                             'Suhaca / Delminium',
                                             'Tomislavgrad / Duvno / Zupanjac / Delminium'),
                                'Delminium', `cleaned_place`)) %>%
  mutate(`cleaned_place`=ifelse(place %in% c('Mali Losinj / Apsorus'),
                                'Apsorus', `cleaned_place`)) %>%
  mutate(`cleaned_place`=ifelse(place %in% c('Medvida / Medvigge / Hadra / Sidrona'),
                                'Sidrona', `cleaned_place`)) %>%
  mutate(`cleaned_place`=ifelse(place %in% c('Niksic / Mons Nigia / Anderva'),
                                'Anderva', `cleaned_place`)) %>%
  mutate(`cleaned_place`=ifelse(place %in% c('Nin / Aenona'),
                                'Aenona', `cleaned_place`)) %>%
  mutate(`cleaned_place`=ifelse(place %in% c('Obrovac / Clambetae'),
                                'Clambetae', `cleaned_place`)) %>%
  mutate(`cleaned_place`=ifelse(place %in% c('Omis / Almissa / Oneum / Onaeum'),
                                'Oneum', `cleaned_place`)) %>%
  mutate(`cleaned_place`=ifelse(place %in% c('Omisalj / Fulfinum'),
                                'Fulfinum', `cleaned_place`)) %>%
  mutate(`cleaned_place`=ifelse(place %in% c('Osor / Opsorus'),
                                'Opsorus', `cleaned_place`)) %>%
  mutate(`cleaned_place`=ifelse(place %in% c('Ostrovica / Alveria'),
                                'Alveria', `cleaned_place`)) %>%
  mutate(`cleaned_place`=ifelse(place %in% c('Perast / Risinium',
                                             'Risan / Risano / Risinium'),
                                'Risinium', `cleaned_place`)) %>%
  mutate(`cleaned_place`=ifelse(place %in% c('Plomin / Flanona'),
                                'Flanona', `cleaned_place`)) %>%
  mutate(`cleaned_place`=ifelse(place %in% c('Podstrana / Pituntium'),
                                'Pituntium', `cleaned_place`)) %>%
  mutate(`cleaned_place`=ifelse(place %in% c('Rab, Otok / Arba'),
                                'Arba', `cleaned_place`)) %>%
  mutate(`cleaned_place`=ifelse(place %in% c('Ruduse / Rudusa'),
                                'Rudusa', `cleaned_place`)) %>%
  mutate(`cleaned_place`=ifelse(place %in% c('Runovici / Runovic / Novae'),
                                'Novae', `cleaned_place`)) %>%
  mutate(`cleaned_place`=ifelse(place %in% c('Seget Donji / Tragurium',
                                             'Trogir / Tragurium'),
                                'Tragurium', `cleaned_place`)) %>%
  mutate(`cleaned_place`=ifelse(place %in% c('Senj / Segna / Zengg / Senia'),
                                'Senia', `cleaned_place`))%>%
  mutate(`cleaned_place`=ifelse(place %in% c('Shkoder / Shkodra / Skutari / Scodra'),
                                'Scodra', `cleaned_place`)) %>%
  mutate(`cleaned_place`=ifelse(place %in% c('Sinj / Osinium'),
                                'Osinium', `cleaned_place`)) %>%
  mutate(`cleaned_place`=ifelse(place %in% c('Skradin / Scardona'),
                                'Scardona', `cleaned_place`)) %>%
  mutate(`cleaned_place`=ifelse(place %in% c('Solta, Otok / Solentia'),
                                'Solentia', `cleaned_place`)) %>%
  mutate(`cleaned_place`=ifelse(place %in% c('Split / Spalatum'),
                                'Spalatum', `cleaned_place`)) %>%
  mutate(`cleaned_place`=ifelse(place %in% c('Stari Grad / Argyruntum'),
                                'Argyruntum', `cleaned_place`)) %>%
  mutate(`cleaned_place`=ifelse(place %in% c('Stobrec / Epetino / Epetium'),
                                'Epetium', `cleaned_place`)) %>%
  mutate(`cleaned_place`=ifelse(place %in% c('Sveti Juraj / Lopsica'),
                                'Lopsica', `cleaned_place`)) %>%
  mutate(`cleaned_place`=ifelse(place %in% c('Tepljuh / Promona'),
                                'Promona', `cleaned_place`)) %>%
  mutate(`cleaned_place`=ifelse(place %in% c('Tivat / Teodo'),
                                'Teodo', `cleaned_place`)) %>%
  mutate(`cleaned_place`=ifelse(place %in% c('Trsat / Tersatto / Tarsatica'),
                                'Tarsatica', `cleaned_place`)) %>%
  mutate(`cleaned_place`=ifelse(place %in% c('Vele Srakane / Canidole Grande'),
                                'Vele Srakane', `cleaned_place`)) %>%
  mutate(`cleaned_place`=ifelse(place %in% c('Veli Bijac / Bijaci / Bijaca / Siculi'),
                                'Siculi', `cleaned_place`)) %>%
  mutate(`cleaned_place`=ifelse(place %in% c('Velika Kopanica / Kopjenica'),
                                'Kopjenica', `cleaned_place`)) %>%
  mutate(`cleaned_place`=ifelse(place %in% c('Vis / Issa'),
                                'Issa', `cleaned_place`)) %>%
  mutate(`cleaned_place`=ifelse(place %in% c('Vitalj, Otocac / Arupium'),
                                'Arupium', `cleaned_place`)) %>%
  mutate(`cleaned_place`=ifelse(place %in% c('Zaostrog / Zastrogh'),
                                'Zaostrog', `cleaned_place`))


CleanEDPlace <- CleanerEDPlace %>%
  select(`EDCS-ID`,publication,province,`place`,`cleaned_place`,`dating_from`,`dating_to`,status,inscription,`inscription_interpretive_cleaning`,latitude,longitude,photo) %>%
  group_by(`cleaned_place`)

SumCleanEDPlace <-  CleanEDPlace %>%
  group_by(`cleaned_place`) %>%
  count(`cleaned_place`)

## Made the above (lines 35-237) into function which is now at scripts/primary_epigraphic_cor(e)pus.R

## now to export

write.csv(CleanEDPlace,"data/clean_lat-epig_scrape-place.csv", row.names = FALSE)


##works, but there must be a quicker way...
