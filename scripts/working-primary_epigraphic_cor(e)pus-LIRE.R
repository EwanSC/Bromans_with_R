#trying to use LIRE
#5/9/2024

library(dplyr)
library(sqldf)


#getting data using parquet format, following https://docs.opendatablend.io/open-data-blend-datasets/loading-data-files-in-r

library(arrow)

#get all data
LIRE <- read_parquet("data/LIRE_v3-0.parquet")

#dates missing, checking older version
LIRE_2_3 <- read_parquet("data/LIRE_v2-3.parquet")
LIRE_2_2 <- read_parquet("data/LIRE_v2-2.parquet")
LIRE_2_1 <- read_parquet("data/LIRE_v2-1.parquet")

#get only some columns
LIRE_filtered <- read_parquet("data/LIRE_v3-0.parquet", 
                                col_select = c("LIST-ID", 
                                               "EDCS-ID", 
                                               "EDH-ID", 
                                               "not_before", 
                                               "not_after", 
                                               "inscription", 
                                               "transcription", 
                                               "clean_text_interpretive_word", 
                                               "type_of_inscription_clean", 
                                               "province", 
                                               "province_label_clean", 
                                               "place", 
                                               "findspot_ancient_clean", 
                                               "findspot_modern_clean", 
                                               "status_notation", 
                                               "type_of_monument_clean", 
                                               "type_of_inscription_clean",
                                               "Latitude",
                                               "Longitude"))


#get only Dalmatia
LIRE_Dal <- filter(LIRE_filtered, province == "Dalmatia")
LIRE_Dal_clean <- filter(LIRE_filtered, province_label_clean == "Dalmatia")

#get places
LIRE_Dal_places <- sqldf("Select DISTINCT place from LIRE_Dal")
write.csv(LIRE_Dal_places, file = 'data/LIRE_Dal_places.csv', row.names = TRUE)

LIRE_Dal_clean_places <- sqldf("Select DISTINCT place from LIRE_Dal_clean")
write.csv(LIRE_Dal_clean_places, file = 'data/LIRE_Dal_clean_places.csv', row.names = TRUE)


#clean places
  clean_LIRE_Dal <- LIRE_Dal %>%
  mutate(`cleaned_place`=ifelse(place %in% c('Burnum',
                                             'Ivosevci / Burnum',
                                             'Karlovac / Karlstadt / Burnum',
                                             'Kistanje / Burnum','Knin / Burnum',
                                             'Mokro Polje / Burnum',
                                             'Puljane / Pugliane / Burnum',
                                             'Strmca / Stermizza / Burnum',
                                             'Mratovo / Burnum'),
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
                                             'Dugopolje / Salona',
                                             'Lecevica / Salona',
                                             'Kapljuc / Salona',
                                             'Grudine / Salona',
                                             'Gradina / Salona',
                                             'Rupotine / Rupotina / Salona',
                                             'Vranjic / Vragnizza / Salona',
                                             'Zrnovnica / Salona'),
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
                                             'Ruzic / Balijna Glavica / Municipium Magnum',
                                             'Otavice / Municipium Magnum',
                                             'Ramljane / Municipium Magnum',
                                             'Umljanovic / Municipium Magnum'),
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

clean_LIRE_Dal_clean <- LIRE_Dal_clean %>%
  mutate(`cleaned_place`=ifelse(place %in% c('Burnum',
                                             'Ivosevci / Burnum',
                                             'Karlovac / Karlstadt / Burnum',
                                             'Kistanje / Burnum','Knin / Burnum',
                                             'Mokro Polje / Burnum',
                                             'Puljane / Pugliane / Burnum',
                                             'Strmca / Stermizza / Burnum',
                                             'Mratovo / Burnum'),
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
                                             'Dugopolje / Salona',
                                             'Lecevica / Salona',
                                             'Kapljuc / Salona',
                                             'Grudine / Salona',
                                             'Gradina / Salona',
                                             'Rupotine / Rupotina / Salona',
                                             'Vranjic / Vragnizza / Salona',
                                             'Zrnovnica / Salona'),
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
                                             'Ruzic / Balijna Glavica / Municipium Magnum',
                                             'Otavice / Municipium Magnum',
                                             'Ramljane / Municipium Magnum',
                                             'Umljanovic / Municipium Magnum'),
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


#now military
LIRE_Dal_mil <- sqldf("Select * from clean_LIRE_Dal
                  WHERE clean_text_interpretive_word 
                    LIKE '%legio%'
                  OR clean_text_interpretive_word 
                    LIKE '%cohor%'
                  OR clean_text_interpretive_word 
                    LIKE '%ala%'
                  OR clean_text_interpretive_word 
                    LIKE '%alae%'
                  OR clean_text_interpretive_word 
                    LIKE '%milit%'
                  OR clean_text_interpretive_word 
                    LIKE '%eques%'
                  OR clean_text_interpretive_word 
                    LIKE '%equit%'
                  OR clean_text_interpretive_word 
                    LIKE '%duplicari%'
                  OR clean_text_interpretive_word 
                    LIKE '%veteran%'
                  or clean_text_interpretive_word
                    LIKE '%centuri%'
                  or clean_text_interpretive_word
                    LIKE '%immun%'
                  or clean_text_interpretive_word
                    LIKE '%miles%'
                  or clean_text_interpretive_word
                    LIKE '%beneficiar%'
                  or clean_text_interpretive_word
                    LIKE '%tesserari%'
                  or clean_text_interpretive_word
                    LIKE '%signifer%'
                  or clean_text_interpretive_word
                    LIKE '%aquilifer%'
                  or clean_text_interpretive_word
                    LIKE '%imaginifer%'
                  or clean_text_interpretive_word
                    LIKE '%corniculari%'
                  or clean_text_interpretive_word
                    LIKE '%principalis%'
                  or clean_text_interpretive_word
                    LIKE '%primus pilus%'
                  or clean_text_interpretive_word
                    LIKE '%primo pilo%'
                  or clean_text_interpretive_word
                    LIKE '%primi pili%'
                  or clean_text_interpretive_word
                    LIKE '%praefectus castrorum%'
                  or clean_text_interpretive_word
                    LIKE '%optio %'
                  or clean_text_interpretive_word
                    LIKE '%option%'
                  or status_notation
                    LIKE '%milites%'
                  OR cleaned_place = 'Tilurium'
                  OR cleaned_place = 'Burnum'
                  OR cleaned_place = 'Andetrium'
                  OR cleaned_place = 'Bigeste'
                  ")

LIRE_Dal_mil_clean <- sqldf("Select * from clean_LIRE_Dal_clean
                  WHERE clean_text_interpretive_word 
                    LIKE '%legio%'
                  OR clean_text_interpretive_word 
                    LIKE '%cohor%'
                  OR clean_text_interpretive_word 
                    LIKE '%ala%'
                  OR clean_text_interpretive_word 
                    LIKE '%alae%'
                  OR clean_text_interpretive_word 
                    LIKE '%milit%'
                  OR clean_text_interpretive_word 
                    LIKE '%eques%'
                  OR clean_text_interpretive_word 
                    LIKE '%equit%'
                  OR clean_text_interpretive_word 
                    LIKE '%duplicari%'
                  OR clean_text_interpretive_word 
                    LIKE '%veteran%'
                  or clean_text_interpretive_word
                    LIKE '%centuri%'
                  or clean_text_interpretive_word
                    LIKE '%immun%'
                  or clean_text_interpretive_word
                    LIKE '%miles%'
                  or clean_text_interpretive_word
                    LIKE '%beneficiar%'
                  or clean_text_interpretive_word
                    LIKE '%tesserari%'
                  or clean_text_interpretive_word
                    LIKE '%signifer%'
                  or clean_text_interpretive_word
                    LIKE '%aquilifer%'
                  or clean_text_interpretive_word
                    LIKE '%imaginifer%'
                  or clean_text_interpretive_word
                    LIKE '%corniculari%'
                  or clean_text_interpretive_word
                    LIKE '%principalis%'
                  or clean_text_interpretive_word
                    LIKE '%primus pilus%'
                  or clean_text_interpretive_word
                    LIKE '%primo pilo%'
                  or clean_text_interpretive_word
                    LIKE '%primi pili%'
                  or clean_text_interpretive_word
                    LIKE '%praefectus castrorum%'
                  or clean_text_interpretive_word
                    LIKE '%optio %'
                  or clean_text_interpretive_word
                    LIKE '%option%'
                  or status_notation
                    LIKE '%milites%'
                  OR cleaned_place = 'Tilurium'
                  OR cleaned_place = 'Burnum'
                  OR cleaned_place = 'Andetrium'
                  OR cleaned_place = 'Bigeste'
                  ")
