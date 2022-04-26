# Cleaning Epig Scrape Data
# ESC 17/12/2021 
# Aim is to clean the most recent data from 

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
  mutate(`ancient_place`=ifelse(place %in% c('Burnum',
                                             'Ivosevci / Burnum',
                                             'Karlovac / Karlstadt / Burnum',
                                             'Kistanje / Burnum','Knin / Burnum',
                                             'Mokro Polje / Burnum',
                                             'Puljane / Pugliane / Burnum',
                                             'Strmca / Stermizza / Burnum'), 
                                'Burnum', place)) %>%
  mutate(`ancient place`=ifelse(place %in% c('Gardun / Tilurium',
                                             'Trilj / Tilurium',
                                             'Vojnic Sinjski / Tilurium'), 
                                'Tilurium', `ancient_place`)) %>%
  mutate(`ancient place`=ifelse(place %in% c('Kastel Sucurac / Salona',
                                             'Manastirine / Salona',
                                             'Pazdigrad / Pazdigrada / Salona',
                                             'Solin / Salona',
                                             'Vrlika / Vrlica / Salona',
                                             'Dugopolje / Salona'), 
                                'Salona', `ancient_place`)) %>%
  mutate(`ancient place`=ifelse(place %in% c('Asseria',
                                             'Benkovac / Asseria',
                                             'Podgrade / Asseria'),
                                'Asseria', `ancient_place`)) %>%
  mutate(`ancient place`=ifelse(place %in% c('Beli / Caisole / Crexa',
                                             'Cres / Crexa'),
                                'Crexa', `ancient_place`)) %>%
  mutate(`ancient place`=ifelse(place %in% c('Biograd na Moru / Zaravecchia / Nedinum',
                                             'Nadin / Nedinum',
                                             'Skabrnja / Nedinum'),
                                'Nedinum', `ancient_place`)) %>%
  mutate(`ancient place`=ifelse(place %in% c('Bastasi / Municipium Salvium',
                                             'Bosansko Grahovo / Municipium Salvium',
                                             'Glamoc / Municipium Salvium',
                                             'Glavica / Municipium Salvium','Podgradina / Municipium Salvium'),
                                'Municipium Salvium', `ancient_place`)) %>%
  mutate(`ancient place`=ifelse(place %in% c('Gornji Muc / Andetrium'),
                                'Andetrium', `ancient_place`)) %>%
  mutate(`ancient place`=ifelse(place %in% c('Bigeste',
                                             'Hardomilje / Bigeste',
                                             'Humac / Bigeste',
                                             'Ljubuski / Mlade / Bigeste',
                                             'Veljaci / Bigeste'),
                                'Bigeste', `ancient_place`)) %>%
  mutate(`ancient place`=ifelse(place %in% c('Bihac / Raetinium',
                                             'Golubic / Raetinium',
                                             'Pritoka / Raetinium',
                                             'Ribic / Raetinium'),
                                'Raetinium', `ancient_place`)) %>%
  mutate(`ancient place`=ifelse(place %in% c('Brac, Otok / Brattia',
                                             'Skrip / Brattia'),
                                'Brattia', `ancient_place`)) %>%
  mutate(`ancient place`=ifelse(place %in% c('Bribir / Bribirska Glavica / Varvaria',
                                             'Lastve, Bribir'),
                                'Varvaria', `ancient_place`)) %>%
  mutate(`ancient place`=ifelse(place %in% c('Budva / Buthoe'),
                                'Buthoe', `ancient_place`)) %>%
  mutate(`ancient place`=ifelse(place %in% c('Cacak / Malvesiatium'),
                                'Malvesiatium', `ancient_place`)) %>%
  mutate(`ancient place`=ifelse(place %in% c('Caska / Cissa'),
                                'Cissa', `ancient_place`)) %>%
  mutate(`ancient place`=ifelse(place %in% c('Cavtat / Epidaurum'),
                                'Epidaurum', `ancient_place`)) %>%
  mutate(`ancient place`=ifelse(place %in% c('Citluk / Aequum'),
                                'Aequum', `ancient_place`)) %>%
  mutate(`ancient place`=ifelse(place %in% c('Danilo Gornje / Rider',
                                             'Sibenik / Rider',
                                             'Grusine, Sibenik'),
                                'Rider', `ancient_place`)) %>%
  mutate(`ancient place`=ifelse(place %in% c('Dol / Pharia',
                                             'Hvar / Lesina / Pharia'),
                                'Pharia', `ancient_place`)) %>%
  mutate(`ancient place`=ifelse(place %in% c('Donje Biljane / Iader',
                                             'Galovac / Iader',
                                             'Zadar / Iader'),
                                'Iader', `ancient_place`)) %>%
  mutate(`ancient place`=ifelse(place %in% c('Dubrava / Diluntum',
                                             'Stolac / Rotimlja / Megjina / Diluntum'),
                                'Diluntum', `ancient_place`)) %>%
  mutate(`ancient place`=ifelse(place %in% c('Duklja / Duklje / Rusevine / Doclea'),
                                'Doclea', `ancient_place`)) %>%
  mutate(`ancient place`=ifelse(place %in% c('Gradina / Domavium'),
                                'Domavium', `ancient_place`))

SumCleanerEDPlace <-  CleanerEDPlace %>% 
  group_by(`ancient_place`) %>% 
  count(`ancient_place`)

CleanEDPlace <- CleanerEDPlace %>%
  select(`EDCS-ID`,publication,province,`place`,`ancient_place`,`dating_from`,`dating_to`,status,inscription,`inscription_interpretive_cleaning`,latitude,longitude) %>%
  group_by(`ancient_place`)

##works, but there must be a quicker way...

## lets have a look at status
SumEDStatus <-  SelectEpigData %>% 
  group_by(status) %>% 
  count(status)
