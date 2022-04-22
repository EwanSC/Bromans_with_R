# Cleaning Epig Scrape Data
# ESC 17/12/2021 
# Aim is to clean the most recent data from 

library(tidyverse)

# get the data using premade function

RawEpigData <- load_epig_data("data/2021-11-16-EDCS_via_Lat_Epig-prov_Dalmatia-10140.json")

str(RawEpigData)

head(RawEpigData)

# lets count by place with the RwData

SumRawED <-  RawEpigData %>% 
group_by(place) %>% 
  count(place)

# lets remove unnecessary columns

SelectEpigData <- RawEpigData %>%
  select(`EDCS-ID`,publication,province,place,`dating from`,`dating to`,status,inscription,`inscription interpretive cleaning`,Latitude,Longitude)

# lets clean 'place'
# first count it

SumEDPlace <-  SelectEpigData %>% 
  group_by(place) %>% 
  count(place)

#lets practice making all major sites one location with ifelse

CleanerEDPlace <- SelectEpigData %>%
  mutate(`ancient place`=ifelse(place %in% c('Burnum',
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
                                'Tilurium', `ancient place`)) %>%
  mutate(`ancient place`=ifelse(place %in% c('Kastel Sucurac / Salona',
                                             'Manastirine / Salona',
                                             'Pazdigrad / Pazdigrada / Salona',
                                             'Solin / Salona',
                                             'Vrlika / Vrlica / Salona',
                                             'Dugopolje / Salona'), 
                                'Salona', `ancient place`)) %>%
  mutate(`ancient place`=ifelse(place %in% c('Asseria',
                                             'Benkovac / Asseria',
                                             'Podgrade / Asseria'),
                                'Asseria', `ancient place`)) %>%
  mutate(`ancient place`=ifelse(place %in% c('Beli / Caisole / Crexa',
                                             'Cres / Crexa'),
                                'Crexa', `ancient place`)) %>%
  mutate(`ancient place`=ifelse(place %in% c('Biograd na Moru / Zaravecchia / Nedinum',
                                             'Nadin / Nedinum',
                                             'Skabrnja / Nedinum'),
                                'Nedinum', `ancient place`)) %>%
  mutate(`ancient place`=ifelse(place %in% c('Bastasi / Municipium Salvium',
                                             'Bosansko Grahovo / Municipium Salvium',
                                             'Glamoc / Municipium Salvium',
                                             'Glavica / Municipium Salvium','Podgradina / Municipium Salvium'),
                                'Municipium Salvium', `ancient place`)) %>%
  mutate(`ancient place`=ifelse(place %in% c('Gornji Muc / Andetrium'),
                                'Andetrium', `ancient place`)) %>%
  mutate(`ancient place`=ifelse(place %in% c('Bigeste',
                                             'Hardomilje / Bigeste',
                                             'Humac / Bigeste',
                                             'Ljubuski / Mlade / Bigeste',
                                             'Veljaci / Bigeste'),
                                'Bigeste', `ancient place`)) %>%
  mutate(`ancient place`=ifelse(place %in% c('Bihac / Raetinium',
                                             'Golubic / Raetinium',
                                             'Pritoka / Raetinium',
                                             'Ribic / Raetinium'),
                                'Raetinium', `ancient place`)) %>%
  mutate(`ancient place`=ifelse(place %in% c('Brac, Otok / Brattia',
                                             'Skrip / Brattia'),
                                'Brattia', `ancient place`)) %>%
  mutate(`ancient place`=ifelse(place %in% c('Bribir / Bribirska Glavica / Varvaria',
                                             'Lastve, Bribir'),
                                'Varvaria', `ancient place`)) %>%
  mutate(`ancient place`=ifelse(place %in% c('Budva / Buthoe'),
                                'Buthoe', `ancient place`)) %>%
  mutate(`ancient place`=ifelse(place %in% c('Cacak / Malvesiatium'),
                                'Malvesiatium', `ancient place`)) %>%
  mutate(`ancient place`=ifelse(place %in% c('Caska / Cissa'),
                                'Cissa', `ancient place`)) %>%
  mutate(`ancient place`=ifelse(place %in% c('Cavtat / Epidaurum'),
                                'Epidaurum', `ancient place`)) %>%
  mutate(`ancient place`=ifelse(place %in% c('Citluk / Aequum'),
                                'Aequum', `ancient place`)) %>%
  mutate(`ancient place`=ifelse(place %in% c('Danilo Gornje / Rider',
                                             'Sibenik / Rider',
                                             'Grusine, Sibenik'),
                                'Rider', `ancient place`)) %>%
  mutate(`ancient place`=ifelse(place %in% c('Dol / Pharia',
                                             'Hvar / Lesina / Pharia'),
                                'Pharia', `ancient place`)) %>%
  mutate(`ancient place`=ifelse(place %in% c('Donje Biljane / Iader',
                                             'Galovac / Iader',
                                             'Zadar / Iader'),
                                'Iader', `ancient place`)) %>%
  mutate(`ancient place`=ifelse(place %in% c('Dubrava / Diluntum',
                                             'Stolac / Rotimlja / Megjina / Diluntum'),
                                'Diluntum', `ancient place`)) %>%
  mutate(`ancient place`=ifelse(place %in% c('Duklja / Duklje / Rusevine / Doclea'),
                                'Doclea', `ancient place`)) %>%
  mutate(`ancient place`=ifelse(place %in% c('Gradina / Domavium'),
                                'Domavium', `ancient place`))

SumCleanerEDPlace <-  CleanerEDPlace %>% 
  group_by(`ancient place`) %>% 
  count(`ancient place`)

CleanEDPlace <- CleanerEDPlace %>%
  select(`EDCS-ID`,publication,province,`place`,`ancient place`,`dating from`,`dating to`,status,inscription,`inscription interpretive cleaning`,Latitude,Longitude) %>%
  group_by(`ancient place`)

##works, but there must be a quicker way...

## lets have a look at status
SumEDStatus <-  SelectEpigData %>% 
  group_by(status) %>% 
  count(status)
