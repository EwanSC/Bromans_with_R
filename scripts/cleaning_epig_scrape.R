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
                                             'Vrlika / Vrlica / Salona'), 
                                'Salona', `ancient place`))

SumCleanerEDPlace <-  CleanerEDPlace %>% 
  group_by(`ancient place`) %>% 
  count(`ancient place`)

CleanEDPlace <- CleanerEDPlace %>%
  select(`EDCS-ID`,publication,province,`place`,`ancient place`,`dating from`,`dating to`,status,inscription,`inscription interpretive cleaning`,Latitude,Longitude)

##works, but there must be a quicker way...

## lets have a look at status
SumEDStatus <-  SelectEpigData %>% 
  group_by(status) %>% 
  count(status)
