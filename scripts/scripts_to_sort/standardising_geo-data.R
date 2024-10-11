#Trying to standardize lat and long and clena data to help with mapping
# cleaning dataset for preparation for mapping
## created 12/10/2022
## E Coopey

library(tibble)
library(data.table)
library(dplyr)
library(sqldf)
library(ggplot2)
library(ggrepel)
library(sf)
library("rnaturalearthdata")
library("rnaturalearth")
library("rnaturalearthhires")


epigraphy <- read.csv('data/bromans-all_monument_with_location.csv')

str(epigraphy)

funerary_monuments <- sqldf("Select * from epigraphy
                            WHERE MonumentType = 'stela'
                            AND Mention_Legio_VII != 'no'
                            AND LAT IS NOT NULL
                            OR MonumentType = 'funerary inscription'
                            OR MonumentType = 'titulus'
                            OR MonumentType = 'inscription fragment'
                            OR MonumentType = 'sacral altar'
                            OR MonumentType = 'sacral monument'
                            ")

dal_monuments <- sqldf("Select * from funerary_monuments
                       WHERE Roman_Province = 'Dalmatia'
                       ")

# now to use modern names for sites with no ancient name
# by making 'sites' column and then standardising 
# lat and long in new standardized columns

clean_dal_monuments <- dal_monuments %>%
  mutate(`Site`=ifelse(Modern_Find_Site %in% c('Dugopolje'),
                       'Dugopolje', Ancient_Site)) %>%
  mutate(`Site`=ifelse(Modern_Find_Site %in% c('Dicmo'),
                       'Dicmo', `Site`)) %>%
  mutate(`standardised_LONG`=ifelse(Ancient_Site %in% c('Salona'),
                                    '16.48343', `LONG`)) %>%
  mutate(`standardised_LAT`=ifelse(Ancient_Site %in% c('Salona'),
                                   '43.53956', `LAT`)) %>%
  mutate(`standardised_LONG`=ifelse(Ancient_Site %in% c('Aequum'),
                                    '16.62110', `standardised_LONG`)) %>%
  mutate(`standardised_LAT`=ifelse(Ancient_Site %in% c('Aequum'),
                                   '43.75519', `standardised_LAT`)) %>%
  mutate(`standardised_LONG`=ifelse(Ancient_Site %in% c('Narona'),
                                    '17.62465', `standardised_LONG`)) %>%
  mutate(`standardised_LAT`=ifelse(Ancient_Site %in% c('Narona'),
                                   '43.08130', `standardised_LAT`)) %>%
  mutate(`standardised_LONG`=ifelse(Ancient_Site %in% c('Pagus Scunasticus'),
                                    '17.54475', `standardised_LONG`)) %>%
  mutate(`standardised_LAT`=ifelse(Ancient_Site %in% c('Pagus Scunasticus'),
                                   '43.15910', `standardised_LAT`)) %>%
  mutate(`standardised_LONG`=ifelse(Ancient_Site %in% c('Tilurium'),
                                    '16.71477', `standardised_LONG`)) %>%
  mutate(`standardised_LAT`=ifelse(Ancient_Site %in% c('Tilurium'),
                                   '43.61201', `standardised_LAT`))

# now to prepare for mapping

dal_monuments_place <- na.omit(clean_dal_monuments %>%
                                 select(Site,standardised_LAT,standardised_LONG) %>%
                                 group_by(Site) %>%
                                 count(Site,standardised_LAT,standardised_LONG) %>%
                                 arrange(desc(n))) 

(dal_monuments_ll <- st_as_sf(dal_monuments_place, coords = c('standardised_LONG', 'standardised_LAT'), remove = FALSE,
                              crs = 4326, agr = "constant"))