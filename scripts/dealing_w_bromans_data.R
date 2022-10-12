## lets try make a map with data from OC/Bromans
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


leg7_epigraphy <- read.csv('data/bromans-all_monument_with_location.csv')

str(leg7_epigraphy)

all_monuments <- na.omit(leg7_epigraphy %>%
                            select(Ancient_Site,Modern_Find_Site,LAT,LONG) %>%
                            group_by(Ancient_Site) %>%
                            count(Ancient_Site,Modern_Find_Site,LAT,LONG) %>%
                            arrange(desc(n)))

clean_all_monuments <- sqldf("SELECT * FROM all_monuments
                              WHERE LAT IS NOT NULL
                             ")

(all_monuments_ll <- st_as_sf(clean_all_monuments, coords = c('LONG', 'LAT'), remove = FALSE,
                              crs = 4326, agr = "constant"))

funerary_monuments <- sqldf("Select * from leg7_epigraphy
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
 
dal_monuments_place <- na.omit(clean_dal_monuments %>%
                            select(Site,standardised_LAT,standardised_LONG) %>%
                            group_by(Site) %>%
                            count(Site,standardised_LAT,standardised_LONG) %>%
                            arrange(desc(n))) 

(dal_monuments_ll <- st_as_sf(dal_monuments_place, coords = c('standardised_LONG', 'standardised_LAT'), remove = FALSE,
                          crs = 4326, agr = "constant"))

## plotting all monuments and funerary only in dalmatia separately

## with labels
ggplot() + 
  geom_sf(data = world, color = "darkgrey", fill = "lightgrey") + 
  geom_sf(data = roman_69_provinces, colour = 'black') +
  geom_sf(data = dal_monuments_ll, aes(size = n, color=Site), alpha=0.6) + 
  labs(size = 'Monument Density', color = 'Sites') +
  ggtitle("Monuments of Legio VII in Roman Dalmatia", subtitle = "Epigraphic Distribution") +
  coord_sf(default_crs = st_crs(4326), xlim = c(14, 20), ylim = c(42, 45.5)) +
  theme_void()

## without labels
ggplot() + 
  geom_sf(data = world, color = "darkgrey", fill = "lightgrey") + 
  geom_sf(data = roman_69_provinces, colour = 'black') +
  geom_sf(data = dal_monuments_ll, aes(size = n, color=Site), alpha=0.6) + 
  labs(size = 'Monument Density', color = 'Sites') +
  ggtitle("Monuments of Legio VII in Roman Dalmatia", subtitle = "Epigraphic Distribution") +
  coord_sf(default_crs = st_crs(4326), xlim = c(14, 20), ylim = c(42, 45.5)) +
  theme_void()

ggplot() + 
  geom_sf(data = world, color = "darkgrey", fill = "lightgrey") + 
  geom_sf(data = roman_69_provinces, colour = 'black') +
  geom_sf(data = all_monuments_ll, aes(color=Ancient_Site), alpha=0.6) + 
  labs(color = 'Sites') +
  ggtitle("Monuments of Legio VII in Dataset", subtitle = "Epigraphic Distribution") +
  coord_sf(default_crs = st_crs(4326), xlim = c(15.237922, 37.5053445), ylim = c(32.492331, 44.73291)) +
  theme_void()
