## lets try make a map with data from OC/Bromans

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


Epigraphy <- read.csv('data/bromans-all_monument_with_location.csv')

str(Epigraphy)

allmonumentsplace <- na.omit(Epigraphy %>%
                            select(Ancient_Site,LAT,LONG) %>%
                            group_by(Ancient_Site) %>%
                            count(Ancient_Site,LAT,LONG) %>%
                            arrange(desc(n))) 

(allmonumentsll <- st_as_sf(allmonumentsplace, coords = c('LONG', 'LAT'), remove = FALSE,
                          crs = 4326, agr = "constant"))

## plotting
ggplot() + 
  geom_sf(data = world, color = "darkgrey", fill = "lightgrey") + 
  geom_sf(data = roman_69_provinces, colour = 'black') +
  geom_sf(data = allmonumentsll, aes(size = n), alpha=0.6, colour = '#cd2026') + 
  labs(size = 'Inscribed Monuments') +
  ggtitle("Monuments of Legio VII in Roman Dalmatia", subtitle = "Epigraphic Distribution") +
  coord_sf(default_crs = st_crs(4326), xlim = c(14, 20), ylim = c(42, 45.5))


ggplot() + 
  geom_sf(data = world, color = "darkgrey", fill = "lightgrey") + 
  geom_sf(data = roman_69_provinces, colour = 'black') +
  geom_sf(data = allmonumentsll, aes(size = n), alpha=0.6, colour = '#cd2026') + 
  labs(size = 'Inscribed Monuments') +
  ggtitle("Monuments of Legio VII in Dataset", subtitle = "Epigraphic Distribution") +
  coord_sf(default_crs = st_crs(4326), xlim = c(15.237922, 37.5053445), ylim = c(32.492331, 44.73291))
