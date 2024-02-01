# mapping all dalmatia for csf conference
# run 2.primary_map.R first
library(sf)
library(ggplot2)
library(dplyr)

Dalmatia <- load_clean_place("data/2022-08-29-EDCS_via_Lat_Epig-prov_Dalmatia-10140.json")

Dalmatia_place <- na.omit(Dalmatia %>%
                                  select(cleaned_place,longitude,latitude) %>%
                                  group_by(cleaned_place) %>%
                                  count(cleaned_place,longitude,latitude) %>%
                                  arrange(desc(n))) 

(Dalmatia_place_ll <- st_as_sf(Dalmatia_place, coords = c('longitude', 'latitude'), remove = FALSE,
                                     crs = 4326, agr = "constant"))

ggplot() + 
  geom_sf(data = world, color = "grey", fill = "lightgrey") + 
  geom_sf(data = roman_roads, colour = 'grey30', size = 0.6) +
  geom_sf(data = roman_69_provinces, colour = 'black', size = 0.8) +
  geom_sf(data = roman_settlements, colour = 'black', alpha=0.6, size = 0.8) +
  geom_sf(data = Dalmatia_place_ll, aes(size = n), alpha=0.8, colour = 'darkorange') +
  labs(size = 'Monuments') +
  ggtitle("Epigraphic Distribution of Latin Monuments", subtitle = "Dalmatia") +
  coord_sf(default_crs = st_crs(4326), xlim = c(13, 21), ylim = c(41.5, 46)) +
  theme_void()
