#plot data on scatter maps for EDH
# started 23/10/2024
# edited 23/10/2024

# packages
library(dplyr)
library(sqldf)
library(ggplot2)
library(sf)
library(ggrepel)
library("gridExtra")
library("rnaturalearthdata")
library("rnaturalearth")
library("rnaturalearthhires")

# download rnaturalearthhires for basemap
world <- ne_countries(scale = "large", returnclass = "sf")

# adding road data from DARMC https://hub.arcgis.com/datasets/55a54a1350e14ca0b355d95633da3851_0
roman_roads <- st_read(
  "shape_files/Roman_roads.shp")

# adding settlements from Pleiades https://pleiades.stoa.org/downloads
roman_settlements <- st_read(
  "data/mapping/Roman_settlements_pleiades.gpkg")

# create df for layer of key sites 
key_sites <- data.frame(findspot_ancient_clean=c("Tilurium",
                                                 "Salona", 
                                                 "Burnum",
                                                 "Narona",
                                                 "Iader"),
                        Longitude=c(16.7216523938,
                                    16.483426,
                                    16.025622,
                                    17.598611,
                                    15.223778),
                        Latitude=c(43.609647549,
                                   43.539561,
                                   44.018914,
                                   43.046389,
                                   44.115501))

print(key_sites)

(key_sites_ll <- st_as_sf(key_sites,
                          coords = c("Longitude",
                                     "Latitude"),
                          remove = FALSE,
                          crs = 4326, agr = "constant"))

key_mil_sites <- data.frame(findspot_ancient_clean=c("Tilurium",
                                                     "Salona",
                                                     "Burnum",
                                                     "Andetrium",
                                                     "Bigeste",
                                                     "M. Malvesiatium"),
                            Longitude=c(16.7216523938,
                                        16.483426,
                                        16.025622,
                                        16.5206,
                                        17.52710,
                                        19.53330),
                            Latitude=c(43.609647549,
                                       43.539561,
                                       44.018914,
                                       43.6922,
                                       43.18180,
                                       43.96670))

print(key_mil_sites)

(key_sites_mil_ll <- st_as_sf(key_mil_sites,
                              coords = c("Longitude",
                                         "Latitude"),
                              remove = FALSE,
                              crs = 4326, agr = "constant"))

# create function for omitting nulls and organising a DF for mapping
dataframe_ll <- function(dataframe) {
  library(dplyr)
  library(stringr)
  dataframe_place <- na.omit(dataframe %>%
                               select(fo_antik,
                                       koordinaten1) %>%
                                group_by(fo_antik) %>%
                                count(fo_antik,
                                      koordinaten1) %>%
                                arrange(desc(n)))
  dataframe_place[c('latitude', 'longitude')] <- str_split_fixed(dataframe_place$koordinaten1, ',', 2)
  (dataframe_ll <- st_as_sf(dataframe_place, coords = c("longitude", "latitude"),
                            remove = FALSE,
                            crs = 4326, agr = "constant"))
  return(dataframe_ll)
}

# plot on map
EDH_Dalmatia <-
  read.csv("output_tables/corpus/EDH/EDH_Dalmatia.csv")

EDH_Dalmatia_ll <- dataframe_ll(EDH_Dalmatia)

EDH_Dalmatia_n <- count(EDH_Dalmatia)

plot1 <- 
  ggplot() + 
  geom_sf(data = world, color = "#BEBEBE", fill = "#e4e4e4") + 
  geom_sf(data = roman_roads, colour = "#4D4D4D", size = 0.6) +
  geom_sf(data = roman_settlements, colour = "#4D4D4D", alpha=0.6, size = 0.8) +
  geom_sf(data = EDH_Dalmatia_ll, aes(size = n), alpha=0.8, colour = "#3468d6") +
  geom_sf(data = key_sites_ll, colour = "#000000", size = 1) +
  geom_text_repel(data = key_sites_ll,
                  aes(x = Longitude,
                      y = Latitude,
                      label = findspot_ancient_clean), 
                  nudge_x = c(-1, -1.5, -1, -1.5, -1.5, 0.5), 
                  nudge_y = c(-0.25,-0.5,-0.25,-0.25,-0.25, 1.5)) +
  labs(size = "Density",
       caption = paste("n = ",
                       EDH_Dalmatia_n$n,
                       sep = "",
                       ".\nEpigraphic data = EDH (CC BY 4.0).\n",
                       "Roads = DARMC (CC BY-NC-SA 4.0). Settlements = Pleiades (CC-BY)."),
       title = "Distribution of all inscribed monuments",
       subtitle = "Dalmatia") +
  coord_sf(default_crs = st_crs(4326), xlim = c(14, 21), ylim = c(41.5, 46)) +
  theme_void()

plot(plot1)

ggsave("output_images/geographical_distribution/14.EDH_Dalmatia_scatter.jpeg",
       width = 180, height = 140, unit = "mm", dpi = 600)

EDH_Dalmatia_votive_epitaph <-
  read.csv("output_tables/corpus/EDH/EDH_Dalmatia_votive_epitaph.csv")

EDH_Dalmatia_votive_epitaph_ll <- dataframe_ll(EDH_Dalmatia_votive_epitaph)

EDH_Dalmatia_votive_epitaph_n <- count(EDH_Dalmatia_votive_epitaph)

plot2 <- 
  ggplot() + 
  geom_sf(data = world, color = "#BEBEBE", fill = "#e4e4e4") + 
  geom_sf(data = roman_roads, colour = "#4D4D4D", size = 0.6) +
  geom_sf(data =   roman_settlements, colour = "#6e6e6e", alpha=0.6, size = 0.8) +
  geom_sf(data = EDH_Dalmatia_votive_epitaph_ll, aes(size = n), alpha=0.8, colour = "#3468d6") +
  geom_sf(data = key_sites_ll, colour = "#000000", size = 1) +
  geom_text_repel(data = key_sites_ll,
                  aes(x = Longitude,
                      y = Latitude,
                      label = findspot_ancient_clean), 
                  nudge_x = c(-1, -1.5, -1, -1.5, -1.5, 0.5), 
                  nudge_y = c(-0.25,-0.5,-0.25,-0.25,-0.25, 1.5)) +
  labs(size = "Density", 
       caption = paste("n = ",
                       EDH_Dalmatia_votive_epitaph_n$n,
                       sep = "",
                       ".\nEpigraphic data = EDH (CC BY 4.0).\n",
                       "Roads = DARMC (CC BY-NC-SA 4.0). Settlements = Pleiades (CC-BY)."),
       title = "Distribution of funerary and sacral monuments",
       subtitle = "Dalmatia") +
  coord_sf(default_crs = st_crs(4326), xlim = c(14, 21), ylim = c(41.5, 46)) +
  theme_void()

plot(plot2)

ggsave("output_images/geographical_distribution/15.EDH_Dalmatia_funerary_sacral.jpeg",
       width = 180, height = 140, unit = "mm", dpi = 600)

EDH_Dalmatia_epitaph <-
  read.csv("output_tables/corpus/EDH/EDH_Dalmatia_epitaph.csv")

EDH_Dalmatia_epitaph_ll <- dataframe_ll(EDH_Dalmatia_epitaph)

EDH_Dalmatia_epitaph_n <- count(EDH_Dalmatia_epitaph)

plot3 <-
  ggplot() + 
  geom_sf(data = world, color = "#BEBEBE", fill = "#e4e4e4") + 
  geom_sf(data = roman_roads, colour = "#4D4D4D", size = 0.6) +
  geom_sf(data = roman_settlements, colour = "#4D4D4D", alpha=0.6, size = 0.8) +
  geom_sf(data = EDH_Dalmatia_epitaph_ll, aes(size = n), alpha=0.8, colour = "#3468d6") +
  geom_sf(data = key_sites_ll, colour = "#000000", size = 1) +
  geom_text_repel(data = key_sites_ll,
                  aes(x = Longitude,
                      y = Latitude,
                      label = findspot_ancient_clean), 
                  nudge_x = c(-1, -1.5, -1, -1.5, -1.5, 0.5), 
                  nudge_y = c(-0.25,-0.5,-0.25,-0.25,-0.25, 1.5)) +
  labs(size = "Density",
       caption = paste("n = ",
                       EDH_Dalmatia_epitaph_n$n,
                       sep = "",
                       ".\nEpigraphic data = EDH (CC BY 4.0).\n",
                       "Roads = DARMC (CC BY-NC-SA 4.0). Settlements = Pleiades (CC-BY).\n",
                       "Filtered by key words and tags."),
       title = "Distribution of funerary monuments",
       subtitle = "Dalmatia") +
  coord_sf(default_crs = st_crs(4326), xlim = c(14, 21), ylim = c(41.5, 46)) +
  theme_void()

plot(plot3)

ggsave("output_images/geographical_distribution16.EDH_Dalmatia_epitaph.jpeg",
       width = 180, height = 140, unit = "mm", dpi = 600)

## combine 1 and 3 for comparison
doubletrouble <- grid.arrange(plot1, plot3, ncol = 2)

ggsave("output_images/geographical_distribution/17.EDH_comparison.jpeg",
       doubletrouble, width = 240, height = 120, unit = "mm", dpi = 600)

doubletroubler <- grid.arrange(plot7, plot10, ncol = 2)

ggsave("output_images/geographical_distribution/18.Dalmatia_all_types_Salona_comparison.jpeg",
       doubletroubler, width = 240, height = 120, unit = "mm", dpi = 600)