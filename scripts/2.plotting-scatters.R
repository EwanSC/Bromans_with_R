#plot data on scatter maps
# started 17/09/2024
# edited 19/09/2024

#packages
library(dplyr)
library(sqldf)
library(arrow)
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
  library(sf)
  dataframe_place <- na.omit(dataframe %>%
                              select(findspot_ancient_clean,
                                     Longitude,
                                     Latitude) %>%
                               group_by(findspot_ancient_clean) %>%
                               count(findspot_ancient_clean,
                                     Longitude,
                                     Latitude) %>%
                                 arrange(desc(n)))
  (dataframe_ll <- st_as_sf(dataframe_place, coords = c("Longitude", "Latitude"),
                                  remove = FALSE,
                                  crs = 4326, agr = "constant"))
  return(dataframe_ll)
}


# plot on map
## undated/dated corpora with and without place filtration
LIRE_Dal_corpus <-
  read.csv("output_tables/corpus/LIRE_corpus.csv")
           
LIRE_Dal_corpus_ll <- dataframe_ll(LIRE_Dal_corpus)

LIRE_Dal_corpus_n <- count(LIRE_Dal_corpus)

plot1 <- 
  ggplot() + 
  geom_sf(data = world, color = "#BEBEBE", fill = "#e4e4e4") + 
  geom_sf(data = roman_roads, colour = "#4D4D4D", size = 0.6) +
  geom_sf(data = roman_settlements, colour = "#4D4D4D", alpha=0.6, size = 0.8) +
  geom_sf(data = LIRE_Dal_corpus_ll, aes(size = n), alpha=0.8, colour = "#FF8247") +
  geom_sf(data = key_sites_mil_ll, colour = "#000000", size = 1) +
  geom_text_repel(data = key_sites_mil_ll,
                  aes(x = Longitude,
                      y = Latitude,
                      label = findspot_ancient_clean), 
                  nudge_x = c(-1, -1.5, -1, -1.5, -1.5, 0.5), 
                  nudge_y = c(-0.25,-0.5,-0.25,-0.25,-0.25, 1.5)) +
  labs(size = "Density", 
       caption = paste("n = ",
                       LIRE_Dal_corpus_n$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0).\n",
                       "Roads = DARMC (CC BY-NC-SA 4.0). Settlements = Pleiades (CC-BY).\n",
                       "Filtered by key words and tags"),
                       title = "Distribution of military funerary and sacral monuments",
                       subtitle = "Dalmatia") +
  coord_sf(default_crs = st_crs(4326), xlim = c(14, 21), ylim = c(41.5, 46)) +
  theme_void()

plot(plot1)

ggsave("output_images/geographical_distribution/01.LIRE_corpus_scatter.jpeg",
       width = 180, height = 140, unit = "mm", dpi = 600)

LIRE_Dal_corpus_place_filtering <-
  read.csv("output_tables/corpus/LIRE_corpus_place_filter.csv")

LIRE_Dal_corpus_place_filtering_ll <- dataframe_ll(LIRE_Dal_corpus_place_filtering)

LIRE_Dal_corpus_place_filtering_n <- count(LIRE_Dal_corpus_place_filtering)

plot2 <- 
  ggplot() + 
  geom_sf(data = world, color = "#BEBEBE", fill = "#e4e4e4") + 
  geom_sf(data = roman_roads, colour = "#4D4D4D", size = 0.6) +
  geom_sf(data =   roman_settlements, colour = "#6e6e6e", alpha=0.6, size = 0.8) +
  geom_sf(data = LIRE_Dal_corpus_place_filtering_ll, aes(size = n), alpha=0.8, colour = "#FF8247") +
  geom_sf(data = key_sites_mil_ll, colour = "#000000", size = 1) +
  geom_text_repel(data = key_sites_mil_ll,
                  aes(x = Longitude,
                      y = Latitude,
                      label = findspot_ancient_clean), 
                  nudge_x = c(-1, -1.5, -1, -1.5, -1.5, 0.5), 
                  nudge_y = c(-0.25,-0.5,-0.25,-0.25,-0.25, 1.5)) +
  labs(size = "Density", 
       caption = paste("n = ",
                       LIRE_Dal_corpus_place_filtering_n$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0).\n",
                       "Roads = DARMC (CC BY-NC-SA 4.0). Settlements = Pleiades (CC-BY).\n",
                       "Filtered by key words, tags, and places."),
                      title = "Distribution of military funerary and sacral monuments",
                      subtitle = "Dalmatia") +
  coord_sf(default_crs = st_crs(4326), xlim = c(14, 21), ylim = c(41.5, 46)) +
  theme_void()

plot(plot2)

ggsave("output_images/geographical_distribution/02.LIRE_corpus_place_filter_scatter.jpeg",
       width = 180, height = 140, unit = "mm", dpi = 600)

## create one for just relevant period (~200 CE cut off)
LIRE_corpus_dated <-
  read.csv("output_tables/corpus/LIRE_corpus_dated.csv")

LIRE_corpus_dated_ll <- dataframe_ll(LIRE_corpus_dated)

LIRE_corpus_dated_n <- count(LIRE_corpus_dated)

plot3 <-
  ggplot() + 
  geom_sf(data = world, color = "#BEBEBE", fill = "#e4e4e4") + 
  geom_sf(data = roman_roads, colour = "#4D4D4D", size = 0.6) +
  geom_sf(data = roman_settlements, colour = "#4D4D4D", alpha=0.6, size = 0.8) +
  geom_sf(data = LIRE_corpus_dated_ll, aes(size = n), alpha=0.8, colour = "#FF8247") +
  geom_sf(data = key_sites_mil_ll, colour = "#000000", size = 1) +
  geom_text_repel(data = key_sites_mil_ll,
                  aes(x = Longitude,
                      y = Latitude,
                      label = findspot_ancient_clean), 
                  nudge_x = c(-1, -1.5, -1, -1.5, -1.5, 0.5), 
                  nudge_y = c(-0.25,-0.5,-0.25,-0.25,-0.25, 1.5)) +
  labs(size = "Density",
       caption = paste("n = ",
                       LIRE_corpus_dated_n$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0).\n",
                       "Roads = DARMC (CC BY-NC-SA 4.0). Settlements = Pleiades (CC-BY).\n",
                       "Filtered by key words and tags."),
                       title = "Distribution of military funerary and sacral monuments",
                       subtitle = "Dalmatia: Julio-Claudians to Antonines") +
  coord_sf(default_crs = st_crs(4326), xlim = c(14, 21), ylim = c(41.5, 46)) +
  theme_void()

plot(plot3)

ggsave("output_images/geographical_distribution/03.LIRE_corpus_dated_scatter.jpeg",
       width = 180, height = 140, unit = "mm", dpi = 600)

LIRE_corpus_dated_place_filtering <-
  read.csv("output_tables/corpus/LIRE_corpus_dated_place_filter.csv")

LIRE_corpus_dated_place_filtering_ll <- dataframe_ll(LIRE_corpus_dated_place_filtering)

LIRE_corpus_dated_place_filtering_n <- count(LIRE_corpus_dated_place_filtering)

plot4 <-
  ggplot() + 
  geom_sf(data = world, color = "#BEBEBE", fill = "#e4e4e4") + 
  geom_sf(data = roman_roads, colour = "#4D4D4D", size = 0.6) +
  geom_sf(data = roman_settlements, colour = "#4D4D4D", alpha=0.6, size = 0.8) +
  geom_sf(data = LIRE_corpus_dated_place_filtering_ll, aes(size = n), alpha=0.8, colour = "#FF8247") +
  geom_sf(data = key_sites_mil_ll, colour = "#000000", size = 1) +
  geom_text_repel(data = key_sites_mil_ll,
                  aes(x = Longitude,
                      y = Latitude,
                      label = findspot_ancient_clean), 
                  nudge_x = c(-1, -1.5, -1, -1.5, -1.5, 0.5), 
                  nudge_y = c(-0.25,-0.5,-0.25,-0.25,-0.25, 1.5)) +
  labs(size = "Density",
       caption = paste("n = ",
                       LIRE_corpus_dated_place_filtering_n$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0).\n",
                       "Roads = DARMC (CC BY-NC-SA 4.0). Settlements = Pleiades (CC-BY).\n",
                       "Filtered by key words, tags, and places."),
                       title = "Distribution of military funerary and sacral monuments",
                       subtitle = "Dalmatia: Julio-Claudians to Antonines") +
  coord_sf(default_crs = st_crs(4326), xlim = c(14, 21), ylim = c(41.5, 46)) +
  theme_void()

plot(plot4)

ggsave("output_images/geographical_distribution/04.LIRE_corpus_dated_place_filter_scatter.jpeg",
       width = 180, height = 140, unit = "mm", dpi = 600)

#save csv of plotted places
LIRE_Dal_corpus_place <- st_drop_geometry(LIRE_Dal_corpus_ll)
write.csv(LIRE_Dal_corpus_place,
          file = "output_tables/corpus/places/LIRE_corpus_places.csv")
LIRE_Dal_corpus_place_filtering_place <- st_drop_geometry(LIRE_Dal_corpus_place_filtering_ll)
write.csv(LIRE_Dal_corpus_place_filtering_place,
          file = "output_tables/corpus/places/LIRE_corpus_place_filter_places.csv")          
LIRE_corpus_dated_place <- st_drop_geometry(LIRE_corpus_dated_ll)
write.csv(LIRE_corpus_dated_place,
          file = "output_tables/corpus/places/LIRE_corpus_dated.csv")
LIRE_corpus_dated_place_filtering_place <- st_drop_geometry(LIRE_corpus_dated_place_filtering_ll)
write.csv(LIRE_corpus_dated_place_filtering_place,
          file = "output_tables/corpus/places/LIRE_corpus_dated_place_filter.csv")

# Now compare to all Dalmatia
LIRE_Dal <-
  read.csv("output_tables/corpus/LIRE_Dalmatia.csv")

LIRE_Dal_ll <- dataframe_ll(LIRE_Dal)

LIRE_Dal_n <- count(LIRE_Dal)

plot5 <-
  ggplot() + 
  geom_sf(data = world, color = "#BEBEBE", fill = "#e4e4e4") + 
  geom_sf(data = roman_roads, colour = "#4D4D4D", size = 0.6) +
  geom_sf(data = roman_settlements, colour = "#6e6e6e", alpha=0.6, size = 0.8) +
  geom_sf(data = LIRE_Dal_ll, aes(size = n), alpha=0.8, colour = "#3468d6") +
  geom_sf(data = key_sites_ll, colour = "#000000", size = 1) +
  geom_text_repel(data = key_sites_ll,
                  aes(x = Longitude,
                      y = Latitude,
                      label = findspot_ancient_clean), 
                  nudge_x = c(-2, -1.5, -1, -1, -1), 
                  nudge_y = c(-0.25,-0.25,-0.25, -0.25,-0.25)) +
  labs(size = "Density",
       caption = paste("n = ",
                       LIRE_Dal_n$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0).\n",
                       "Roads = DARMC (CC BY-NC-SA 4.0). Settlements = Pleiades (CC-BY)."),
                       title = "Distribution of funerary and sacral monuments",
                       subtitle = "Dalmatia") +
  coord_sf(default_crs = st_crs(4326), xlim = c(14, 21), ylim = c(41.5, 46)) +
  theme_void()

plot(plot5)

ggsave("output_images/geographical_distribution/05.LIRE_Dalmatia_scatter.jpeg",
       width = 180, height = 140, unit = "mm", dpi = 600)

LIRE_Dal_dated <-
  read.csv("output_tables/corpus/LIRE_Dalmatia_dated.csv")

LIRE_Dal_dated_ll <- dataframe_ll(LIRE_Dal_dated)

LIRE_Dal_dated_n <- count(LIRE_Dal_dated)

plot6 <-
  ggplot() + 
  geom_sf(data = world, color = "#BEBEBE", fill = "#e4e4e4") + 
  geom_sf(data = roman_roads, colour = "#4D4D4D", size = 0.6) +
  geom_sf(data = roman_settlements, colour = "#6e6e6e", alpha=0.6, size = 0.8) +
  geom_sf(data = LIRE_Dal_dated_ll, aes(size = n), alpha=0.8, colour = "#3468d6") +
  geom_sf(data = key_sites_ll, colour = "#000000", size = 1) +
  geom_text_repel(data = key_sites_ll, aes(x = Longitude,
                                           y = Latitude,
                                           label = findspot_ancient_clean), 
                  nudge_x = c(-2, -1.5, -1, -1, -1), 
                  nudge_y = c(-0.25,-0.25,-0.25, -0.25,-0.25)) +
  labs(size = "Density",
       caption = paste("n = ",
                       LIRE_Dal_dated_n$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0).\n",
                       "Roads = DARMC (CC BY-NC-SA 4.0). Settlements = Pleiades (CC-BY)."),
                       title = "Distribution of funerary and sacral monuments",
                       subtitle = "Dalmatia: Julio-Claudians to Antonines") +
  coord_sf(default_crs = st_crs(4326), xlim = c(14, 21), ylim = c(41.5, 46)) +
  theme_void()

plot(plot6)

ggsave("output_images/geographical_distribution/06.LIRE_Dalmatia_dated_scatter.jpeg",
       width = 180, height = 140, unit = "mm", dpi = 600)

# now to save plotted places
LIRE_Dal_place <- st_drop_geometry(LIRE_Dal_ll)
write.csv(LIRE_Dal_place,
          file = "output_tables/corpus/places/LIRE_Dalmatia_places.csv")
LIRE_Dal_dated_place <- st_drop_geometry(LIRE_Dal_dated_ll)
write.csv(LIRE_Dal_dated_place,
          file = "output_tables/corpus/places/LIRE_Dalmatia_dated_places.csv")

# now for mapping without Salona
LIRE_Dal_no_salona <-
  read.csv("output_tables/corpus/LIRE_Dalmatia_no_salona.csv")

LIRE_Dal_no_salona_ll <- dataframe_ll(LIRE_Dal_no_salona)

LIRE_Dal_no_salona_n <- count(LIRE_Dal_no_salona)

plot7 <-
  ggplot() + 
  geom_sf(data = world, color = "#BEBEBE", fill = "#e4e4e4") + 
  geom_sf(data = roman_roads, colour = "#4D4D4D", size = 0.6) +
  geom_sf(data = roman_settlements, colour = "#6e6e6e", alpha=0.6, size = 0.8) +
  geom_sf(data = LIRE_Dal_no_salona_ll, aes(size = n), alpha=0.8, colour = "#3468d6") +
  geom_sf(data = key_sites_ll, colour = "#000000", size = 1) +
  geom_text_repel(data = key_sites_ll, aes(x = Longitude,
                                           y = Latitude,
                                           label = findspot_ancient_clean), 
                  nudge_x = c(-2, -1.5, -1, -1, -1), 
                  nudge_y = c(-0.25,-0.25,-0.25, -0.25,-0.25)) +
  labs(size = "Density",
       caption = paste("n = ",
                       LIRE_Dal_no_salona_n$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0).\n",
                       "Roads = DARMC (CC BY-NC-SA 4.0). Settlements = Pleiades (CC-BY)."),
                       title = "Distribution of funerary and sacral monuments",
                       subtitle = "Dalmatia (Outside Salona)") +
  coord_sf(default_crs = st_crs(4326), xlim = c(14, 21), ylim = c(41.5, 46)) +
  theme_void()

plot(plot7)

ggsave("output_images/geographical_distribution/07.LIRE_Dalmatia_no_salona_scatter.jpeg",
       width = 180, height = 140, unit = "mm", dpi = 600)

LIRE_Dal_corpus_no_salona <-
  read.csv("output_tables/corpus/LIRE_corpus_no_salona.csv")

LIRE_Dal_corpus_no_salona_ll <- dataframe_ll(LIRE_Dal_corpus_no_salona)

LIRE_Dal_corpus_no_salona_n <- count(LIRE_Dal_corpus_no_salona)

plot8 <-
  ggplot() + 
  geom_sf(data = world, color = "#BEBEBE", fill = "#e4e4e4") + 
  geom_sf(data = roman_roads, colour = "#4D4D4D", size = 0.6) +
  geom_sf(data = roman_settlements, colour = "#6e6e6e", alpha=0.6, size = 0.8) +
  geom_sf(data = LIRE_Dal_corpus_no_salona_ll, aes(size = n), alpha=0.8, colour = "#FF8247") +
  geom_sf(data = key_sites_mil_ll, colour = "#000000", size = 1) +
  geom_text_repel(data = key_sites_mil_ll,
                  aes(x = Longitude,
                      y = Latitude,
                      label = findspot_ancient_clean), 
                  nudge_x = c(-1, -1.5, -1, -1.5, -1.5, 0.5), 
                  nudge_y = c(-0.25,-0.5,-0.25,-0.25,-0.25, 1.5)) +
  labs(size = "Density",
       caption = paste("n = ",
                       LIRE_Dal_corpus_no_salona_n$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0).\n",
                       "Roads = DARMC (CC BY-NC-SA 4.0). Settlements = Pleiades (CC-BY).\n",
                       "Filtered by key words and tags."),
                       title = "Distribution of military funerary and sacral monuments",
                       subtitle = "Dalmatia (Outside Salona)") +
  coord_sf(default_crs = st_crs(4326), xlim = c(14, 21), ylim = c(41.5, 46)) +
  theme_void()

plot(plot8)

ggsave("output_images/geographical_distribution/08.LIRE_corpus_no_salona_scatter.jpeg",
       width = 180, height = 140, unit = "mm", dpi = 600)

LIRE_Dal_dated_no_salona <-
  read.csv("output_tables/corpus/LIRE_Dalmatia_dated_no_salona.csv")

LIRE_Dal_dated_no_salona_ll <- dataframe_ll(LIRE_Dal_dated_no_salona)

LIRE_Dal_dated_no_salona_n <- count(LIRE_Dal_dated_no_salona)

plot9 <-
  ggplot() + 
  geom_sf(data = world, color = "#BEBEBE", fill = "#e4e4e4") + 
  geom_sf(data = roman_roads, colour = "#4D4D4D", size = 0.6) +
  geom_sf(data = roman_settlements, colour = "#6e6e6e", alpha=0.6, size = 0.8) +
  geom_sf(data = LIRE_Dal_dated_no_salona_ll, aes(size = n), alpha=0.8, colour = "#3468d6") +
  geom_sf(data = key_sites_ll, colour = "#000000", size = 1) +
  geom_text_repel(data = key_sites_ll, aes(x = Longitude,
                                           y = Latitude,
                                           label = findspot_ancient_clean), 
                  nudge_x = c(-2, -1.5, -1, -1, -1), 
                  nudge_y = c(-0.25,-0.25,-0.25, -0.25,-0.25)) +
  labs(size = "Density",
       caption = paste("n = ",
                       LIRE_Dal_dated_no_salona_n$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0).\n",
                       "Roads = DARMC (CC BY-NC-SA 4.0). Settlements = Pleiades (CC-BY)."),
       title = "Distribution of funerary and sacral monuments",
       subtitle = "Dalmatia (Outside Salona): Julio-Claudians to Antonines") +
  coord_sf(default_crs = st_crs(4326), xlim = c(14, 21), ylim = c(41.5, 46)) +
  theme_void()

plot(plot9)

ggsave("output_images/geographical_distribution/09.LIRE_Dalmatia_dated_no_salona_scatter.jpeg",
       width = 180, height = 140, unit = "mm", dpi = 600)

LIRE_Dal_corpus_dated_no_salona <-
  read.csv("output_tables/corpus/LIRE_corpus_dated_no_salona.csv")

LIRE_Dal_corpus_dated_no_salona_ll <- dataframe_ll(LIRE_Dal_corpus_dated_no_salona)

LIRE_Dal_corpus_dated_no_salona_n <- count(LIRE_Dal_corpus_dated_no_salona)

plot10 <-
  ggplot() + 
  geom_sf(data = world, color = "#BEBEBE", fill = "#e4e4e4") + 
  geom_sf(data = roman_roads, colour = "#4D4D4D", size = 0.6) +
  geom_sf(data = roman_settlements, colour = "#6e6e6e", alpha=0.6, size = 0.8) +
  geom_sf(data = LIRE_Dal_corpus_dated_no_salona_ll, aes(size = n), alpha=0.8, colour = "#FF8247") +
  geom_sf(data = key_sites_mil_ll, colour = "#000000", size = 1) +
  geom_text_repel(data = key_sites_mil_ll,
                  aes(x = Longitude,
                      y = Latitude,
                      label = findspot_ancient_clean), 
                  nudge_x = c(-1, -1.5, -1, -1.5, -1.5, 0.5), 
                  nudge_y = c(-0.25,-0.5,-0.25,-0.25,-0.25, 1.5)) +
  labs(size = "Density",
       caption = paste("n = ",
                       LIRE_Dal_corpus_dated_no_salona_n$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0).\n",
                       "Roads = DARMC (CC BY-NC-SA 4.0). Settlements = Pleiades (CC-BY).\n",
                       "Filtered by key words and tags."),
                       title = "Distribution of military funerary and sacral monuments",
                       subtitle = "Dalmatia (Outside Salona): Julio-Claudians to Antonines") +
  coord_sf(default_crs = st_crs(4326), xlim = c(14, 21), ylim = c(41.5, 46)) +
  theme_void()

plot(plot10)

ggsave("output_images/geographical_distribution/10.LIRE_corpus_dated_no_salona_scatter.jpeg",
       width = 180, height = 140, unit = "mm", dpi = 600)