#plot data on scatter maps
# started 17/09/2024
# edited 19/09/2024

# FIRST RUN /scripts/1.map.R and /scripts/2.corpus_LIRE.R

#packages
library(dplyr)
library(sqldf)
library(arrow)
library(ggplot2)
library(sf)
library(ggrepel)
library("gridExtra")

# create df for layer of key sites 
key_sites <- data.frame(findspot_ancient_clean=c("Tilurium",
                                                 "Salona", 
                                                 "Burnum"),
                        Longitude=c(16.7216523938 , 16.483426 , 16.025622),
                        Latitude=c(43.609647549, 43.539561, 44.018914))

print(key_sites)

(key_sites_ll <- st_as_sf(key_sites,
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
LIRE_Dal_corpus_ll <- dataframe_ll(LIRE_Dal_corpus)

LIRE_Dal_corpus_n <- count(LIRE_Dal_corpus)

plot1 <- 
  ggplot() + 
  geom_sf(data = world, color = "grey", fill = "lightgrey") + 
  geom_sf(data = roman_roads, colour = "grey30", size = 0.6) +
  geom_sf(data = roman_69_provinces, colour = "black", size = 0.8) +
  geom_sf(data = roman_settlements, colour = "grey30", alpha=0.6, size = 0.8) +
  geom_sf(data = LIRE_Dal_corpus_ll, aes(size = n), alpha=0.8, colour = "#FF8247") +
  geom_sf(data = key_sites_ll, colour = "black", size = 1) +
  geom_text_repel(data = key_sites_ll,
                  aes(x = Longitude,
                      y = Latitude,
                      label = findspot_ancient_clean), 
                  nudge_x = c(-2, -1.5, -1), 
                  nudge_y = c(-0.25,-0.25,-0.25)) +
  labs(size = "Density", 
       caption = paste("n = ",
                       LIRE_Dal_corpus_n$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0) (Province = Dalmatia).\n",
                       "Roads = DARMC (CC BY-NC-SA 4.0). Provinces = AWMC (ODbL).\nSettlements = Pleiades (CC-BY).\n",
                       "Filtered by key words, tags, and places."),
                       title = "Distribution of military funerary and sacral monuments",
                       subtitle = "Dalmatia") +
  coord_sf(default_crs = st_crs(4326), xlim = c(13, 21), ylim = c(41.5, 46)) +
  theme_void()

plot(plot1)

ggsave("output_images/geographical_distribution/LIRE_corpus_scatter.jpeg",
       width = 180, height = 140, unit = "mm", dpi = 600)

LIRE_Dal_corpus_no_place_filtering_ll <- dataframe_ll(LIRE_Dal_corpus_no_place_filtering)

LIRE_Dal_corpus_no_place_filtering_n <- count(LIRE_Dal_corpus_no_place_filtering)

plot2 <- 
  ggplot() + 
  geom_sf(data = world, color = "grey", fill = "lightgrey") + 
  geom_sf(data = roman_roads, colour = "grey30", size = 0.6) +
  geom_sf(data = roman_69_provinces, colour = "black", size = 0.8) +
  geom_sf(data = roman_settlements, colour = "black", alpha=0.6, size = 0.8) +
  geom_sf(data = LIRE_Dal_corpus_no_place_filtering_ll, aes(size = n), alpha=0.8, colour = "#FF8247") +
  geom_sf(data = key_sites_ll, colour = "black", size = 1) +
  geom_text_repel(data = key_sites_ll, aes(x = Longitude,
                                           y = Latitude,
                                           label = findspot_ancient_clean), 
                  nudge_x = c(-2, -1.5, -1), 
                  nudge_y = c(-0.25,-0.25,-0.25)) +
  labs(size = "Density", 
       caption = paste("n = ",
                       LIRE_Dal_corpus_no_place_filtering_n$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0) (Province = Dalmatia).\n",
                       "Roads = DARMC (CC BY-NC-SA 4.0). Provinces = AWMC (ODbL).\nSettlements = Pleiades (CC-BY).\n",
                       "Filtered by key words and tags."),
                      title = "Distribution of military funerary and sacral monuments",
                      subtitle = "Dalmatia") +
  coord_sf(default_crs = st_crs(4326), xlim = c(13, 21), ylim = c(41.5, 46)) +
  theme_void()

plot(plot2)

ggsave("output_images/geographical_distribution/LIRE_corpus_scatter_no_place_filter.jpeg",
       width = 180, height = 140, unit = "mm", dpi = 600)

LIRE_Dal_corpus_clean_ll <- dataframe_ll(LIRE_Dal_corpus_clean)

LIRE_Dal_corpus_clean_n <- count(LIRE_Dal_corpus_clean)

plot3 <- 
  ggplot() + 
  geom_sf(data = world, color = "grey", fill = "lightgrey") + 
  geom_sf(data = roman_roads, colour = "grey30", size = 0.6) +
  geom_sf(data = roman_69_provinces, colour = "black", size = 0.8) +
  geom_sf(data = roman_settlements, colour = "black", alpha=0.6, size = 0.8) +
  geom_sf(data = LIRE_Dal_corpus_clean_ll, aes(size = n), alpha=0.8, colour = "#FF8247") +
  geom_sf(data = key_sites_ll, colour = "black", size = 1) +
  geom_text_repel(data = key_sites_ll, aes(x = Longitude,
                                           y = Latitude,
                                           label = findspot_ancient_clean), 
                  nudge_x = c(-2, -1.5, -1), 
                  nudge_y = c(-0.25,-0.25,-0.25)) +
  labs(size = "Density", 
       caption = paste("n = ",
                       LIRE_Dal_corpus_clean_n$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0) (Clean province = Dalmatia).\n",
                       "Roads = DARMC (CC BY-NC-SA 4.0). Provinces = AWMC (ODbL).\nSettlements = Pleiades (CC-BY).\n",
                       "Filtered by key words, tags, and places."),
                      title = "Distribution of military funerary and sacral monuments",
                      subtitle = "Dalmatia") +
  coord_sf(default_crs = st_crs(4326), xlim = c(13, 21), ylim = c(41.5, 46)) +
  theme_void()

plot(plot3)

ggsave("output_images/geographical_distribution/LIRE_clean_corpus_scatter.jpeg",
       width = 180, height = 140, unit = "mm", dpi = 600)

LIRE_Dal_corpus_no_place_filtering_clean_ll <- dataframe_ll(LIRE_Dal_corpus_no_place_filtering_clean)

LIRE_Dal_corpus_no_place_filtering_clean_n <- count(LIRE_Dal_corpus_no_place_filtering_clean)

plot4 <- 
  ggplot() + 
  geom_sf(data = world, color = "grey", fill = "lightgrey") + 
  geom_sf(data = roman_roads, colour = "grey30", size = 0.6) +
  geom_sf(data = roman_69_provinces, colour = "black", size = 0.8) +
  geom_sf(data = roman_settlements, colour = "black", alpha=0.6, size = 0.8) +
  geom_sf(data = LIRE_Dal_corpus_no_place_filtering_clean_ll, aes(size = n), alpha=0.8, colour = "#FF8247") +
  geom_sf(data = key_sites_ll, colour = "black", size = 1) +
  geom_text_repel(data = key_sites_ll, aes(x = Longitude,
                                           y = Latitude,
                                           label = findspot_ancient_clean), 
                  nudge_x = c(-2, -1.5, -1), 
                  nudge_y = c(-0.25,-0.25,-0.25)) +
  labs(size = "Density", 
       caption = paste("n = ",
                       LIRE_Dal_corpus_no_place_filtering_clean_n$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0) (Clean province = Dalmatia).\n",
                       "Roads = DARMC (CC BY-NC-SA 4.0). Provinces = AWMC (ODbL).\nSettlements = Pleiades (CC-BY).\n",
                       "Filtered by key words and tags."),
                      title = "Distribution of military funerary and sacral monuments",
                      subtitle = "Dalmatia") +
  coord_sf(default_crs = st_crs(4326), xlim = c(13, 21), ylim = c(41.5, 46)) +
  theme_void()

plot(plot4)

ggsave("output_images/geographical_distribution/LIRE_clean_corpus_scatter_no_place_filter.jpeg",
       width = 180, height = 140, unit = "mm", dpi = 600)

## create one for just relevant period (~200 CE cut off)
LIRE_dated_corpus_ll <- dataframe_ll(LIRE_dated_corpus)

LIRE_dated_corpus_n <- count(LIRE_dated_corpus)

plot5 <-
  ggplot() + 
  geom_sf(data = world, color = "grey", fill = "lightgrey") + 
  geom_sf(data = roman_roads, colour = "grey30", size = 0.6) +
  geom_sf(data = roman_69_provinces, colour = "black", size = 0.8) +
  geom_sf(data = roman_settlements, colour = "grey30", alpha=0.6, size = 0.8) +
  geom_sf(data = LIRE_dated_corpus_ll, aes(size = n), alpha=0.8, colour = "#FF8247") +
  geom_sf(data = key_sites_ll, colour = "black", size = 1) +
  geom_text_repel(data = key_sites_ll, aes(x = Longitude,
                                           y = Latitude,
                                           label = findspot_ancient_clean), 
                  nudge_x = c(-2, -1.5, -1), 
                  nudge_y = c(-0.25,-0.25,-0.25)) +
  labs(size = "Density",
       caption = paste("n = ",
                       LIRE_dated_corpus_n$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0) (Province = Dalmatia).\n",
                       "Roads = DARMC (CC BY-NC-SA 4.0). Provinces = AWMC (ODbL).\nSettlements = Pleiades (CC-BY).\n",
                       "Filtered by key words, tags, and places."),
                       title = "Distribution of military funerary and sacral monuments",
                       subtitle = "Dalmatia: Julio-Claudians to Antonines") +
  coord_sf(default_crs = st_crs(4326), xlim = c(13, 21), ylim = c(41.5, 46)) +
  theme_void()

plot(plot5)

ggsave("output_images/geographical_distribution/LIRE_dated_corpus_scatter.jpeg",
       width = 180, height = 140, unit = "mm", dpi = 600)

LIRE_dated_corpus_no_place_filtering_ll <- dataframe_ll(LIRE_dated_corpus_no_place_filtering)

LIRE_dated_corpus_no_place_filtering_n <- count(LIRE_dated_corpus_no_place_filtering)

plot6 <-
  ggplot() + 
  geom_sf(data = world, color = "grey", fill = "lightgrey") + 
  geom_sf(data = roman_roads, colour = "grey30", size = 0.6) +
  geom_sf(data = roman_69_provinces, colour = "black", size = 0.8) +
  geom_sf(data = roman_settlements, colour = "grey30", alpha=0.6, size = 0.8) +
  geom_sf(data = LIRE_dated_corpus_no_place_filtering_ll, aes(size = n), alpha=0.8, colour = "#FF8247") +
  geom_sf(data = key_sites_ll, colour = "black", size = 1) +
  geom_text_repel(data = key_sites_ll, aes(x = Longitude,
                                           y = Latitude,
                                           label = findspot_ancient_clean), 
                  nudge_x = c(-2, -1.5, -1), 
                  nudge_y = c(-0.25,-0.25,-0.25)) +
  labs(size = "Density",
       caption = paste("n = ",
                       LIRE_dated_corpus_no_place_filtering_n$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0) (Province = Dalmatia).\n",
                       "Roads = DARMC (CC BY-NC-SA 4.0). Provinces = AWMC (ODbL).\nSettlements = Pleiades (CC-BY).\n",
                       "Filtered by key words and tags."),
                       title = "Distribution of military funerary and sacral monuments",
                       subtitle = "Dalmatia: Julio-Claudians to Antonines") +
  coord_sf(default_crs = st_crs(4326), xlim = c(13, 21), ylim = c(41.5, 46)) +
  theme_void()

plot(plot6)

ggsave("output_images/geographical_distribution/LIRE_dated_corpus_scatter_no_place_filter.jpeg",
       width = 180, height = 140, unit = "mm", dpi = 600)

LIRE_dated_corpus_clean_ll <- dataframe_ll(LIRE_dated_corpus_clean)

LIRE_dated_corpus_clean_n <- count(LIRE_dated_corpus_clean)

plot7 <-
  ggplot() + 
  geom_sf(data = world, color = "grey", fill = "lightgrey") + 
  geom_sf(data = roman_roads, colour = "grey30", size = 0.6) +
  geom_sf(data = roman_69_provinces, colour = "black", size = 0.8) +
  geom_sf(data = roman_settlements, colour = "grey30", alpha=0.6, size = 0.8) +
  geom_sf(data = LIRE_dated_corpus_clean_ll, aes(size = n), alpha=0.8, colour = "#FF8247") +
  geom_sf(data = key_sites_ll, colour = "black", size = 1) +
  geom_text_repel(data = key_sites_ll, aes(x = Longitude,
                                           y = Latitude,
                                           label = findspot_ancient_clean), 
                  nudge_x = c(-2, -1.5, -1), 
                  nudge_y = c(-0.25,-0.25,-0.25)) +
  labs(size = "Density",
       caption = paste("n = ",
                       LIRE_dated_corpus_clean_n$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0) (Clean province = Dalmatia).\n",
                       "Roads = DARMC (CC BY-NC-SA 4.0). Provinces = AWMC (ODbL).\nSettlements = Pleiades (CC-BY).\n",
                       "Filtered by key words, tags, and places."),
                       title = "Distribution of military funerary and sacral monuments",
                       subtitle = "Dalmatia: Julio-Claudians to Antonines") +
  coord_sf(default_crs = st_crs(4326), xlim = c(13, 21), ylim = c(41.5, 46)) +
  theme_void()

plot(plot7)

ggsave("output_images/geographical_distribution/LIRE_clean_dated_corpus_scatter.jpeg",
       width = 180, height = 140, unit = "mm", dpi = 600)

LIRE_dated_corpus_no_place_filtering_clean_ll <- dataframe_ll(LIRE_dated_corpus_no_place_filtering_clean)

LIRE_dated_corpus_no_place_filtering_clean_n <- count(LIRE_dated_corpus_no_place_filtering_clean)

plot8 <-
  ggplot() + 
  geom_sf(data = world, color = "grey", fill = "lightgrey") + 
  geom_sf(data = roman_roads, colour = "grey30", size = 0.6) +
  geom_sf(data = roman_69_provinces, colour = "black", size = 0.8) +
  geom_sf(data = roman_settlements, colour = "grey30", alpha=0.6, size = 0.8) +
  geom_sf(data = LIRE_dated_corpus_no_place_filtering_clean_ll, aes(size = n), alpha=0.8, colour = "#FF8247") +
  geom_sf(data = key_sites_ll, colour = "black", size = 1) +
  geom_text_repel(data = key_sites_ll, aes(x = Longitude,
                                           y = Latitude,
                                           label = findspot_ancient_clean), 
                  nudge_x = c(-2, -1.5, -1), 
                  nudge_y = c(-0.25,-0.25,-0.25)) +
  labs(size = "Density",
       caption = paste("n = ",
                       LIRE_dated_corpus_no_place_filtering_clean_n$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0) (Clean province = Dalmatia).\n",
                       "Roads = DARMC (CC BY-NC-SA 4.0). Provinces = AWMC (ODbL).\nSettlements = Pleiades (CC-BY).\n",
                       "Filtered by key words and tags."),
                       title = "Distribution of military funerary and sacral monuments",
                       subtitle = "Dalmatia: Julio-Claudians to Antonines") +
  coord_sf(default_crs = st_crs(4326), xlim = c(13, 21), ylim = c(41.5, 46)) +
  theme_void()

plot(plot8)

ggsave("output_images/geographical_distribution/LIRE_clean_dated_corpus_scatter_no_place_filter.jpeg",
       width = 180, height = 140, unit = "mm", dpi = 600)

#save csv of plotted places
LIRE_Dal_corpus_place <- st_drop_geometry(LIRE_Dal_corpus_ll)
write.csv(LIRE_Dal_corpus_place,
          file = "output_tables/corpus/places/LIRE_thesis_corpus.csv")
LIRE_Dal_corpus_no_place_filtering_place <- st_drop_geometry(LIRE_Dal_corpus_no_place_filtering_ll)
write.csv(LIRE_Dal_corpus_no_place_filtering_place,
          file = "output_tables/corpus/places/LIRE_thesis_corpus_no_place_filter.csv")          
LIRE_Dal_corpus_clean_place <- st_drop_geometry(LIRE_Dal_corpus_clean_ll)
write.csv(LIRE_Dal_corpus_clean_place,
          file = "output_tables/corpus/places/LIRE_clean_thesis_corpus.csv")
LIRE_Dal_corpus_no_place_filtering_clean_place <- st_drop_geometry(LIRE_Dal_corpus_no_place_filtering_clean_ll)
write.csv(LIRE_Dal_corpus_no_place_filtering_clean_place,
          file = "output_tables/corpus/places/LIRE_clean_thesis_corpus_no_place_filter.csv")
LIRE_dated_corpus_place <- st_drop_geometry(LIRE_dated_corpus_ll)
write.csv(LIRE_dated_corpus_place,
          file = "output_tables/corpus/places/LIRE_thesis_corpus_dated.csv")
LIRE_dated_corpus_no_place_filtering_place <- st_drop_geometry(LIRE_dated_corpus_no_place_filtering_ll)
write.csv(LIRE_dated_corpus_no_place_filtering_place,
          file = "output_tables/corpus/places/LIRE_thesis_corpus_dated_no_place_filter.csv")
LIRE_dated_corpus_clean_place <- st_drop_geometry(LIRE_dated_corpus_clean_ll)
write.csv(LIRE_dated_corpus_clean_place,
          file = "output_tables/corpus/places/LIRE_clean_thesis_corpus_dated.csv")
LIRE_dated_corpus_no_place_filtering_clean_place <- st_drop_geometry(LIRE_dated_corpus_no_place_filtering_clean_ll)
write.csv(LIRE_dated_corpus_no_place_filtering_clean_place,
          file = "output_tables/corpus/places/LIRE_clean_thesis_corpus_dated_no_place_filter.csv")

# Now compare to all Dalmatia
LIRE_Dal_ll <- dataframe_ll(LIRE_Dal)

LIRE_Dal_n <- count(LIRE_Dal)

plot9 <-
  ggplot() + 
  geom_sf(data = world, color = "grey", fill = "lightgrey") + 
  geom_sf(data = roman_roads, colour = "grey30", size = 0.6) +
  geom_sf(data = roman_69_provinces, colour = "black", size = 0.8) +
  geom_sf(data = roman_settlements, colour = "black", alpha=0.6, size = 0.8) +
  geom_sf(data = LIRE_Dal_ll, aes(size = n), alpha=0.8, colour = "#009E73") +
  geom_sf(data = key_sites_ll, colour = "black", size = 1) +
  geom_text_repel(data = key_sites_ll, aes(x = Longitude,
                                           y = Latitude,
                                           label = findspot_ancient_clean), 
                  nudge_x = c(-2, -1.5, -1), 
                  nudge_y = c(-0.25,-0.25,-0.25)) +
  labs(size = "Density",
       caption = paste("n = ",
                       LIRE_Dal_n$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0) (Province = Dalmatia).\n",
                       "Roads = DARMC (CC BY-NC-SA 4.0). Provinces = AWMC (ODbL).\nSettlements = Pleiades (CC-BY)."),
                       title = "Distribution of funerary and sacral monuments",
                       subtitle = "Dalmatia") +
  coord_sf(default_crs = st_crs(4326), xlim = c(13, 21), ylim = c(41.5, 46)) +
  theme_void()

plot(plot9)

ggsave("output_images/geographical_distribution/LIRE_dal_scatter.jpeg",
       width = 180, height = 140, unit = "mm", dpi = 600)

LIRE_Dal_clean_ll <- dataframe_ll(LIRE_Dal_clean)

LIRE_Dal_clean_n <- count(LIRE_Dal_clean)

plot10 <-
  ggplot() + 
  geom_sf(data = world, color = "grey", fill = "lightgrey") + 
  geom_sf(data = roman_roads, colour = "grey30", size = 0.6) +
  geom_sf(data = roman_69_provinces, colour = "black", size = 0.8) +
  geom_sf(data = roman_settlements, colour = "black", alpha=0.6, size = 0.8) +
  geom_sf(data = LIRE_Dal_clean_ll, aes(size = n), alpha=0.8, colour = "#009E73") +
  geom_sf(data = key_sites_ll, colour = "black", size = 1) +
  geom_text_repel(data = key_sites_ll, aes(x = Longitude,
                                           y = Latitude,
                                           label = findspot_ancient_clean), 
                  nudge_x = c(-2, -1.5, -1), 
                  nudge_y = c(-0.25,-0.25,-0.25)) +
  labs(size = "Density",
       caption = paste("n = ",
                       LIRE_Dal_clean_n$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0) (Clean province = Dalmatia).\n",
                       "Roads = DARMC (CC BY-NC-SA 4.0). Provinces = AWMC (ODbL).\nSettlements = Pleiades (CC-BY)."),
                       title = "Distribution of funerary and sacral monuments",
                       subtitle = "Dalmatia") +
  coord_sf(default_crs = st_crs(4326), xlim = c(13, 21), ylim = c(41.5, 46)) +
  theme_void()

plot(plot10)

ggsave("output_images/geographical_distribution/LIRE_clean_dal_scatter.jpeg",
       width = 180, height = 140, unit = "mm", dpi = 600)

LIRE_Dal_dated_ll <- dataframe_ll(LIRE_Dal_dated)

LIRE_Dal_dated_n <- count(LIRE_Dal_dated)

plot11 <-
  ggplot() + 
  geom_sf(data = world, color = "grey", fill = "lightgrey") + 
  geom_sf(data = roman_roads, colour = "grey30", size = 0.6) +
  geom_sf(data = roman_69_provinces, colour = "black", size = 0.8) +
  geom_sf(data = roman_settlements, colour = "black", alpha=0.6, size = 0.8) +
  geom_sf(data = LIRE_Dal_dated_ll, aes(size = n), alpha=0.8, colour = "#009E73") +
  geom_sf(data = key_sites_ll, colour = "black", size = 1) +
  geom_text_repel(data = key_sites_ll, aes(x = Longitude,
                                           y = Latitude,
                                           label = findspot_ancient_clean), 
                  nudge_x = c(-2, -1.5, -1), 
                  nudge_y = c(-0.25,-0.25,-0.25)) +
  labs(size = "Density",
       caption = paste("n = ",
                       LIRE_Dal_dated_n$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0) (Province = Dalmatia).\n",
                       "Roads = DARMC (CC BY-NC-SA 4.0). Provinces = AWMC (ODbL).\nSettlements = Pleiades (CC-BY)."),
                       title = "Distribution of funerary and sacral monuments",
                       subtitle = "Dalmatia: Julio-Claudians to Antonines") +
  coord_sf(default_crs = st_crs(4326), xlim = c(13, 21), ylim = c(41.5, 46)) +
  theme_void()

plot(plot11)

ggsave("output_images/geographical_distribution/LIRE_dated_dal_scatter.jpeg",
       width = 180, height = 140, unit = "mm", dpi = 600)

LIRE_Dal_dated_clean_ll <- dataframe_ll(LIRE_Dal_dated_clean)

LIRE_Dal_dated_clean_n <- count(LIRE_Dal_dated_clean)

plot12 <-
  ggplot() + 
  geom_sf(data = world, color = "grey", fill = "lightgrey") + 
  geom_sf(data = roman_roads, colour = "grey30", size = 0.6) +
  geom_sf(data = roman_69_provinces, colour = "black", size = 0.8) +
  geom_sf(data = roman_settlements, colour = "black", alpha=0.6, size = 0.8) +
  geom_sf(data = LIRE_Dal_dated_clean_ll, aes(size = n), alpha=0.8, colour = "#009E73") +
  geom_sf(data = key_sites_ll, colour = "black", size = 1) +
  geom_text_repel(data = key_sites_ll, aes(x = Longitude,
                                           y = Latitude,
                                           label = findspot_ancient_clean), 
                  nudge_x = c(-2, -1.5, -1), 
                  nudge_y = c(-0.25,-0.25,-0.25)) +
  labs(size = "Density",
       caption = paste("n = ",
                       LIRE_Dal_dated_clean_n$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0) (Clean province = Dalmatia).\n",
                       "Roads = DARMC (CC BY-NC-SA 4.0). Provinces = AWMC (ODbL).\nSettlements = Pleiades (CC-BY)."),
                       title = "Distribution of funerary and sacral monuments",
                       subtitle = "Dalmatia: Julio-Claudians to Antonines") +
  coord_sf(default_crs = st_crs(4326), xlim = c(13, 21), ylim = c(41.5, 46)) +
  theme_void()

plot(plot12)

ggsave("output_images/geographical_distribution/LIRE_dated_clean_dal_scatter.jpeg",
       width = 180, height = 140, unit = "mm", dpi = 600)

# now to save plotted places
LIRE_Dal_place <- st_drop_geometry(LIRE_Dal_ll)
write.csv(LIRE_Dal_place,
          file = "output_tables/corpus/places/LIRE_thesis_dalmatia.csv")
LIRE_Dal_clean_place <- st_drop_geometry(LIRE_Dal_clean_ll)
write.csv(LIRE_Dal_clean_place,
          file = "output_tables/corpus/places/LIRE_clean_thesis_dalmatia.csv")
LIRE_Dal_dated_place <- st_drop_geometry(LIRE_Dal_dated_ll)
write.csv(LIRE_Dal_dated_place,
          file = "output_tables/corpus/places/LIRE_thesis_dalmatia_dated.csv")
LIRE_Dal_dated_clean_place <- st_drop_geometry(LIRE_Dal_dated_clean_ll)
write.csv(LIRE_Dal_dated_clean_place,
          file = "output_tables/corpus/places/LIRE_clean_thesis_dalmatia_dated.csv")

#now to combine https://www.geeksforgeeks.org/draw-multiple-ggplot2-plots-side-by-side/
## first, combine undated
doubletrouble <- grid.arrange(plot1, plot9, ncol = 2)

ggsave("output_images/geographical_distribution/LIRE_corpus_and_dal_scatter.pdf",
       doubletrouble, width = 11.7, height = 8.3)
ggsave("output_images/geographical_distribution/LIRE_corpus_and_dal_scatter.jpeg",
       doubletrouble, width = 180, height = 140, unit = "mm", dpi = 600)

doubletroubler <- grid.arrange(plot3, plot10, ncol = 2)

ggsave("output_images/geographical_distribution/dated_LIRE_clean_corpus_and_dal_scatter.pdf",
       doubletroubler, width = 11.7, height = 8.3)
ggsave("output_images/geographical_distribution/dated_LIRE_clean_corpus_and_dal_scatter.jpeg",
       doubletroubler, width = 180, height = 140, unit = "mm", dpi = 600)

## now to combine dated
doubletroublest <- grid.arrange(plot5, plot11, ncol = 2)

ggsave("output_images/geographical_distribution/LIRE_corpus_and_dal_scatter2.pdf",
       doubletrouble, width = 11.7, height = 8.3)
ggsave("output_images/geographical_distribution/LIRE_corpus_and_dal_scatter2.jpeg",
       doubletrouble, width = 180, height = 140, unit = "mm", dpi = 600)

doubletroublester <- grid.arrange(plot7, plot12, ncol = 2)

ggsave("output_images/geographical_distribution/dated_LIRE_clean_corpus_and_dal_scatter2.pdf",
       doubletroubler, width = 11.7, height = 8.3)
ggsave("output_images/geographical_distribution/dated_LIRE_clean_corpus_and_dal_scatter2.jpeg",
       doubletroubler, width = 180, height = 140, unit = "mm", dpi = 600)
