#plot data on scatter maps
# started 17/09/2024
# edited 17/09/2024

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

## plot on map
LIRE_Dal_corpus_place <- na.omit(LIRE_Dal_corpus %>%
                                   select(findspot_ancient_clean,Longitude,Latitude) %>%
                                   group_by(findspot_ancient_clean) %>%
                                   count(findspot_ancient_clean,Longitude,Latitude) %>%
                                   arrange(desc(n)))

(LIRE_Dal_corpus_ll <- st_as_sf(LIRE_Dal_corpus_place, coords = c("Longitude", "Latitude"), remove = FALSE,
                                crs = 4326, agr = "constant"))

LIRE_Dal_corpus_n <- count(LIRE_Dal_corpus)

plot1 <- 
  ggplot() + 
  geom_sf(data = world, color = "grey", fill = "lightgrey") + 
  geom_sf(data = roman_roads, colour = "grey30", size = 0.6) +
  geom_sf(data = roman_69_provinces, colour = "black", size = 0.8) +
  geom_sf(data = roman_settlements, colour = "grey30", alpha=0.6, size = 0.8) +
  geom_sf(data = LIRE_Dal_corpus_ll, aes(size = n), alpha=0.8, colour = "sienna1") +
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
                       ". Data from LIRE v.3.0.\n",
                       "Roads = DARMC. Provinces = AWMC. Settlements = Pleiades"),
                       title = "Distribution of military funerary and sacral monuments",
                       subtitle = "Dalmatia") +
  coord_sf(default_crs = st_crs(4326), xlim = c(13, 21), ylim = c(41.5, 46)) +
  theme_void()

plot(plot1)

ggsave("output_images/geographical_distribution/LIRE_corpus_scatter.jpeg", dpi = 600)

LIRE_Dal_corpus_clean_place <- na.omit(LIRE_Dal_corpus_clean %>%
                                         select(findspot_ancient_clean,Longitude,Latitude) %>%
                                         group_by(findspot_ancient_clean) %>%
                                         count(findspot_ancient_clean,Longitude,Latitude) %>%
                                         arrange(desc(n)))

(LIRE_Dal_corpus_clean_ll <- st_as_sf(LIRE_Dal_corpus_clean_place, coords = c("Longitude", "Latitude"), remove = FALSE,
                                      crs = 4326, agr = "constant"))

LIRE_Dal_corpus_clean_n <- count(LIRE_Dal_corpus_clean)

plot2 <- 
  ggplot() + 
  geom_sf(data = world, color = "grey", fill = "lightgrey") + 
  geom_sf(data = roman_roads, colour = "grey30", size = 0.6) +
  geom_sf(data = roman_69_provinces, colour = "black", size = 0.8) +
  geom_sf(data = roman_settlements, colour = "black", alpha=0.6, size = 0.8) +
  geom_sf(data = LIRE_Dal_corpus_clean_ll, aes(size = n), alpha=0.8, colour = "sienna1") +
  geom_sf(data = key_sites_ll, colour = "black", size = 1) +
  geom_text_repel(data = key_sites_ll, aes(x = Longitude, y = Latitude,label = findspot_ancient_clean), 
                  nudge_x = c(-2, -1.5, -1), 
                  nudge_y = c(-0.25,-0.25,-0.25)) +
  labs(size = "Density", 
       caption = paste("n = ",
                       LIRE_Dal_corpus_clean_n$n,
                       sep = "",
                       ". Data from LIRE v.3.0 (Clean province = Dalmatia).\n",
                       "Roads = DARMC. Provinces = AWMC. Settlements = Pleiades"),
                      title = "Distribution of military funerary and sacral monuments",
                      subtitle = "Dalmatia") +
  coord_sf(default_crs = st_crs(4326), xlim = c(13, 21), ylim = c(41.5, 46)) +
  theme_void()

plot(plot2)

ggsave("output_images/geographical_distribution/LIRE_clean_corpus_scatter.jpeg", dpi = 600)

## create one for just relevant period (~200 CE cut off)
LIRE_dated_corpus_place <- na.omit(LIRE_dated_corpus %>%
                                     select(findspot_ancient_clean,Longitude,Latitude) %>%
                                     group_by(findspot_ancient_clean) %>%
                                     count(findspot_ancient_clean,Longitude,Latitude) %>%
                                     arrange(desc(n)))

(LIRE_dated_corpus_ll <- st_as_sf(LIRE_dated_corpus_place, coords = c("Longitude", "Latitude"), remove = FALSE,
                                  crs = 4326, agr = "constant"))

LIRE_dated_corpus_n <- count(LIRE_dated_corpus)

plot3 <-
  ggplot() + 
  geom_sf(data = world, color = "grey", fill = "lightgrey") + 
  geom_sf(data = roman_roads, colour = "grey30", size = 0.6) +
  geom_sf(data = roman_69_provinces, colour = "black", size = 0.8) +
  geom_sf(data = roman_settlements, colour = "grey30", alpha=0.6, size = 0.8) +
  geom_sf(data = LIRE_dated_corpus_ll, aes(size = n), alpha=0.8, colour = "sienna1") +
  geom_sf(data = key_sites_ll, colour = "black", size = 1) +
  geom_text_repel(data = key_sites_ll, aes(x = Longitude, y = Latitude,label = findspot_ancient_clean), 
                  nudge_x = c(-2, -1.5, -1), 
                  nudge_y = c(-0.25,-0.25,-0.25)) +
  labs(size = "Density",
       caption = paste("n = ",
                       LIRE_dated_corpus_n$n,
                       sep = "",
                       ". Data from LIRE v.3.0.\n",
                       "Roads = DARMC. Provinces = AWMC. Settlements = Pleiades"),
                       title = "Distribution of military funerary and sacral monuments",
                       subtitle = "Dalmatia: Julio-Claudians to Antonines") +
  coord_sf(default_crs = st_crs(4326), xlim = c(13, 21), ylim = c(41.5, 46)) +
  theme_void()

plot(plot3)

ggsave("output_images/geographical_distribution/LIRE_dated_corpus_scatter.jpeg", dpi = 600)

LIRE_clean_dated_corpus_place <- na.omit(LIRE_clean_dated_corpus %>%
                                           select(findspot_ancient_clean,Longitude,Latitude) %>%
                                           group_by(findspot_ancient_clean) %>%
                                           count(findspot_ancient_clean,Longitude,Latitude) %>%
                                           arrange(desc(n)))

(LIRE_clean_dated_corpus_ll <- st_as_sf(LIRE_clean_dated_corpus_place, coords = c("Longitude", "Latitude"), remove = FALSE,
                                        crs = 4326, agr = "constant"))

LIRE_clean_dated_corpus_n <- count(LIRE_clean_dated_corpus)

plot4 <-
  ggplot() + 
  geom_sf(data = world, color = "grey", fill = "lightgrey") + 
  geom_sf(data = roman_roads, colour = "grey30", size = 0.6) +
  geom_sf(data = roman_69_provinces, colour = "black", size = 0.8) +
  geom_sf(data = roman_settlements, colour = "grey30", alpha=0.6, size = 0.8) +
  geom_sf(data = LIRE_clean_dated_corpus_ll, aes(size = n), alpha=0.8, colour = "sienna1") +
  geom_sf(data = key_sites_ll, colour = "black", size = 1) +
  geom_text_repel(data = key_sites_ll, aes(x = Longitude, y = Latitude,label = findspot_ancient_clean), 
                  nudge_x = c(-2, -1.5, -1), 
                  nudge_y = c(-0.25,-0.25,-0.25)) +
  labs(size = "Density",
       caption = paste("n = ",
                       LIRE_clean_dated_corpus_n$n,
                       sep = "",
                       ". Data from LIRE v.3.0 (Clean province = Dalmatia).\n",
                       "Roads = DARMC. Provinces = AWMC. Settlements = Pleiades"),
                       title = "Distribution of military funerary and sacral monuments",
                       subtitle = "Dalmatia: Julio-Claudians to Antonines") +
  coord_sf(default_crs = st_crs(4326), xlim = c(13, 21), ylim = c(41.5, 46)) +
  theme_void()

plot(plot4)

ggsave("output_images/geographical_distribution/LIRE_clean_dated_corpus_scatter.jpeg", dpi = 600)

# Now compare to all dalmatia
LIRE_all_Dal_place <- na.omit(LIRE_all_Dal %>%
                                select(findspot_ancient_clean,Longitude,Latitude) %>%
                                group_by(findspot_ancient_clean) %>%
                                count(findspot_ancient_clean,Longitude,Latitude) %>%
                                arrange(desc(n)))

(LIRE_all_Dal_ll <- st_as_sf(LIRE_all_Dal_place, coords = c("Longitude", "Latitude"), remove = FALSE,
                             crs = 4326, agr = "constant"))

LIRE_all_Dal_n <- count(LIRE_all_Dal)

plot5 <-
  ggplot() + 
  geom_sf(data = world, color = "grey", fill = "lightgrey") + 
  geom_sf(data = roman_roads, colour = "grey30", size = 0.6) +
  geom_sf(data = roman_69_provinces, colour = "black", size = 0.8) +
  geom_sf(data = roman_settlements, colour = "black", alpha=0.6, size = 0.8) +
  geom_sf(data = LIRE_all_Dal_ll, aes(size = n), alpha=0.8, colour = "brown") +
  geom_sf(data = key_sites_ll, colour = "black", size = 1) +
  geom_text_repel(data = key_sites_ll, aes(x = Longitude, y = Latitude,label = findspot_ancient_clean), 
                  nudge_x = c(-2, -1.5, -1), 
                  nudge_y = c(-0.25,-0.25,-0.25)) +
  labs(size = "Density",
       caption = paste("n = ",
                       LIRE_all_Dal_n$n,
                       sep = "",
                       ". Data from LIRE v.3.0.\n",
                       "Roads = DARMC. Provinces = AWMC. Settlements = Pleiades"),
                       title = "Distribution of funerary and sacral monuments",
                       subtitle = "Dalmatia") +
  coord_sf(default_crs = st_crs(4326), xlim = c(13, 21), ylim = c(41.5, 46)) +
  theme_void()

plot(plot5)

ggsave("output_images/geographical_distribution/LIRE_dal_scatter.jpeg", dpi = 600)

LIRE_all_Dal_clean_place <- na.omit(LIRE_all_Dal_clean %>%
                                      select(findspot_ancient_clean,Longitude,Latitude) %>%
                                      group_by(findspot_ancient_clean) %>%
                                      count(findspot_ancient_clean,Longitude,Latitude) %>%
                                      arrange(desc(n)))

(LIRE_all_Dal_clean_ll <- st_as_sf(LIRE_all_Dal_clean_place, coords = c("Longitude", "Latitude"), remove = FALSE,
                                   crs = 4326, agr = "constant"))

LIRE_all_Dal_clean_n <- count(LIRE_all_Dal_clean)

plot6 <-
  ggplot() + 
  geom_sf(data = world, color = "grey", fill = "lightgrey") + 
  geom_sf(data = roman_roads, colour = "grey30", size = 0.6) +
  geom_sf(data = roman_69_provinces, colour = "black", size = 0.8) +
  geom_sf(data = roman_settlements, colour = "black", alpha=0.6, size = 0.8) +
  geom_sf(data = LIRE_all_Dal_clean_ll, aes(size = n), alpha=0.8, colour = "brown") +
  geom_sf(data = key_sites_ll, colour = "black", size = 1) +
  geom_text_repel(data = key_sites_ll, aes(x = Longitude, y = Latitude,label = findspot_ancient_clean), 
                  nudge_x = c(-2, -1.5, -1), 
                  nudge_y = c(-0.25,-0.25,-0.25)) +
  labs(size = "Density",
       caption = paste("n = ",
                       LIRE_all_Dal_clean_n$n,
                       sep = "",
                       ". Data from LIRE v.3.0 (Clean province = Dalmatia).\n",
                       "Roads = DARMC. Provinces = AWMC. Settlements = Pleiades"),
                       title = "Distribution of funerary and sacral monuments",
                       subtitle = "Dalmatia") +
  coord_sf(default_crs = st_crs(4326), xlim = c(13, 21), ylim = c(41.5, 46)) +
  theme_void()

plot(plot6)

ggsave("output_images/geographical_distribution/LIRE_clean_dal_scatter.jpeg", dpi = 600)

LIRE_all_Dal_dated_place <- na.omit(LIRE_all_Dal_dated %>%
                                      select(findspot_ancient_clean,Longitude,Latitude) %>%
                                      group_by(findspot_ancient_clean) %>%
                                      count(findspot_ancient_clean,Longitude,Latitude) %>%
                                      arrange(desc(n)))

(LIRE_all_Dal_dated_ll <- st_as_sf(LIRE_all_Dal_dated_place, coords = c("Longitude", "Latitude"), remove = FALSE,
                                   crs = 4326, agr = "constant"))

LIRE_all_Dal_dated_n <- count(LIRE_all_Dal_dated)

plot7 <-
  ggplot() + 
  geom_sf(data = world, color = "grey", fill = "lightgrey") + 
  geom_sf(data = roman_roads, colour = "grey30", size = 0.6) +
  geom_sf(data = roman_69_provinces, colour = "black", size = 0.8) +
  geom_sf(data = roman_settlements, colour = "black", alpha=0.6, size = 0.8) +
  geom_sf(data = LIRE_all_Dal_dated_ll, aes(size = n), alpha=0.8, colour = "brown") +
  geom_sf(data = key_sites_ll, colour = "black", size = 1) +
  geom_text_repel(data = key_sites_ll, aes(x = Longitude, y = Latitude,label = findspot_ancient_clean), 
                  nudge_x = c(-2, -1.5, -1), 
                  nudge_y = c(-0.25,-0.25,-0.25)) +
  labs(size = "Density",
       caption = paste("n = ",
                       LIRE_all_Dal_dated_n$n,
                       sep = "",
                       ". Data from LIRE v.3.0.\n",
                       "Roads = DARMC. Provinces = AWMC. Settlements = Pleiades"),
                       title = "Distribution of funerary and sacral monuments",
                       subtitle = "Dalmatia: Julio-Claudians to Antonines") +
  coord_sf(default_crs = st_crs(4326), xlim = c(13, 21), ylim = c(41.5, 46)) +
  theme_void()

plot(plot7)

ggsave("output_images/geographical_distribution/LIRE_dated_dal_scatter.jpeg", dpi = 600)

LIRE_all_Dal_clean_dated_place <- na.omit(LIRE_all_Dal_clean_dated %>%
                                            select(findspot_ancient_clean,Longitude,Latitude) %>%
                                            group_by(findspot_ancient_clean) %>%
                                            count(findspot_ancient_clean,Longitude,Latitude) %>%
                                            arrange(desc(n)))

(LIRE_all_Dal_clean_dated_ll <- st_as_sf(LIRE_all_Dal_clean_dated_place, coords = c("Longitude", "Latitude"), remove = FALSE,
                                         crs = 4326, agr = "constant"))

LIRE_all_Dal_clean_dated_n <- count(LIRE_all_Dal_clean_dated)

plot8 <-
  ggplot() + 
  geom_sf(data = world, color = "grey", fill = "lightgrey") + 
  geom_sf(data = roman_roads, colour = "grey30", size = 0.6) +
  geom_sf(data = roman_69_provinces, colour = "black", size = 0.8) +
  geom_sf(data = roman_settlements, colour = "black", alpha=0.6, size = 0.8) +
  geom_sf(data = LIRE_all_Dal_clean_dated_ll, aes(size = n), alpha=0.8, colour = "brown") +
  geom_sf(data = key_sites_ll, colour = "black", size = 1) +
  geom_text_repel(data = key_sites_ll, aes(x = Longitude, y = Latitude,label = findspot_ancient_clean), 
                  nudge_x = c(-2, -1.5, -1), 
                  nudge_y = c(-0.25,-0.25,-0.25)) +
  labs(size = "Density",
       caption = paste("n = ",
                       LIRE_all_Dal_clean_dated_n$n,
                       sep = "",
                       ". Data from LIRE v.3.0 (Clean province = Dalmatia).\n",
                       "Roads = DARMC. Provinces = AWMC. Settlements = Pleiades"),
                       title = "Distribution of funerary and sacral monuments",
                       subtitle = "Dalmatia: Julio-Claudians to Antonines") +
  coord_sf(default_crs = st_crs(4326), xlim = c(13, 21), ylim = c(41.5, 46)) +
  theme_void()

plot(plot8)

ggsave("output_images/geographical_distribution/LIRE_dated_clean_dal_scatter.jpeg", dpi = 600)

#now to combine https://www.geeksforgeeks.org/draw-multiple-ggplot2-plots-side-by-side/
## first, combine undated
doubletrouble <- grid.arrange(plot1, plot5, ncol = 2)

ggsave("output_images/geographical_distribution/LIRE_corpus_and_dal_scatter.pdf", doubletrouble, width = 11.7, height = 8.3)
ggsave("output_images/geographical_distribution/LIRE_corpus_and_dal_scatter.jpeg", doubletrouble, dpi = 600)

doubletroubler <- grid.arrange(plot2, plot6, ncol = 2)

ggsave("output_images/geographical_distribution/dated_LIRE_clean_corpus_and_dal_scatter.pdf", doubletroubler, width = 11.7, height = 8.3)
ggsave("output_images/geographical_distribution/dated_LIRE_clean_corpus_and_dal_scatter.jpeg", doubletroubler, dpi = 600)

## now to combine dated
doubletroublest <- grid.arrange(plot3, plot7, ncol = 6)

ggsave("output_images/geographical_distribution/LIRE_corpus_and_dal_scatter.pdf", doubletrouble, width = 11.7, height = 8.3)
ggsave("output_images/geographical_distribution/LIRE_corpus_and_dal_scatter.jpeg", doubletrouble, dpi = 600)

doubletroublester <- grid.arrange(plot4, plot8, ncol = 2)

ggsave("output_images/geographical_distribution/dated_LIRE_clean_corpus_and_dal_scatter.pdf", doubletroubler, width = 11.7, height = 8.3)
ggsave("output_images/geographical_distribution/dated_LIRE_clean_corpus_and_dal_scatter.jpeg", doubletroubler, dpi = 600)
