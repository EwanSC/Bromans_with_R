# counting dalmatian inscriptions on
# LIRE, EDCS, EDH
# 27/09/2024
# last ran 11/10/2024
library(dplyr)
library(stringr)
library(rjson)
library(tibble)
library(data.table)
library(arrow)
library(sqldf)
library(arrow)
library(ggplot2)
library(sf)
library(ggrepel)
library("gridExtra")
library("rnaturalearthdata")
library("rnaturalearth")
library("rnaturalearthhires")

# LIRE
LIRE_all <-
  read_parquet("https://zenodo.org/records/8431452/files/LIRE_v3-0.parquet?download=1")

LIRE_Dalmatia <- filter(LIRE_all, province_label_clean == "Dalmatia" | province == "Dalmatia")

# EDCS
## downloaded via Ballsun-Stanton, B., Heřmánková, P., & Laurence, R. (2024).
## Latin Epigraphy Scraper (v2.0). GitHub.
## https://doi.org/10.5281/zenodo.12036540
## as of 2022-08-29

EDCS <- fromJSON(file= "data/EDCS/2022-08-29-EDCS_via_Lat_Epig-prov_Dalmatia-10140.json")
EDCS_data <- EDCS$data
EDCS_Dalmatia <- data.table::rbindlist(EDCS_data, fill = TRUE) 

# EDH
EDH_all <-
 read.csv("https://edh.ub.uni-heidelberg.de/data/download/edh_data_text.csv",
          na = c("","NA","NULL",NULL))

EDH_Dalmatia <- filter(EDH_all, provinz == "Dal")

write.csv(EDH_Dalmatia,
          file = "data/EDH/EDH_Dalmatia.csv")

# count
LIRE_Dalmatia_count <- count(LIRE_Dalmatia)
EDCS_Dalmatia_count <- count(EDCS_Dalmatia)
EDH_Dalmatia_count <- count(EDH_Dalmatia)

LIRE_Dalmatia_count$database <- "LIRE"
EDCS_Dalmatia_count$database <- "EDCS"
EDH_Dalmatia_count$database <- "EDH"

#merge all data frames together
count_list <- list(LIRE_Dalmatia_count, EDCS_Dalmatia_count, EDH_Dalmatia_count)      

database_Dalmatia_count <- Reduce(function(x, y) merge(x, y, all=TRUE), count_list) 

write.csv(database_Dalmatia_count,
          file = "output_tables/appendices/database_Dalmatia_count.csv")

# count Salona
LIRE_Dalmatia_salona_count <- count(filter(LIRE_Dalmatia, findspot_ancient_clean == "Salonae" | place == "Solin / Salona"))
EDCS_Dalmatia_salona_count <- count(filter(EDCS_Dalmatia, place == "Solin / Salona"))
EDH_Dalmatia_salona_count <- count(filter(EDH_Dalmatia, fo_antik %like% "Salonae"))

LIRE_Dalmatia_salona_count$database <- "LIRE"
EDCS_Dalmatia_salona_count$database <- "EDCS"
EDH_Dalmatia_salona_count$database <- "EDH"

#merge all data frames together
salona_count_list <- list(LIRE_Dalmatia_salona_count,
                             EDCS_Dalmatia_salona_count,
                             EDH_Dalmatia_salona_count)

database_Dalmatia_salona_count <- Reduce(function(x, y) merge(x, y, all=TRUE),
                                            salona_count_list) 

write.csv(database_Dalmatia_salona_count,
          file = "output_tables/appendices/database_salona_count.csv")

# plot
## download rnaturalearthhires for basemap
world <- ne_countries(scale = "large", returnclass = "sf")

## adding road data from DARMC https://hub.arcgis.com/datasets/55a54a1350e14ca0b355d95633da3851_0
roman_roads <- st_read(
  "shape_files/Roman_roads.shp")

## adding settlements from Pleiades https://pleiades.stoa.org/downloads
roman_settlements <- st_read(
  "data/mapping/Roman_settlements_pleiades.gpkg")

## create df for layer of key sites 
imp_sites <- data.frame(findspot_ancient_clean=c("Salona",
                                                 "Narona",
                                                 "Iader"),
                        Longitude=c(16.483426,
                                    17.598611,
                                    15.223778),
                        Latitude=c(43.539561,
                                   43.046389,
                                   44.115501))
(imp_sites_ll <- st_as_sf(imp_sites,
                          coords = c("Longitude",
                                     "Latitude"),
                          remove = FALSE,
                          crs = 4326, agr = "constant"))

## plot LIRE
LIRE_Dalmatia_place <- na.omit(LIRE_Dalmatia %>%
                            select(findspot_ancient_clean,
                                   Longitude,
                                   Latitude) %>%
                             group_by(findspot_ancient_clean) %>%
                             count(findspot_ancient_clean,
                                   Longitude,
                                   Latitude) %>%
                               arrange(desc(n)))
(LIRE_Dalmatia_ll <- st_as_sf(LIRE_Dalmatia_place, coords = c("Longitude", "Latitude"),
                                remove = FALSE,
                                crs = 4326, agr = "constant"))
LIRE_Dalmatia_n <- count(LIRE_Dalmatia)

plot1 <-
  ggplot() +
  geom_sf(data = world, color = "#BEBEBE", fill = "#e4e4e4") +
  geom_sf(data = roman_roads, colour = "#4D4D4D", size = 0.6) +
  geom_sf(data = roman_settlements, colour = "#4D4D4D", alpha=0.6, size = 0.8) +
  geom_sf(data = LIRE_Dalmatia_ll, aes(size = n), alpha=0.8, colour = "#3468d6") +
  geom_sf(data = imp_sites_ll, colour = "#000000", size = 1) +
  geom_text_repel(data = imp_sites_ll,
                  aes(x = Longitude,
                      y = Latitude,
                      label = findspot_ancient_clean),
                  nudge_x = c(-1, -1, -1), 
                  nudge_y = c(-0.25,-0.25,-0.25)) +
  labs(size = "Density",
       caption = paste("n = ",
                       LIRE_Dalmatia_n$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0).\n",
                       "Roads = DARMC (CC BY-NC-SA 4.0). Settlements = Pleiades (CC-BY).\n",
                       "Filtered by key words and tags"),
                       title = "Distribution of inscriptions",
                       subtitle = "LIRE") +
  coord_sf(default_crs = st_crs(4326), xlim = c(14, 21), ylim = c(41.5, 46)) +
  theme_void()

plot(plot1)

ggsave("output_images/appendix-1/01.LIRE_scatter.jpeg",
       width = 180, height = 140, unit = "mm", dpi = 600)


##plot EDCS 
EDCS_Dalmatia_place <- na.omit(EDCS_Dalmatia %>%
                            select(place,
                                   longitude,
                                   latitude) %>%
                             group_by(place) %>%
                             count(place,
                                   longitude,
                                   latitude) %>%
                               arrange(desc(n)))

(EDCS_Dalmatia_ll <- st_as_sf(EDCS_Dalmatia_place, coords = c("longitude", "latitude"),
                                remove = FALSE,
                                crs = 4326, agr = "constant"))

EDCS_Dalmatia_n <- count(EDCS_Dalmatia)

plot2 <-
  ggplot() + 
  geom_sf(data = world, color = "#BEBEBE", fill = "#e4e4e4") + 
  geom_sf(data = roman_roads, colour = "#4D4D4D", size = 0.6) +
  geom_sf(data = roman_settlements, colour = "#4D4D4D", alpha=0.6, size = 0.8) +
  geom_sf(data = EDCS_Dalmatia_ll, aes(size = n), alpha=0.8, colour = "#3468d6") +
  geom_sf(data = imp_sites_ll, colour = "#000000", size = 1) +
  geom_text_repel(data = imp_sites_ll,
                  aes(x = Longitude,
                      y = Latitude,
                      label = findspot_ancient_clean),
                  nudge_x = c(-1, -1, -1), 
                  nudge_y = c(-0.25,-0.25,-0.25)) +
  labs(size = "Density",
       caption = paste("n = ",
                       EDCS_Dalmatia_n$n,
                       sep = "",
                       ".\nEpigraphic data = EDCS.\n",
                       "Roads = DARMC (CC BY-NC-SA 4.0). Settlements = Pleiades (CC-BY).\n",
                       "Filtered by key words and tags"),
                       title = "Distribution of inscriptions",
                       subtitle = "EDCS") +
  coord_sf(default_crs = st_crs(4326), xlim = c(14, 21), ylim = c(41.5, 46)) +
  theme_void()

plot(plot2)

ggsave("output_images/appendix-1/02.EDCS_scatter.jpeg",
       width = 180, height = 140, unit = "mm", dpi = 600)

##plot EDH
EDH_Dalmatia_place <- na.omit(EDH_Dalmatia %>%
                            select(fo_antik,
                                   koordinaten1) %>%
                             group_by(fo_antik) %>%
                             count(fo_antik,
                                   koordinaten1) %>%
                               arrange(desc(n)))

EDH_Dalmatia_place[c('latitude', 'longitude')] <- str_split_fixed(EDH_Dalmatia_place$koordinaten1, ',', 2)

(EDH_Dalmatia_ll <- st_as_sf(EDH_Dalmatia_place, coords = c("longitude", "latitude"),
                             remove = FALSE,
                             crs = 4326, agr = "constant"))

EDH_Dalmatia_n <- count(EDH_Dalmatia)

plot3 <-
  ggplot() + 
  geom_sf(data = world, color = "#BEBEBE", fill = "#e4e4e4") + 
  geom_sf(data = roman_roads, colour = "#4D4D4D", size = 0.6) +
  geom_sf(data = roman_settlements, colour = "#4D4D4D", alpha=0.6, size = 0.8) +
  geom_sf(data = EDH_Dalmatia_ll, aes(size = n), alpha=0.8, colour = "#3468d6") +
  geom_sf(data = imp_sites_ll, colour = "#000000", size = 1) +
  geom_text_repel(data = imp_sites_ll,
                  aes(x = Longitude,
                      y = Latitude,
                      label = findspot_ancient_clean),
                  nudge_x = c(-1, -1, -1), 
                  nudge_y = c(-0.25,-0.25,-0.25)) +
  labs(size = "Density",
       caption = paste("n = ",
                       EDH_Dalmatia_n$n,
                       sep = "",
                       ".\nEpigraphic data = EDH (CC BY-SA 4.0).\n",
                       "Roads = DARMC (CC BY-NC-SA 4.0). Settlements = Pleiades (CC-BY).\n",
                       "Filtered by key words and tags"),
                       title = "Distribution of inscriptions",
                       subtitle = "EDH") +
  coord_sf(default_crs = st_crs(4326), xlim = c(14, 21), ylim = c(41.5, 46)) +
  theme_void()

plot(plot3)

ggsave("output_images/appendix-1/03.EDH_scatter.jpeg",
       width = 180, height = 140, unit = "mm", dpi = 600)

## combine LIRE and EDH for comparison
doubletrouble <- grid.arrange(plot1, plot3, ncol = 2)

ggsave("output_images/appendix-1/04.LIRE_EDH_comparison.jpeg",
       doubletrouble, width = 240, height = 120, unit = "mm", dpi = 600)
