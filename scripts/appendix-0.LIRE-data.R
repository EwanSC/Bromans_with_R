# appendix-LIRE-data
# started 19/9/2024
# last edited 10/10/2024
# last ran 23/10/2024
library(dplyr)
library(sqldf)
library(arrow)
library(datplot)
library(ggplot2)
library(ggrepel)
library("gridExtra")
library("rnaturalearthdata")
library("rnaturalearth")
library("rnaturalearthhires")

LIRE <-
  read_parquet("https://zenodo.org/records/8431452/files/LIRE_v3-0.parquet?download=1")

# fix zadar lat long
LIRE$Longitude[LIRE$findspot_ancient_clean == 'Iader'
               & LIRE$findspot_modern_clean == 'Zadar'] <- "15.223778"

LIRE$Latitude[LIRE$findspot_ancient_clean == 'Iader'
              & LIRE$findspot_modern_clean == 'Zadar'] <- "44.115501"

#filter columns
LIRE_filtered <- select(LIRE,
                      "LIST-ID",
                      "EDCS-ID",
                      "EDH-ID",
                      "not_before",
                      "not_after",
                      "transcription",
                      "clean_text_interpretive_word",
                      "type_of_inscription_clean",
                      "type_of_inscription_auto",
                      "type_of_monument_clean",
                      "province",
                      "province_label_clean",
                      "place",
                      "findspot_ancient_clean",
                      "findspot_modern_clean",
                      "status_notation",
                      "Latitude",
                      "Longitude")


#get only Dalmatia
LIRE_Dal_EDCS <- filter(LIRE_filtered, province == "Dalmatia")
LIRE_Dal_EDH <- filter(LIRE_filtered, province_label_clean == "Dalmatia")
LIRE_Dalmatia <- filter(LIRE_filtered, province_label_clean == "Dalmatia" | province == "Dalmatia")

write.csv(LIRE_Dalmatia, file = "data/LIRE/LIRE_Dalmatia.csv")
write.csv(LIRE_Dal_EDCS, file = "data/LIRE/LIRE_Dal_EDCS.csv")
write.csv(LIRE_Dal_EDH, file = "data/LIRE/LIRE_Dal_EDH.csv")

## plot
world <- ne_countries(scale = "large", returnclass = "sf")

roman_roads <- st_read(
  "shape_files/Roman_roads.shp")

roman_settlements <- st_read(
  "data/mapping/Roman_settlements_pleiades.gpkg")

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
LIRE_Dalmatia_ll <- dataframe_ll(LIRE_Dalmatia)

LIRE_Dalmatia_n <- count(LIRE_Dalmatia)

ggplot() + 
  geom_sf(data = world, color = "#BEBEBE", fill = "#e4e4e4") + 
  geom_sf(data = roman_roads, colour = "#4D4D4D", size = 0.6) +
  geom_sf(data = roman_settlements, colour = "#4D4D4D", alpha=0.6, size = 0.8) +
  geom_sf(data = LIRE_Dalmatia_ll, aes(size = n), alpha=0.8, colour = "#3468d6") +
  geom_sf(data = key_sites_ll, colour = "#000000", size = 0.5) +
  geom_text_repel(data = key_sites_ll,
                  aes(x = Longitude,
                      y = Latitude,
                      label = findspot_ancient_clean), 
                  nudge_x = c(-1, -1.5, -1, -1.5, -1.5, 0.5), 
                  nudge_y = c(-0.25,-0.5,-0.25,-0.25,-0.25, 1.5)) +
  labs(size = "Density",
       caption = paste("n = ",
                       LIRE_Dalmatia_n$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0).\n",
                       "Roads = DARMC (CC BY-NC-SA 4.0). Settlements = Pleiades (CC-BY)."),
       title = "Distribution of all inscriptions",
       subtitle = "Dalmatia (df)") +
  coord_sf(default_crs = st_crs(4326), xlim = c(14, 21), ylim = c(41.5, 46)) +
  theme_void()

ggsave("output_images/appendix-1/LIRE_Dalmatia_all_types_from_df.jpeg",
       doubletrouble, width = 240, height = 120, unit = "mm", dpi = 600)

LIRE_Dalmatia_csv <-
  read.csv("data/LIRE/LIRE_Dalmatia.csv")

LIRE_Dalmatia_csv_ll <- dataframe_ll(LIRE_Dalmatia_csv)

LIRE_Dalmatia_csv_n <- count(LIRE_Dalmatia_csv)

ggplot() + 
  geom_sf(data = world, color = "#BEBEBE", fill = "#e4e4e4") + 
  geom_sf(data = roman_roads, colour = "#4D4D4D", size = 0.6) +
  geom_sf(data = roman_settlements, colour = "#4D4D4D", alpha=0.6, size = 0.8) +
  geom_sf(data = LIRE_Dalmatia_csv_ll, aes(size = n), alpha=0.8, colour = "#3468d6") +
  geom_sf(data = key_sites_ll, colour = "#000000", size = 0.5) +
  geom_text_repel(data = key_sites_ll,
                  aes(x = Longitude,
                      y = Latitude,
                      label = findspot_ancient_clean), 
                  nudge_x = c(-1, -1.5, -1, -1.5, -1.5, 0.5), 
                  nudge_y = c(-0.25,-0.5,-0.25,-0.25,-0.25, 1.5)) +
  labs(size = "Density",
       caption = paste("n = ",
                       LIRE_Dalmatia_csv_n$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0).\n",
                       "Roads = DARMC (CC BY-NC-SA 4.0). Settlements = Pleiades (CC-BY)."),
       title = "Distribution of all inscriptions",
       subtitle = "Dalmatia (csv)") +
  coord_sf(default_crs = st_crs(4326), xlim = c(14, 21), ylim = c(41.5, 46)) +
  theme_void()

ggsave("output_images/appendix-1/LIRE_Dalmatia_all_types_from_csv.jpeg",
       doubletrouble, width = 240, height = 120, unit = "mm", dpi = 600)
