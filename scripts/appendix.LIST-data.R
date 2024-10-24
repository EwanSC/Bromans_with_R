# appendix-LIST-data
# started 23/10/2024
# last edited 23/10/2024
# last ran 23/10/2024
library(dplyr)
library(sqldf)
library(arrow)
library(datplot)
library(ggplot2)
library(sf)
library(ggrepel)
library("gridExtra")
library("rnaturalearthdata")
library("rnaturalearth")
library("rnaturalearthhires")

LIST <-
  read_parquet("https://zenodo.org/record/8147298/files/LIST_v2-3.parquet?download=1")

str(LIST)

#LIST_Dal_EDCS <- filter(LIST, province == "Dalmatia")
#LIST_Dal_EDH <- filter(LIST, province_label_clean == "Dalmatia")
#LIST_Dalmatia <- filter(LIST, province_label_clean == "Dalmatia" | province == "Dalmatia")

# map
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

LIST_Dalmatia_ll <- dataframe_ll(LIST_Dalmatia)

LIST_Dalmatia_n <- count(LIST_Dalmatia)

ggplot() + 
  geom_sf(data = world, color = "#BEBEBE", fill = "#e4e4e4") + 
  geom_sf(data = roman_roads, colour = "#4D4D4D", size = 0.6) +
  geom_sf(data = roman_settlements, colour = "#4D4D4D", alpha=0.6, size = 0.8) +
  geom_sf(data = LIST_Dalmatia_ll, aes(size = n), alpha=0.8, colour = "#3468d6") +
  geom_sf(data = key_sites_ll, colour = "#000000", size = 1) +
  geom_text_repel(data = key_sites_ll,
                  aes(x = Longitude,
                      y = Latitude,
                      label = findspot_ancient_clean), 
                  nudge_x = c(-1, -1.5, -1, -1.5, -1.5, 0.5), 
                  nudge_y = c(-0.25,-0.5,-0.25,-0.25,-0.25, 1.5)) +
  labs(size = "Density",
       caption = paste("n = ",
                       LIST_Dalmatia_n$n,
                       sep = "",
                       ".\nEpigraphic data = LIST v.3.0 (CC BY 4.0).\n",
                       "Roads = DARMC (CC BY-NC-SA 4.0). Settlements = Pleiades (CC-BY)."),
       title = "Distribution of inscriptions",
       subtitle = "Dalmatia") +
  coord_sf(default_crs = st_crs(4326), xlim = c(14, 21), ylim = c(41.5, 46)) +
  theme_void()

write.csv(LIST_Dalmatia, file = "data/LIST/LIST_Dalmatia.csv")
write.csv(LIST_Dal_EDCS, file = "data/LIST/LIST_Dal_EDCS.csv")
write.csv(LIST_Dal_EDH, file = "data/LIST/LIST_Dal_EDH.csv")
