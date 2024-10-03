# Map
# Ewan Coopey
# created 17/09/2024
# last edit: 19/09/2024
# Download required data and shape files to make province of Dalmatia map for plotting
# testing and trouble-shooting found at 'scripts/old-2.primary_map-working-out-kinks.R'
library(sf)
library(ggplot2)
library(dplyr)
library("rnaturalearthdata")
library("rnaturalearth")
library("rnaturalearthhires")

# download rnaturalearthhires
world <- ne_countries(scale = "large", returnclass = "sf")

# get Roman shape files from
roman_69_map <- st_read(
  "shape_files/roman_empire_ad_69_extent.shp")

roman_69_provinces <- st_read(
  "shape_files/roman_empire_ad_69_provinces.shp")

roman_117_map <- st_read(
  "shape_files/roman_empire_ad_117_extent.shp")

roman_117_provinces <- st_read(
  "shape_files/roman_empire_ad_117.shp")

# adding road data from DARMC https://hub.arcgis.com/datasets/55a54a1350e14ca0b355d95633da3851_0
roman_roads <- st_read(
  "shape_files/Roman_roads.shp")

# adding settlements from Pleiades https://pleiades.stoa.org/downloads
roman_settlements <- st_read(
  "data/mapping/Roman_settlements_pleiades.gpkg")

# making base maps
ggplot() + 
  geom_sf(data = world, color = "darkgrey", fill = "lightgrey") + 
  geom_sf(data = roman_69_provinces, colour = 'black') +
  geom_sf(data = roman_roads, colour = 'brown') +
  geom_sf(data = roman_settlements, colour = 'black', size = 0.8) +
  ggtitle("Roman Empire 69 CE", subtitle = "Dalmatia") +
  coord_sf(xlim = c(14, 20), ylim = c(41.5, 46)) +
  theme_void()

ggplot() + 
  geom_sf(data = world, color = "darkgrey", fill = "lightgrey") + 
  geom_sf(data = roman_roads, colour = 'brown') +
  geom_sf(data = roman_settlements, colour = 'black', size = 0.8) +
  ggtitle("Roman Empire 69 CE", subtitle = "Dalmatia") +
  coord_sf(xlim = c(14, 20), ylim = c(41.5, 46)) +
  theme_void()

ggplot() + 
  geom_sf(data = world, color = "darkgrey", fill = "lightgrey") + 
  geom_sf(data = roman_117_provinces, colour = 'black') +
  geom_sf(data = roman_roads, colour = 'brown') +
  geom_sf(data = roman_settlements, colour = 'black', size = 0.8) +
  ggtitle("Roman Empire 117 CE", subtitle = "Dalmatia") +
  coord_sf(xlim = c(14, 20), ylim = c(41.5, 46)) +
  theme_void()
