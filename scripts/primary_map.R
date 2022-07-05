library(sf)
library(ggplot2)
library(dplyr)
library("rnaturalearthdata")
library("rnaturalearth")
library("rnaturalearthhires")

# couldn't download rnaturalearthhires
# fixed with https://stackoverflow.com/questions/66294044/rnaturalearthhires-package-installation-error
# following https://datacarpentry.org/r-raster-vector-geospatial/06-vector-open-shapefile-in-r/index.html

roman_69_map <- st_read(
  "shape_files/roman_empire_ad_69_extent.shp")

# bring world geo data from rnatural earth
world <- ne_countries(scale = "large", returnclass = "sf")
class(world)

# wasn't working, but issue explained by https://stackoverflow.com/questions/64929640/opening-shapefile-with-sf-in-r-gives-an-error

st_geometry_type(roman_69_map)

st_crs(roman_69_map)

st_bbox(roman_69_map) 

ncol(roman_69_map)

names(roman_69_map)

head(roman_69_map)

roman_69_provinces <- st_read(
  "shape_files/roman_empire_ad_69_provinces.shp")

ggplot() + 
  geom_sf(data = world, color = "darkgrey", fill = "lightgrey") + 
  geom_sf(data = roman_69_provinces, colour = 'black') +
  ggtitle("Roman Empire 69 CE", subtitle = "Dalmatia") +
  coord_sf(xlim = c(13, 21), ylim = c(41.5, 46))



