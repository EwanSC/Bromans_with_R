library(sf)
library(ggplot2)
library(dplyr)

# following https://datacarpentry.org/r-raster-vector-geospatial/06-vector-open-shapefile-in-r/index.html

roman_69_map <- st_read(
  "shape_files/roman_empire_ad_69_extent.shp")

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
  geom_sf(data = roman_69_map, color = "black", fill = "lightgreen") + 
  geom_sf(data = roman_69_provinces, colour = 'black') +
  ggtitle("Roman Empire 69 CE", subtitle = "Bad Provinces") +
  coord_sf()



