# Mapping practice

# packages

library(tidyverse) #installing tidyvery

library(RColorBrewer) #installing colour package for plot, used by DAACS

library(ggplot2) #installing package for plotting data

library(sf) #geographic plotting package

library(tmap) #for maps
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)

#map of world
data(World, rivers, land)

tm_shape(World) +
  tm_polygons("HPI")

# with land and elevation and no legend, note, order of commands = layers
data(World,rivers, land)

tm_shape(land) +
  tm_raster("elevation", palette = terrain.colors(10)) +
tm_shape(World) +
  tm_borders("white", lwd = .5)

# now europe
world <- ne_countries(scale = "medium", returnclass = "sf")
Europe <- world[which(world$continent == "Europe"),]
ggplot(Europe) +
  geom_sf() +
  coord_sf(xlim = c(-25,50), ylim = c(35,70), expand = FALSE)

# now balkans
world <- ne_countries(scale = "medium", returnclass = "sf")
Europe <- world[which(world$continent == "Europe"),]
ggplot(Europe) +
  geom_sf() +
  coord_sf(xlim = c(41,47), ylim = c(15,25), expand = FALSE)

map <- map_data(map)

# making maps - this requires lines 1-79 of Bromans_with_R.R
## first define the map size with lat lon constraints based off the above output
ggplot(DLatLonNNPlot) + 
  geom_sf(aes(color = n)) +
  coord_sf(xlim = c(42,46), ylim = c(13.5,21), expand = FALSE)

## now get map data
mapdf <- map_data("world") 

## now map this df and add coordinate view of lower Dalmatia
ggplot(mapdf, aes(x=long, y=lat, group=group)) +
  geom_polygon(fill="white", colour="black") +
  coord_sf(xlim = c(42,46), ylim = c(13.5,21), expand = FALSE)

## or create a bigger Balkan region df and map with mercantor projection
balkanmapdf <- map_data("world", region=c("Croatia","Albania","Kosovo","Montenegro","Serbia","Bosnia and Herzegovina","Slovenia","Macedonia","Hungary")) 
ggplot(balkanmapdf, aes(x=long, y=lat, group=group, fill=region)) +
  geom_path() + 
  coord_map("mercator")

balkanmap <- ggplot(balkanmapdf, aes(x=long, y=lat, group=group, fill=region)) +
  geom_path() + 
  geom_sf(color = "black", fill = "lightgreen") +
  coord_map("mercator")

## now combine plotdf with map from balkanmapdf or lat lon constraints
## make it a df
balkanmapsf <- map_data("world", region=c("Croatia","Albania","Kosovo","Montenegro","Serbia","Bosnia and Herzegovina","Slovenia","Macedonia","Hungary"), , returnclass = "sf") 
ggplot(balkanmapdf, aes(x=long, y=lat, group=group, fill=region)) +
  geom_path() + 
  coord_map("mercator")

ggplot(data = balkanmapsf) + 
  geom_sf() +
  geom_sf(data=DLatLonNNPlot, size = 1) +
  coord_sf(xlim = c(13.5,21), ylim = c(42,45.5), expand = FALSE)



