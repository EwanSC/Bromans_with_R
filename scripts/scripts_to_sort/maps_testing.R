# New approach at maps using R, sf and ggplot2 from https://r-spatial.org/r/2018/10/25/ggplot2-sf.html
library(tidyverse) #installing tidyvery

library("ggplot2")

theme_set(theme_bw())

library("sf")

library("rnaturalearth")

library("rnaturalearthdata")

library(OpenStreetMap) ## need to install Java - https://stackoverflow.com/questions/50995025/why-cant-i-load-the-openstreetmap-r-package

library(osmdata)

library("rnaturalearthhires")


# bring world geo data from rnatural earth
world <- ne_countries(scale = "large", returnclass = "sf")
class(world)

# generate world map
ggplot(data = world) +
  geom_sf()

# generate one with titles etc
ggplot(data = world) +
  geom_sf() +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("World map", subtitle = paste0("(", length(unique(world$name)), " countries)"))

# make one with just Balkan countries
ggplot(data = world) +
  geom_sf(color = "black", fill = "lightgreen") +
  coord_sf(xlim = c(12, 21), ylim = c(40, 49), expand = FALSE)

# make a lat long df of locations of epigraphy
epigdatafirstcent <- load_epig_data("data/2021-11-16-EDCS_via_Lat_Epig-prov_Dalmatia-10140.json") #makes a df that provides counts of distinct place

DLatLon <- na.omit(epigdatafirstcent %>%
                        select(Latitude,Longitude))

# plot it the simple way (as point data)
ggplot(data = world) +
  geom_sf(color = "black", fill = "lightgreen") +
  geom_point(data = DLatLon, aes(x = Longitude, y = Latitude), size = 1, 
             fill = "darkred") +
  coord_sf(xlim = c(10, 24), ylim = c(35, 49), expand = FALSE)

# plot it the more complex way - as sf
(DLatLonsf <- st_as_sf(DLatLon, coords = c("Longitude", "Latitude"), 
                    crs = 4326, agr = "constant"))

ggplot(data = world) +
  geom_sf() +
  geom_sf(data = DLatLonsf, size = 1) +
  coord_sf(xlim = c(10, 24), ylim = c(35, 49), expand = FALSE)

# now to do this with a range of inscriptions
(DLatLonNNPlot <- st_as_sf(DLatLonNoNULL, coords = c('Longitude', 'Latitude'), 
                          crs = 4326, agr = "constant") %>%
  filter (n %in% (30:1000)))

ggplot(data = world) +
  geom_sf(color = "black", fill = "lightblue") +
  geom_sf(data = DLatLonNNPlot, size = 1, colour = "orange", fill = "orange") + 
  coord_sf(xlim = c(10, 24), ylim = c(35, 49), expand = FALSE)

# another map source

mapdatadf <- map_data("worldHires")
class(mapdatadf)

mapdatasf <- st_as_sf(mapdatadf, coords = c('long', 'lat'), 
                      crs = 4326, agr = "constant")

# these maps are bad for the islands though, so lets try a more detailed one
#ggplot(data = mapdatasf) +
#  geom_sf(color = "black", fill = "lightblue") +
#  geom_sf(data = DLatLonNNAll, size = 1, colour = "orange", fill = "orange") + 
#  coord_sf(xlim = c(10, 24), ylim = c(35, 49), expand = FALSE)

# this one works but takes ages and its an outdated map (yay)
# how about OpenStreetMap 
# very lost at the moment form here on in

osmbalkans <- openmap(c(13, 21),
                      c(41.5, 46),
                      type = "esri",
                      zoom = TRUE)
class(osmbalkans)

plot(osmbalkans)

# need to make data points osm compatible and make them mercantor projection (3857)
(DLatLon3857sf <- st_as_sf(DLatLon, coords = c("Longitude", "Latitude"), 
                       crs = 3857, agr = "constant"))

ggplot(data = osmbalkans) +
  geom_sf(color = "black", fill = "lightblue") +
  geom_sf(data = osmbalkans, size = 1, colour = "orange", fill = "orange") + 
  coord_sf(xlim = c(10, 24), ylim = c(35, 49), expand = FALSE)
