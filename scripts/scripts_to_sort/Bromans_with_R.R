# Ewan Trying out R Code for Cleaning Epigraphic Data
# Created by: EC 2021/11/05
# Last Edit: EC 2021/11/30

# first, lets load packages

library(tidyverse) #installing tidyverse
library(RColorBrewer) #installing colour package for plot, used by DAACS
library(ggplot2) #installing package for plotting data
library(sf) #geographic plotting package
library(maps) #maps
library(mapdata) #higher res maps
library(rnaturalearth) #more maps stuff - for map origins instead of (maps)
library(rnaturalearthhires)
library(rnaturalearthdata)
library(readr)
library(dplyr)

default_crs = sf::st_crs(4326)

AllDalmatiaEpig <- read_tsv('data/2021-11-16-EDCS_via_Lat_Epig-prov_Dalmatia-10140.tsv') # Importing our first dataframe (df)

str(AllDalmatiaEpig) # lets have a look at the structure of the data

head(AllDalmatiaEpig) # what are the first 6 rows

AllDalmatiaByDate <- AllDalmatiaEpig %>% #makes a df arranged by date
  arrange("dating from", "dating to")

AllDalmatiaPlace <- AllDalmatiaEpig %>% #makes a df that provides counts of distinct places
  group_by(place) %>%
  count(place) %>%
  arrange(desc(n)) 

# make a sample df that only has places with 30+ inscriptions
DPSampleThirty <- AllDalmatiaPlace[which(AllDalmatiaPlace$n >= 30),]

# bar plot attempt with angled x-axis names for the list of places with 30+ inscriptions
ggplot(DPSampleThirty, aes(x=place, y=n, fill=place)) +
  geom_bar(stat="identity", show.legend=F, width = 1) +
  theme(axis.text.x = element_text(angle = 45, hjust=1))

# thats still a lot lets make remove the noise of Salona (5000ish inscriptions gets in the way)
# to do so I will make a df that has a range between 30-1000 and then make this into a barplot
DPSampleThirtyToThousand <- AllDalmatiaPlace %>%
  filter(n %in% (30:1000))

theme_set(theme_bw(base_size = 10))
ggplot(DPSampleThirtyToThousand, aes(x=place, y=n, fill=place)) +
  geom_bar(stat="identity", show.legend=F, width = 1) +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  ggtitle("Dalmatian sites with 30-1000 inscriptions") +
  xlab("Site: Modern/Ancient") + 
  ylab("Number of inscriptions")

png(filename = "output_images/30-1000_Dalmatia_Places_columngraph.png",
    res = 150, width = 1100, height = 1100)
    theme_set(theme_bw(base_size = 10))
    ggplot(DPSampleThirtyToThousand, aes(x=place, y=n, fill=place)) +
      geom_bar(stat="identity", show.legend=F, width = 1) +
      theme(axis.text.x = element_text(angle = 45, hjust=1)) +
      ggtitle("Dalmatian sites with 30-1000 inscriptions") +
      xlab("Site: Modern/Ancient") + 
      ylab("Number of inscriptions")
dev.off()

#now lets ignore nulls/NAs
DLatLonNoNULL <- na.omit(AllDalmatiaEpig %>%
  select(place,Longitude,Latitude)) %>%
  group_by(place) %>%
  count(place,Longitude,Latitude) %>%
  arrange(desc(n))

# lets make it into sf for mapping
# all
(DLatLonNNAll <- st_as_sf(DLatLonNoNULL, coords = c('Longitude', 'Latitude'), 
                           crs = 4326, agr = "constant"))

# sites with more than 30 inscriptions
(DLatLonNN30plus <- st_as_sf(DLatLonNoNULL[which(AllDalmatiaPlace$n >= 30),], coords = c('Longitude', 'Latitude'), 
                          crs = 4326, agr = "constant"))

# places with between 30-1000 inscriptions
(DLatLonNNPlot <- st_as_sf(DLatLonNoNULL, coords = c('Longitude', 'Latitude'), 
                          crs = 4326, agr = "constant") %>%
                          filter (n %in% (30:1000)))

# plot DLatLonNNPlot
ggplot(DLatLonNNPlot) + 
  geom_sf(aes(color = n)) 
  
# now n a map
# first, get world map
world <- ne_countries(scale = "large", returnclass = "sf")
class(world)

# First plot all locations on a map and save the output
ggplot(data = world) +
  geom_sf(color = "black", fill = "navy") +
  geom_sf(data = DLatLonNNAll, size = 0.5, colour = "red") +
  ggtitle("Latin Epigraphy in Dalmatia", 
          subtitle = "Distribution of Latin epigraphy from the EDCS") + 
  coord_sf(xlim = c(12, 24), ylim = c(40, 49), expand = FALSE)

png(filename = "output_images/Dalmatia_map.png",
    res = 150, width = 1000, height = 1000)
  ggplot(data = world) +
    geom_sf(color = "black", fill = "navy") +
    geom_sf(data = DLatLonNNAll, size = 0.5, colour = "red") + 
    ggtitle("Latin Epigraphy in Dalmatia", 
            subtitle = "Distribution of Latin epigraphy from the EDCS") + 
    coord_sf(xlim = c(12, 24), ylim = c(40, 49), expand = FALSE)
dev.off()

# now plot on map with gradient and key and zoomed 
# and only site with between 30-1000 inscriptions

ggplot(data = world) +
  geom_sf(color = "black", fill = "lightgreen") +
  geom_sf(data = DLatLonNNPlot, aes(color = n), size = 2) + 
  coord_sf(default_crs = st_crs(4326), xlim = c(13, 21), ylim = c(41.5, 46))

# now a plot with scaled points and zoomed with Salona
ggplot(data = world) +
  geom_sf(color = "black", fill = "lightgreen") +
  geom_sf(data = DLatLonNN30plus, aes(size = n), alpha=0.6) + 
  coord_sf(default_crs = st_crs(4326), xlim = c(13, 21), ylim = c(41.5, 46))

# these maps are bad for the islands, lets try a more detailed one...

# lets add eastern Italy's epigraphy too!
ItalyDalEpig <- read_tsv("data/2021-11-17-EDCS_via_Lat_Epig-prov_5(ApetCa,Da,PiRe,SaRe,UmRe)-31286.tsv")

str(ItalyDalEpig)
head(ItalyDalEpig)

ItalyDalEpigPlace <-  na.omit(ItalyDalEpig %>%
  select(place,Longitude,Latitude)) %>%
  group_by(place) %>%
  count(place,Longitude,Latitude) %>%
  arrange(desc(n))

# now all inscriptions dating between -30/100 - 9/150 CE
IDE1cent <- ItalyDalEpig %>%
  filter(ItalyDalEpig$`dating from` %in% (-30:100), ItalyDalEpig$`dating to` %in% (9:150))

IDE1centplace <- na.omit(IDE1cent %>% #makes a df that provides counts of distinct places
                               select(place,Longitude,Latitude) %>%
                               group_by(place) %>%
                               count(place,Longitude,Latitude) %>%
                               arrange(desc(n))) 

(IDE1centplacesf <- st_as_sf(IDE1centplace, coords = c('Longitude', 'Latitude'), 
                                 crs = 4326, agr = "constant"))

## lets plot this with labels and save the output
ggplot(data = world) +
  geom_sf(color = "black", fill = "lightgreen") +
  geom_sf(data = IDE1centplacesf, aes(size = n), alpha=0.6) + 
  labs(size = 'Epigraphic Monuments') +
  ggtitle("Dalmatia and Eastern Italy", 
          subtitle = "Epigraphic Distribution in the First Century CE") + 
  coord_sf(default_crs = st_crs(4326), xlim = c(13, 21), ylim = c(41.5, 46))

png(filename = "output_images/dalm_italy_1stAD.png",
    res = 150, width = 1100, height = 1100)
ggplot(data = world) +
  geom_sf(color = "black", fill = "lightgreen") +
  geom_sf(data = IDE1centplacesf, aes(size = n), alpha=0.6) + 
  labs(size = 'Epigraphic Monuments') +
  ggtitle("Dalmatia and Eastern Italy", 
          subtitle = "Epigraphic Distribution in the First Century CE") + 
  coord_sf(default_crs = st_crs(4326), xlim = c(13, 21), ylim = c(41.5, 46))
dev.off()