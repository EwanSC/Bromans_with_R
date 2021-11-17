# Ewan Trying out R Code for Cleaning Epigraphic Data
# Created by: EC 2021/11/05
# Last Edit: EC 2021/11/16

# first, lets load packages

library(tidyverse) #installing tidyvery

library(RColorBrewer) #installing colour package for plot, used by DAACS

library(ggplot2) #installing package for plotting data

library(sf) #geographic plotting package

library(maps) #maps

library(mapdata) #higher res maps

library(rnaturalearth) #more maps stuff - for map origins instead of (maps)

library(rnaturalearthdata)

library(readr)

default_crs = sf::st_crs(4326)

AllDalmatiaEpig <- read.csv('whole_dalmatia_epigr_scrape.csv') # Importing our first dataframe (df)

str(AllDalmatiaEpig) # lets have a look at the structure of the data

head(AllDalmatiaEpig) # what are the first 6 rows

AllDalmatiaByDate <- AllDalmatiaEpig %>% #makes a df arranged by date
  arrange(dating.from, dating.to)

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

# thats still a lot of values so lets try 50+
DPSampleFifty <- AllDalmatiaPlace[which(AllDalmatiaPlace$n >= 50),]

# bar plot attempt with angled x-axis names for the list of places with 50+ inscriptions
ggplot(DPSampleFifty, aes(x=place, y=n, fill=place)) +
  geom_bar(stat="identity", show.legend=F, width = 1) +
  theme(axis.text.x = element_text(angle = 45, hjust=1))

# lets make a bar plot that removes the noise of Salona (5000ish inscriptions gets in the way)
# to do so I will make a df that has a range between 30-1000 and then make this into a barplot
DPSampleThirtyToThousand <- AllDalmatiaPlace %>%
  filter(n %in% (30:1000))

theme_set(theme_bw(base_size = 10))
ggplot(DPSampleThirtyToThousand, aes(x=place, y=n, fill=place)) +
  geom_bar(stat="identity", show.legend=F, width = 1) +
  theme(axis.text.x = element_text(angle = 45, hjust=1))

# Now lets try get all the lat long
AllDalmatiaLatLon <- (AllDalmatiaEpig %>%
  select(place,Longitude,Latitude)) %>%
  group_by(place) %>%
  count(place,Longitude,Latitude) %>%
  arrange(desc(n))

#now lets ignore nulls/NAs
DLatLonNoNULL <- na.omit(AllDalmatiaEpig %>%
  select(place,Longitude,Latitude)) %>%
  group_by(place) %>%
  count(place,Longitude,Latitude) %>%
  arrange(desc(n))

# lets make it into sf for mapping
(DLatLonNNAll <- st_as_sf(DLatLonNoNULL, coords = c('Longitude', 'Latitude'), 
                           crs = 4326, agr = "constant"))

(DLatLonNN30plus <- st_as_sf(DLatLonNoNULL[which(AllDalmatiaPlace$n >= 30),], coords = c('Longitude', 'Latitude'), 
                          crs = 4326, agr = "constant"))

# now lets try and plot with this with only places between 30-1000 inscriptions
(DLatLonNNPlot <- st_as_sf(DLatLonNoNULL, coords = c('Longitude', 'Latitude'), 
                          crs = 4326, agr = "constant") %>%
                          filter (n %in% (30:1000)))

# now plot it
ggplot(DLatLonNNPlot) + 
  geom_sf(aes(color = n)) 
  
# now plot it on a map
# first, get world map
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

# First plot all locations on a map
ggplot(data = world) +
  geom_sf(color = "black", fill = "lightblue") +
  geom_sf(data = DLatLonNNAll, size = 1, colour = "orange", fill = "orange") + 
  coord_sf(xlim = c(10, 24), ylim = c(35, 49), expand = FALSE)

# now 30-1000
ggplot(data = world) +
  geom_sf(color = "black", fill = "lightblue") +
  geom_sf(data = DLatLonNNPlot, size = 1, colour = "orange", fill = "orange") + 
  coord_sf(xlim = c(10, 24), ylim = c(35, 49), expand = FALSE)

# now plot on map with gradient and key and zoomed
ggplot(data = world) +
  geom_sf(color = "black", fill = "lightgreen") +
  geom_sf(data = DLatLonNNPlot, aes(color = n), size = 2) + 
  coord_sf(default_crs = st_crs(4326), xlim = c(13, 21), ylim = c(41.5, 46))

# now a plot with scaled points and zoomed
ggplot(data = world) +
  geom_sf(color = "black", fill = "lightgreen") +
  geom_sf(data = DLatLonNNPlot, aes(size = n), alpha=0.6) + 
  coord_sf(default_crs = st_crs(4326), xlim = c(13, 21), ylim = c(41.5, 46))

# now with Salona
ggplot(data = world) +
  geom_sf(color = "black", fill = "lightgreen") +
  geom_sf(data = DLatLonNN30plus, aes(size = n), alpha=0.6) + 
  coord_sf(default_crs = st_crs(4326), xlim = c(13, 21), ylim = c(41.5, 46))

# these maps are bad for the islands though, so lets try a more detailed one

# lets add eastern Italy's epigraphy too!
ItalyDalEpig <- read_tsv("2021-11-17-EDCS_via_Lat_Epig-prov_5(ApetCa,Da,PiRe,SaRe,UmRe)-31286.tsv")

str(ItalyDalEpig)
head(ItalyDalEpig)

ItalyDalEpigPlace <-  na.omit(ItalyDalEpig %>%
  select(place,Longitude,Latitude)) %>%
  group_by(place) %>%
  count(place,Longitude,Latitude) %>%
  arrange(desc(n))

#with Salona
(IDEPthirty <- st_as_sf(ItalyDalEpigPlace[which(ItalyDalEpigPlace$n >= 30),], coords = c('Longitude', 'Latitude'), 
                        crs = 4326, agr = "constant"))

# without Salona
(IDEPthirtythousand <- st_as_sf(ItalyDalEpigPlace, coords = c('Longitude', 'Latitude'), 
                        crs = 4326, agr = "constant") %>%
                        filter (n %in% (30:1000)))

# now on a map with Salona
ggplot(data = world) +
  geom_sf(color = "black", fill = "lightgreen") +
  geom_sf(data = IDEPthirty, aes(size = n), alpha=0.6) + 
  coord_sf(default_crs = st_crs(4326), xlim = c(13, 21), ylim = c(41.5, 46))

# now on a map without Salona
ggplot(data = world) +
  geom_sf(color = "black", fill = "lightgreen") +
  geom_sf(data = IDEPthirtythousand, aes(size = n), alpha=0.6) + 
  coord_sf(default_crs = st_crs(4326), xlim = c(13, 21), ylim = c(41.5, 46))

# now only those dating between -30 - 150 CE
IDE1cent <- ItalyDalEpig %>%
  filter(ItalyDalEpig$`dating from` %in% (-30:100), ItalyDalEpig$`dating to` %in% (9:150))

IDE1centplace <- na.omit(IDE1cent %>% #makes a df that provides counts of distinct places
                               select(place,Longitude,Latitude) %>%
                               group_by(place) %>%
                               count(place,Longitude,Latitude) %>%
                               arrange(desc(n))) 

(IDE1centplacesf <- st_as_sf(IDE1centplace, coords = c('Longitude', 'Latitude'), 
                                 crs = 4326, agr = "constant"))

ggplot(data = world) +
  geom_sf(color = "black", fill = "lightgreen") +
  geom_sf(data = IDE1centplacesf, aes(size = n), alpha=0.6) + 
  coord_sf(default_crs = st_crs(4326), xlim = c(13, 21), ylim = c(41.5, 46))
