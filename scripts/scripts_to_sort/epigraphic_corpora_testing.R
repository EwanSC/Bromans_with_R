# Lets scrape the corpus for https://github.com/EwanSC/ThesisBromans
# Created by: EC 2021/11/16
# Last Edit: EC 2021/11/16

# first, lets load packages

library(tidyverse) #installing tidyvery

library(RColorBrewer) #installing colour package for plot, used by DAACS

library(ggplot2)

library(sf) #geographic plotting package

library(maps) #maps

library(mapdata) #higher res maps

library(rnaturalearth) #more maps stuff - for map origins instead of (maps)

library(rnaturalearthdata)

library(ggmap)

library(dplyr)

library(stringr)

default_crs = sf::st_crs(4326) # setting the default map projection to 4326

# now the data, all epigraphy from 'Dalmatia'

AllDalmatiaEpig <- read_tsv('data/2021-11-16-EDCS_via_Lat_Epig-prov_Dalmatia-10140.tsv') # Importing our first dataframe (df)

str(AllDalmatiaEpig) # lets have a look at the structure of the data

head(AllDalmatiaEpig) # what are the first 6 rows

AllDalmatiaByDate <- AllDalmatiaEpig %>% #makes a df arranged by date
  arrange("dating from", "dating to")

# now to limit the data by date range date-to being 1-100 CE
AD1cent <- AllDalmatiaByDate %>%
  filter(`dating from` %in% (1:100), `dating to` %in% (1:100))

# but what if it is 1-150 date range?
AD1centrange <- AllDalmatiaByDate %>%
  filter(`dating from` %in% (-30:100), `dating to` %in% (9:150)) %>%
  arrange("dating to", "dating from")

# lets see what the distribution looks like on a map
# first, group by place and then make df into an sf
AD1centrangeplace <- na.omit(AD1centrange %>% #makes a df that provides counts of distinct places
  select(place,Longitude,Latitude) %>%
  group_by(place) %>%
  count(place,Longitude,Latitude) %>%
  arrange(desc(n))) 

(AD1centrangeplacesf <- st_as_sf(AD1centrangeplace, coords = c('Longitude', 'Latitude'), 
                          crs = 4326, agr = "constant"))

# now make the world map
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

# now to plot with scaled points and zoomed in 
ggplot(data = world) +
  geom_sf(color = "black", fill = "lightgreen") +
  geom_sf(data = AD1centrangeplacesf, aes(size = n), alpha=0.6) + 
  coord_sf(default_crs = st_crs(4326), xlim = c(13, 21), ylim = c(41.5, 46))

# now lets try filter the inscriptions to include military keywords
AD1centrangemilit <- AD1centrange %>%
  select(`inscription interpretive cleaning`, contains("mil"))
# this doesn't work... why? 
