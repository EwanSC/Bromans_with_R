# trying out load data function and map
# please first run primary_epigraphic_cor(e)pus.R and primary_map.R

epigdatafirstcent <- load_epig_data("data/2022-08-29-EDCS_via_Lat_Epig-prov_Dalmatia-10140.json") %>% #makes a df that provides counts of distinct place
  group_by(place) %>%
  count(place) %>%
  arrange(desc(n)) 

epigdatafirstcentsample <- epigdatafirstcent[which(epigdatafirstcent$n >= 10),]

ggplot(epigdatafirstcentsample, aes(x=place, y=n, fill=place)) +
  geom_bar(stat="identity", show.legend=F, width = 1) +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  ggtitle("1st Cent. CE Dalmatian sites with 10+ inscriptions") +
  xlab("Site: Modern/Ancient") + 
  ylab("Number of inscriptions")

epigdatafirstcent <- load_epig_data("data/2022-08-29-EDCS_via_Lat_Epig-prov_Dalmatia-10140.json") 

epigdatafirstcentLL <- na.omit(epigdatafirstcent %>% #makes a df that provides counts of distinct places
                                 select(place,Longitude,Latitude) %>%
                                 group_by(place) %>%
                                 count(place,Longitude,Latitude) %>%
                                 arrange(desc(n))) 

(epigdatafirstcentLLSF <- st_as_sf(epigdatafirstcentLL, coords = c('Longitude', 'Latitude'), 
                                   crs = 4326, agr = "constant"))

ggplot() + 
  geom_sf(data = world, color = "darkgrey", fill = "lightgrey") + 
  geom_sf(data = roman_69_provinces, colour = 'black') +
  geom_sf(data = epigdatafirstcentLLSF, aes(size = n), alpha=0.6) + 
  labs(size = 'Monuments') +
  ggtitle("Dalmatia 1st c. CE", subtitle = "Epigraphic Distribution") +
  coord_sf(default_crs = st_crs(4326), xlim = c(13, 21), ylim = c(41.5, 46))