#trying to use LIRE
#started 5/9/2024
# edited 6/09/2024

library(dplyr)
library(sqldf)
library(arrow)
library(ggplot2)
library(sf)
library(ggrepel)
library("gridExtra")


## FIRST RUN /scripts/2.primary_map.R

#get all data + vocab
LIRE <- read_parquet("https://zenodo.org/records/8431452/files/LIRE_v3-0.parquet?download=1")
LIRE_dictionary <- read.csv("https://zenodo.org/records/8431452/files/LI_metadata.csv?download=1")
write.csv(LIRE_dictionary, file = "data/LIRE_dictionary.csv")

#filter columns
LIRE_filtered <- select(LIRE, 
                        "LIST-ID", 
                        "EDCS-ID", 
                        "EDH-ID", 
                        "not_before", 
                        "not_after", 
                        "inscription", 
                        "transcription", 
                        "clean_text_interpretive_word", 
                        "type_of_inscription_clean",
                        "type_of_monument_clean",
                        "province", 
                        "province_label_clean", 
                        "place", 
                        "findspot_ancient_clean", 
                        "findspot_modern_clean", 
                        "status_notation",
                        "Latitude",
                        "Longitude")

#get only Dalmatia
LIRE_Dal <- filter(LIRE_filtered, province == "Dalmatia")
LIRE_Dal_clean <- filter(LIRE_filtered, province_label_clean == "Dalmatia")

#clean places
clean_LIRE_Dal_places <- sqldf("Select Distinct 
                        place, findspot_ancient_clean, findspot_modern_clean, Longitude, Latitude 
                        from LIRE_Dal
                        order by Latitude desc, Longitude asc")
clean_LIRE_Dal_clean_places <- sqldf("Select Distinct 
                              place, findspot_ancient_clean, findspot_modern_clean, Longitude, Latitude 
                              from LIRE_Dal_clean
                              order by Latitude desc, Longitude asc")

#now military (unclean, clean, dated)
LIRE_Dal_mil <- sqldf("Select * from LIRE_Dal
                  WHERE clean_text_interpretive_word 
                    LIKE '%legio%'
                  OR clean_text_interpretive_word 
                    LIKE '%cohor%'
                  OR clean_text_interpretive_word 
                    LIKE '%ala%'
                  OR clean_text_interpretive_word 
                    LIKE '%alae%'
                  OR clean_text_interpretive_word 
                    LIKE '%milit%'
                  OR clean_text_interpretive_word 
                    LIKE '%eques%'
                  OR clean_text_interpretive_word 
                    LIKE '%equit%'
                  OR clean_text_interpretive_word 
                    LIKE '%duplicari%'
                  OR clean_text_interpretive_word 
                    LIKE '%veteran%'
                  or clean_text_interpretive_word
                    LIKE '%centuri%'
                  or clean_text_interpretive_word
                    LIKE '%immun%'
                  or clean_text_interpretive_word
                    LIKE '%miles%'
                  or clean_text_interpretive_word
                    LIKE '%beneficiar%'
                  or clean_text_interpretive_word
                    LIKE '%tesserari%'
                  or clean_text_interpretive_word
                    LIKE '%signifer%'
                  or clean_text_interpretive_word
                    LIKE '%aquilifer%'
                  or clean_text_interpretive_word
                    LIKE '%imaginifer%'
                  or clean_text_interpretive_word
                    LIKE '%corniculari%'
                  or clean_text_interpretive_word
                    LIKE '%principalis%'
                  or clean_text_interpretive_word
                    LIKE '%primus pilus%'
                  or clean_text_interpretive_word
                    LIKE '%primo pilo%'
                  or clean_text_interpretive_word
                    LIKE '%primi pili%'
                  or clean_text_interpretive_word
                    LIKE '%praefectus castrorum%'
                  or clean_text_interpretive_word
                    LIKE '%optio %'
                  or clean_text_interpretive_word
                    LIKE '%option%'
                  or status_notation
                    LIKE '%milites%'
                  OR findspot_ancient_clean = 'Tilurium'
                  OR findspot_ancient_clean = 'Burnum'
                  OR findspot_ancient_clean = 'Andetrium'
                  OR findspot_ancient_clean = 'Bigeste'
                  OR findspot_modern_clean = 'Ljubuški'
                  ")

write.csv(LIRE_Dal_mil, file = "data/LIRE_Dal_mil.csv")

LIRE_Dal_mil_clean <- sqldf("Select * from LIRE_Dal_clean
                  WHERE clean_text_interpretive_word 
                    LIKE '%legio%'
                  OR clean_text_interpretive_word 
                    LIKE '%cohor%'
                  OR clean_text_interpretive_word 
                    LIKE '%ala%'
                  OR clean_text_interpretive_word 
                    LIKE '%alae%'
                  OR clean_text_interpretive_word 
                    LIKE '%milit%'
                  OR clean_text_interpretive_word 
                    LIKE '%eques%'
                  OR clean_text_interpretive_word 
                    LIKE '%equit%'
                  OR clean_text_interpretive_word 
                    LIKE '%duplicari%'
                  OR clean_text_interpretive_word 
                    LIKE '%veteran%'
                  or clean_text_interpretive_word
                    LIKE '%centuri%'
                  or clean_text_interpretive_word
                    LIKE '%immun%'
                  or clean_text_interpretive_word
                    LIKE '%miles%'
                  or clean_text_interpretive_word
                    LIKE '%beneficiar%'
                  or clean_text_interpretive_word
                    LIKE '%tesserari%'
                  or clean_text_interpretive_word
                    LIKE '%signifer%'
                  or clean_text_interpretive_word
                    LIKE '%aquilifer%'
                  or clean_text_interpretive_word
                    LIKE '%imaginifer%'
                  or clean_text_interpretive_word
                    LIKE '%corniculari%'
                  or clean_text_interpretive_word
                    LIKE '%principalis%'
                  or clean_text_interpretive_word
                    LIKE '%primus pilus%'
                  or clean_text_interpretive_word
                    LIKE '%primo pilo%'
                  or clean_text_interpretive_word
                    LIKE '%primi pili%'
                  or clean_text_interpretive_word
                    LIKE '%praefectus castrorum%'
                  or clean_text_interpretive_word
                    LIKE '%optio %'
                  or clean_text_interpretive_word
                    LIKE '%option%'
                  or status_notation
                    LIKE '%milites%'
                  OR findspot_ancient_clean = 'Tilurium'
                  OR findspot_ancient_clean = 'Burnum'
                  OR findspot_ancient_clean = 'Andetrium'
                  OR findspot_ancient_clean = 'Bigeste'
                  OR findspot_modern_clean = 'Ljubuški'
                  ")

write.csv(LIRE_Dal_mil_clean, file = "data/LIRE_Dal_mil_clean_province.csv")

##now dated
LIRE_dated <- LIRE_Dal %>%
    filter(not_before %in% (-30:191), not_after %in% (1:200)) %>%
    arrange(not_after, not_before)

LIRE_dated_mil <- sqldf("Select * from LIRE_dated
                  WHERE clean_text_interpretive_word 
                    LIKE '%legio%'
                  OR clean_text_interpretive_word 
                    LIKE '%cohor%'
                  OR clean_text_interpretive_word 
                    LIKE '%ala%'
                  OR clean_text_interpretive_word 
                    LIKE '%alae%'
                  OR clean_text_interpretive_word 
                    LIKE '%milit%'
                  OR clean_text_interpretive_word 
                    LIKE '%eques%'
                  OR clean_text_interpretive_word 
                    LIKE '%equit%'
                  OR clean_text_interpretive_word 
                    LIKE '%duplicari%'
                  OR clean_text_interpretive_word 
                    LIKE '%veteran%'
                  or clean_text_interpretive_word
                    LIKE '%centuri%'
                  or clean_text_interpretive_word
                    LIKE '%immun%'
                  or clean_text_interpretive_word
                    LIKE '%miles%'
                  or clean_text_interpretive_word
                    LIKE '%beneficiar%'
                  or clean_text_interpretive_word
                    LIKE '%tesserari%'
                  or clean_text_interpretive_word
                    LIKE '%signifer%'
                  or clean_text_interpretive_word
                    LIKE '%aquilifer%'
                  or clean_text_interpretive_word
                    LIKE '%imaginifer%'
                  or clean_text_interpretive_word
                    LIKE '%corniculari%'
                  or clean_text_interpretive_word
                    LIKE '%principalis%'
                  or clean_text_interpretive_word
                    LIKE '%primus pilus%'
                  or clean_text_interpretive_word
                    LIKE '%primo pilo%'
                  or clean_text_interpretive_word
                    LIKE '%primi pili%'
                  or clean_text_interpretive_word
                    LIKE '%praefectus castrorum%'
                  or clean_text_interpretive_word
                    LIKE '%optio %'
                  or clean_text_interpretive_word
                    LIKE '%option%'
                  or status_notation
                    LIKE '%milites%'
                  OR findspot_ancient_clean = 'Tilurium'
                  OR findspot_ancient_clean = 'Burnum'
                  OR findspot_ancient_clean = 'Andetrium'
                  OR findspot_ancient_clean = 'Bigeste'
                  OR findspot_modern_clean = 'Ljubuški'
                  ")

LIRE_clean_dated <- LIRE_Dal_clean %>%
  filter(not_before %in% (-30:191), not_after %in% (1:200)) %>%
  arrange(not_after, not_before)

LIRE_clean_dated_mil <- sqldf("Select * from LIRE_clean_dated
                  WHERE clean_text_interpretive_word 
                    LIKE '%legio%'
                  OR clean_text_interpretive_word 
                    LIKE '%cohor%'
                  OR clean_text_interpretive_word 
                    LIKE '%ala%'
                  OR clean_text_interpretive_word 
                    LIKE '%alae%'
                  OR clean_text_interpretive_word 
                    LIKE '%milit%'
                  OR clean_text_interpretive_word 
                    LIKE '%eques%'
                  OR clean_text_interpretive_word 
                    LIKE '%equit%'
                  OR clean_text_interpretive_word 
                    LIKE '%duplicari%'
                  OR clean_text_interpretive_word 
                    LIKE '%veteran%'
                  or clean_text_interpretive_word
                    LIKE '%centuri%'
                  or clean_text_interpretive_word
                    LIKE '%immun%'
                  or clean_text_interpretive_word
                    LIKE '%miles%'
                  or clean_text_interpretive_word
                    LIKE '%beneficiar%'
                  or clean_text_interpretive_word
                    LIKE '%tesserari%'
                  or clean_text_interpretive_word
                    LIKE '%signifer%'
                  or clean_text_interpretive_word
                    LIKE '%aquilifer%'
                  or clean_text_interpretive_word
                    LIKE '%imaginifer%'
                  or clean_text_interpretive_word
                    LIKE '%corniculari%'
                  or clean_text_interpretive_word
                    LIKE '%principalis%'
                  or clean_text_interpretive_word
                    LIKE '%primus pilus%'
                  or clean_text_interpretive_word
                    LIKE '%primo pilo%'
                  or clean_text_interpretive_word
                    LIKE '%primi pili%'
                  or clean_text_interpretive_word
                    LIKE '%praefectus castrorum%'
                  or clean_text_interpretive_word
                    LIKE '%optio %'
                  or clean_text_interpretive_word
                    LIKE '%option%'
                  or status_notation
                    LIKE '%milites%'
                  OR findspot_ancient_clean = 'Tilurium'
                  OR findspot_ancient_clean = 'Burnum'
                  OR findspot_ancient_clean = 'Andetrium'
                  OR findspot_ancient_clean = 'Bigeste'
                  OR findspot_modern_clean = 'Ljubuški'
                  ")

# filter types
##show types and count
LIRE_Dal_mil %>% count(type_of_monument_clean, type_of_inscription_clean)
LIRE_Dal_mil_clean %>% count(type_of_monument_clean, type_of_inscription_clean)
LIRE_dated_mil %>% count(type_of_monument_clean, type_of_inscription_clean)
LIRE_clean_dated_mil %>% count(type_of_monument_clean, type_of_inscription_clean)


##filter /remove building/dedicatory inscription; honorific inscription; boundary inscription; mile-/leaguestone;
# military diploma(type_of_inscription_clean); mile-/leaguestone; tile (type_of_monument_clean)

LIRE_Dal_corpus <- LIRE_Dal_mil %>%
  filter(!type_of_monument_clean %in% c('architectural member',
                                        'mile-/leaguestone', 
                                        'tile', 
                                        'instrumentum domesticum',
                                        'table',
                                        'instrumentum militare') 
        & !type_of_inscription_clean %in% c('building/dedicatory inscription', 
                                            'honorific inscription', 
                                            'boundary inscription', 
                                            'mile-/leaguestone', 
                                            'military diploma',
                                            'building/dedicatory inscription',
                                            'list'))

LIRE_Dal_corpus_clean <- LIRE_Dal_mil_clean %>%
  filter(!type_of_monument_clean %in% c('architectural member',
                                        'mile-/leaguestone', 
                                        'tile', 
                                        'instrumentum domesticum',
                                        'table',
                                        'instrumentum militare') 
         & !type_of_inscription_clean %in% c('architectural member',
                                             'remove building/dedicatory inscription', 
                                             'honorific inscription', 
                                             'boundary inscription', 
                                             'mile-/leaguestone', 
                                             'military diploma',
                                             'building/dedicatory inscription',
                                             'list'))

LIRE_dated_corpus <- LIRE_dated_mil %>%
  filter(!type_of_monument_clean %in% c('architectural member',
                                        'mile-/leaguestone', 
                                        'tile', 
                                        'instrumentum domesticum',
                                        'table',
                                        'instrumentum militare') 
        & !type_of_inscription_clean %in% c('building/dedicatory inscription', 
                                            'honorific inscription', 
                                            'boundary inscription', 
                                            'mile-/leaguestone', 
                                            'military diploma',
                                            'building/dedicatory inscription',
                                            'list'))

LIRE_clean_dated_corpus <- LIRE_clean_dated_mil %>%
  filter(!type_of_monument_clean %in% c('architectural member',
                                        'mile-/leaguestone', 
                                        'tile', 
                                        'instrumentum domesticum',
                                        'table',
                                        'instrumentum militare') 
         & !type_of_inscription_clean %in% c('architectural member',
                                             'remove building/dedicatory inscription', 
                                             'honorific inscription', 
                                             'boundary inscription', 
                                             'mile-/leaguestone', 
                                             'military diploma',
                                             'building/dedicatory inscription',
                                             'list'))
#check data
LIRE_Dal_corpus %>% count(type_of_monument_clean, type_of_inscription_clean)
LIRE_Dal_corpus_clean %>% count(type_of_monument_clean, type_of_inscription_clean)
LIRE_dated_corpus %>% count(type_of_monument_clean, type_of_inscription_clean)
LIRE_clean_dated_corpus %>% count(type_of_monument_clean, type_of_inscription_clean)

#save data
write.csv(LIRE_Dal_corpus, file = "data/LIRE_corpus_monuments.csv")
write.csv(LIRE_Dal_corpus_clean, file = "data/LIRE_clean_corpus_monuments.csv")
write.csv(LIRE_dated_corpus, file = "data/LIRE_corpus_monuments_dated.csv")
write.csv(LIRE_clean_dated_corpus, file = "data/LIRE_clean_corpus_monuments_dated.csv")

#add value for count
LIRE_Dal_corpus$count<- 1
LIRE_Dal_corpus_clean$count<- 1
LIRE_dated_corpus$count<- 1
LIRE_clean_dated_corpus$count<- 1

#now to plot on scatter map
## create df for layer of key sites 

key_sites <- data.frame(findspot_ancient_clean=c('Tilurium', 'Salona', 'Burnum'),
                        Longitude=c(16.7216523938 , 16.483426 , 16.025622),
                        Latitude=c(43.609647549, 43.539561, 44.018914))

print(key_sites)

(key_sites_ll <- st_as_sf(key_sites, coords = c('Longitude', 'Latitude'), remove = FALSE,
                                crs = 4326, agr = "constant"))

## plot on map
LIRE_Dal_corpus_place <- na.omit(LIRE_Dal_corpus %>%
                                  select(findspot_ancient_clean,Longitude,Latitude) %>%
                                  group_by(findspot_ancient_clean) %>%
                                  count(findspot_ancient_clean,Longitude,Latitude) %>%
                                  arrange(desc(n)))
 
(LIRE_Dal_corpus_ll <- st_as_sf(LIRE_Dal_corpus_place, coords = c('Longitude', 'Latitude'), remove = FALSE,
                                     crs = 4326, agr = "constant"))

ggplot() + 
  geom_sf(data = world, color = "grey", fill = "lightgrey") + 
  geom_sf(data = roman_roads, colour = 'grey30', size = 0.6) +
  geom_sf(data = roman_69_provinces, colour = 'black', size = 0.8) +
  geom_sf(data = roman_settlements, colour = 'grey30', alpha=0.6, size = 0.8) +
  geom_sf(data = LIRE_Dal_corpus_ll, aes(size = n), alpha=0.8, colour = 'sienna1') +
  geom_sf(data = key_sites_ll, colour = 'black', size = 1) +
  geom_text_repel(data = key_sites_ll, aes(x = Longitude, y = Latitude,label = findspot_ancient_clean), 
                  nudge_x = c(-2, -1.5, -1), 
                  nudge_y = c(-0.25,-0.25,-0.25)) +
  labs(size = 'Monument density') +
  labs(caption = "Based on data from LIRE v.3.0") +
  ggtitle("Distribution of military funerary and sacral monuments",
          subtitle = "Dalmatia") +
  coord_sf(default_crs = st_crs(4326), xlim = c(13, 21), ylim = c(41.5, 46)) +
  theme_void()

ggsave("output_images/LIRE_corpus_scatter.jpeg", dpi = 300)

LIRE_Dal_corpus_clean_place <- na.omit(LIRE_Dal_corpus_clean %>%
                                   select(findspot_ancient_clean,Longitude,Latitude) %>%
                                   group_by(findspot_ancient_clean) %>%
                                   count(findspot_ancient_clean,Longitude,Latitude) %>%
                                   arrange(desc(n)))

(LIRE_Dal_corpus_clean_ll <- st_as_sf(LIRE_Dal_corpus_clean_place, coords = c('Longitude', 'Latitude'), remove = FALSE,
                                crs = 4326, agr = "constant"))
ggplot() + 
  geom_sf(data = world, color = "grey", fill = "lightgrey") + 
  geom_sf(data = roman_roads, colour = 'grey30', size = 0.6) +
  geom_sf(data = roman_69_provinces, colour = 'black', size = 0.8) +
  geom_sf(data = roman_settlements, colour = 'black', alpha=0.6, size = 0.8) +
  geom_sf(data = LIRE_Dal_corpus_clean_ll, aes(size = n), alpha=0.8, colour = 'sienna1') +
  geom_sf(data = key_sites_ll, colour = 'black', size = 1) +
  geom_text_repel(data = key_sites_ll, aes(x = Longitude, y = Latitude,label = findspot_ancient_clean), 
                  nudge_x = c(-2, -1.5, -1), 
                  nudge_y = c(-0.25,-0.25,-0.25)) +
  labs(size = 'Monument density') +
  labs(caption = "Based on data from LIRE v.3.0 (Clean province = Dalmatia)") +
  ggtitle("Distribution of military funerary and sacral monuments",
          subtitle = "Dalmatia") +
  coord_sf(default_crs = st_crs(4326), xlim = c(13, 21), ylim = c(41.5, 46)) +
  theme_void()

ggsave("output_images/LIRE_clean_corpus_scatter.jpeg", dpi = 300)

## create one for just relevant period (~200 CE cut off)
LIRE_dated_corpus_place <- na.omit(LIRE_dated_corpus %>%
                                  select(findspot_ancient_clean,Longitude,Latitude) %>%
                                  group_by(findspot_ancient_clean) %>%
                                  count(findspot_ancient_clean,Longitude,Latitude) %>%
                                  arrange(desc(n)))
 
(LIRE_dated_corpus_ll <- st_as_sf(LIRE_dated_corpus_place, coords = c('Longitude', 'Latitude'), remove = FALSE,
                                     crs = 4326, agr = "constant"))

ggplot() + 
  geom_sf(data = world, color = "grey", fill = "lightgrey") + 
  geom_sf(data = roman_roads, colour = 'grey30', size = 0.6) +
  geom_sf(data = roman_69_provinces, colour = 'black', size = 0.8) +
  geom_sf(data = roman_settlements, colour = 'grey30', alpha=0.6, size = 0.8) +
  geom_sf(data = LIRE_dated_corpus_ll, aes(size = n), alpha=0.8, colour = 'sienna1') +
  geom_sf(data = key_sites_ll, colour = 'black', size = 1) +
  geom_text_repel(data = key_sites_ll, aes(x = Longitude, y = Latitude,label = findspot_ancient_clean), 
                  nudge_x = c(-2, -1.5, -1), 
                  nudge_y = c(-0.25,-0.25,-0.25)) +
  labs(size = 'Monument density') +
  labs(caption = "Based on data from LIRE v.3.0") +
  ggtitle("Distribution of military funerary and sacral monuments",
          subtitle = "Dalmatia: Julio-Claudians to Antonines") +
  coord_sf(default_crs = st_crs(4326), xlim = c(13, 21), ylim = c(41.5, 46)) +
  theme_void()

ggsave("output_images/LIRE_dated_corpus_scatter.jpeg", dpi = 300)

LIRE_clean_dated_corpus_place <- na.omit(LIRE_clean_dated_corpus %>%
                                  select(findspot_ancient_clean,Longitude,Latitude) %>%
                                  group_by(findspot_ancient_clean) %>%
                                  count(findspot_ancient_clean,Longitude,Latitude) %>%
                                  arrange(desc(n)))
 
(LIRE_clean_dated_corpus_ll <- st_as_sf(LIRE_clean_dated_corpus_place, coords = c('Longitude', 'Latitude'), remove = FALSE,
                                     crs = 4326, agr = "constant"))

ggplot() + 
  geom_sf(data = world, color = "grey", fill = "lightgrey") + 
  geom_sf(data = roman_roads, colour = 'grey30', size = 0.6) +
  geom_sf(data = roman_69_provinces, colour = 'black', size = 0.8) +
  geom_sf(data = roman_settlements, colour = 'grey30', alpha=0.6, size = 0.8) +
  geom_sf(data = LIRE_clean_dated_corpus_ll, aes(size = n), alpha=0.8, colour = 'sienna1') +
  geom_sf(data = key_sites_ll, colour = 'black', size = 1) +
  geom_text_repel(data = key_sites_ll, aes(x = Longitude, y = Latitude,label = findspot_ancient_clean), 
                  nudge_x = c(-2, -1.5, -1), 
                  nudge_y = c(-0.25,-0.25,-0.25)) +
  labs(size = 'Monument density') +
  labs(caption = "Based on data from LIRE v.3.0 (Clean province = Dalmatia)") +
  ggtitle("Distribution of military funerary and sacral monuments",
          subtitle = "Dalmatia: Julio-Claudians to Antonines") +
  coord_sf(default_crs = st_crs(4326), xlim = c(13, 21), ylim = c(41.5, 46)) +
  theme_void()

ggsave("output_images/LIRE_clean_dated_corpus_scatter.jpeg", dpi = 300)


# Now compare to all dalmatia
LIRE_all_Dal <- LIRE_Dal %>%
  filter(!type_of_monument_clean %in% c('architectural member',
                                        'mile-/leaguestone', 
                                        'tile', 
                                        'instrumentum domesticum',
                                        'table',
                                        'instrumentum militare') 
         & !type_of_inscription_clean %in% c('architectural member',
                                             'remove building/dedicatory inscription', 
                                             'honorific inscription', 
                                             'boundary inscription', 
                                             'mile-/leaguestone', 
                                             'military diploma',
                                             'building/dedicatory inscription',
                                             'list'))

LIRE_all_Dal_clean <- LIRE_Dal_clean %>%
  filter(!type_of_monument_clean %in% c('architectural member',
                                        'mile-/leaguestone', 
                                        'tile', 
                                        'instrumentum domesticum',
                                        'table',
                                        'instrumentum militare') 
         & !type_of_inscription_clean %in% c('architectural member',
                                             'remove building/dedicatory inscription', 
                                             'honorific inscription', 
                                             'boundary inscription', 
                                             'mile-/leaguestone', 
                                             'military diploma',
                                             'building/dedicatory inscription',
                                             'list'))

LIRE_all_Dal_dated <- LIRE_Dal %>%
  filter(!type_of_monument_clean %in% c('architectural member',
                                        'mile-/leaguestone', 
                                        'tile', 
                                        'instrumentum domesticum',
                                        'table',
                                        'instrumentum militare') 
         & !type_of_inscription_clean %in% c('architectural member',
                                             'remove building/dedicatory inscription', 
                                             'honorific inscription', 
                                             'boundary inscription', 
                                             'mile-/leaguestone', 
                                             'military diploma',
                                             'building/dedicatory inscription',
                                             'list') 
         & not_before %in% (-30:191) & not_after %in% (1:200)) %>%
          arrange(not_after, not_before)

LIRE_all_Dal_clean_dated <- LIRE_Dal_clean %>%
  filter(!type_of_monument_clean %in% c('architectural member',
                                        'mile-/leaguestone', 
                                        'tile', 
                                        'instrumentum domesticum',
                                        'table',
                                        'instrumentum militare') 
         & !type_of_inscription_clean %in% c('architectural member',
                                             'remove building/dedicatory inscription', 
                                             'honorific inscription', 
                                             'boundary inscription', 
                                             'mile-/leaguestone', 
                                             'military diploma',
                                             'building/dedicatory inscription',
                                             'list') 
         & not_before %in% (-30:191) & not_after %in% (1:200)) %>%
          arrange(not_after, not_before)

LIRE_all_Dal$count<- 1
LIRE_all_Dal_clean$count<- 1
LIRE_all_Dal_dated<- 1
LIRE_all_Dal_clean_dated<- 1

LIRE_all_Dal_place <- na.omit(LIRE_all_Dal %>%
                                   select(findspot_ancient_clean,Longitude,Latitude) %>%
                                   group_by(findspot_ancient_clean) %>%
                                   count(findspot_ancient_clean,Longitude,Latitude) %>%
                                   arrange(desc(n)))

(LIRE_all_Dal_ll <- st_as_sf(LIRE_all_Dal_place, coords = c('Longitude', 'Latitude'), remove = FALSE,
                                crs = 4326, agr = "constant"))

ggplot() + 
  geom_sf(data = world, color = "grey", fill = "lightgrey") + 
  geom_sf(data = roman_roads, colour = 'grey30', size = 0.6) +
  geom_sf(data = roman_69_provinces, colour = 'black', size = 0.8) +
  geom_sf(data = roman_settlements, colour = 'black', alpha=0.6, size = 0.8) +
  geom_sf(data = LIRE_all_Dal_ll, aes(size = n), alpha=0.8, colour = 'brown') +
  geom_sf(data = key_sites_ll, colour = 'black', size = 1) +
  geom_text_repel(data = key_sites_ll, aes(x = Longitude, y = Latitude,label = findspot_ancient_clean), 
                  nudge_x = c(-2, -1.5, -1), 
                  nudge_y = c(-0.25,-0.25,-0.25)) +
  labs(size = 'Monument density') +
  labs(caption = "Based on data from LIRE v.3.0") +
  ggtitle("Distribution of funerary and sacral monuments in Dalmatia",
          subtitle = "Dalmatia") +
  coord_sf(default_crs = st_crs(4326), xlim = c(13, 21), ylim = c(41.5, 46)) +
  theme_void()

ggsave("output_images/LIRE_dal_scatter.jpeg", dpi = 300)

LIRE_all_Dal_clean_place <- na.omit(LIRE_all_Dal_clean %>%
                                select(findspot_ancient_clean,Longitude,Latitude) %>%
                                group_by(findspot_ancient_clean) %>%
                                count(findspot_ancient_clean,Longitude,Latitude) %>%
                                arrange(desc(n)))

(LIRE_all_Dal_clean_ll <- st_as_sf(LIRE_all_Dal_clean_place, coords = c('Longitude', 'Latitude'), remove = FALSE,
                             crs = 4326, agr = "constant"))

ggplot() + 
  geom_sf(data = world, color = "grey", fill = "lightgrey") + 
  geom_sf(data = roman_roads, colour = 'grey30', size = 0.6) +
  geom_sf(data = roman_69_provinces, colour = 'black', size = 0.8) +
  geom_sf(data = roman_settlements, colour = 'black', alpha=0.6, size = 0.8) +
  geom_sf(data = LIRE_all_Dal_clean_ll, aes(size = n), alpha=0.8, colour = 'brown') +
  geom_sf(data = key_sites_ll, colour = 'black', size = 1) +
  geom_text_repel(data = key_sites_ll, aes(x = Longitude, y = Latitude,label = findspot_ancient_clean), 
                  nudge_x = c(-2, -1.5, -1), 
                  nudge_y = c(-0.25,-0.25,-0.25)) +
  labs(size = 'Monument density') +
  labs(caption = "Based on data from LIRE v.3.0 (Clean province = Dalmatia)") +
  ggtitle("Distribution of funerary and sacral monuments in Dalmatia") +
  coord_sf(default_crs = st_crs(4326), xlim = c(13, 21), ylim = c(41.5, 46)) +
  theme_void()

ggsave("output_images/LIRE_clean_dal_scatter.jpeg", dpi = 300)

#now to combine https://www.geeksforgeeks.org/draw-multiple-ggplot2-plots-side-by-side/
## first, combine undated
plot1 <- 
  ggplot() + 
  geom_sf(data = world, color = "grey", fill = "lightgrey") + 
  geom_sf(data = roman_roads, colour = 'grey30', size = 0.6) +
  geom_sf(data = roman_69_provinces, colour = 'black', size = 0.8) +
  geom_sf(data = roman_settlements, colour = 'black', alpha=0.6, size = 0.8) +
  geom_sf(data = LIRE_Dal_corpus_ll, aes(size = n), alpha=0.8, colour = 'sienna1') + 
  geom_sf(data = key_sites_ll, colour = 'black', size = 1) +
  labs(size = 'Monument density') +
  labs(caption = "Based on data from LIRE v.3.0") +
  ggtitle("Distribution of military funerary and sacral monuments",
          subtitle = "Dalmatia") +
  coord_sf(default_crs = st_crs(4326), xlim = c(13, 21), ylim = c(41.5, 46)) +
  theme_void()

plot2 <- 
  ggplot() + 
  geom_sf(data = world, color = "grey", fill = "lightgrey") + 
  geom_sf(data = roman_roads, colour = 'grey30', size = 0.6) +
  geom_sf(data = roman_69_provinces, colour = 'black', size = 0.8) +
  geom_sf(data = roman_settlements, colour = 'black', alpha=0.6, size = 0.8) +
  geom_sf(data = LIRE_all_Dal_ll, aes(size = n), alpha=0.8, colour = 'brown') +
  geom_sf(data = key_sites_ll, colour = 'black', size = 1) +
  labs(size = 'Monument density') +
  labs(caption = "Based on data from LIRE v.3.0") +
  ggtitle("Distribution of funerary and sacral monuments",
          subtitle = "Dalmatia") +
  coord_sf(default_crs = st_crs(4326), xlim = c(13, 21), ylim = c(41.5, 46)) +
  theme_void()

doubletrouble <- grid.arrange(plot1, plot2, ncol = 2)

ggsave("output_images/LIRE_corpus_and_dal_scatter.pdf", doubletrouble, width = 11.7, height = 8.3)
ggsave("output_images/LIRE_corpus_and_dal_scatter.jpeg", doubletrouble, dpi = 600)


plot3 <- 
  ggplot() + 
  geom_sf(data = world, color = "grey", fill = "lightgrey") + 
  geom_sf(data = roman_roads, colour = 'grey30', size = 0.6) +
  geom_sf(data = roman_69_provinces, colour = 'black', size = 0.8) +
  geom_sf(data = roman_settlements, colour = 'black', alpha=0.6, size = 0.8) +
  geom_sf(data = LIRE_Dal_corpus_clean_ll, aes(size = n), alpha=0.8, colour = 'sienna1') + 
  geom_sf(data = key_sites_ll, colour = 'black', size = 1) +
  labs(size = 'Monument density') +
  labs(caption = "Based on data from LIRE v.3.0 (Clean provinces = Dalmatia)") +
  ggtitle("Distribution of military funerary and sacral monuments",
          subtitle = "Dalmatia") +
  coord_sf(default_crs = st_crs(4326), xlim = c(13, 21), ylim = c(41.5, 46)) +
  theme_void()

plot4 <- 
  ggplot() + 
  geom_sf(data = world, color = "grey", fill = "lightgrey") + 
  geom_sf(data = roman_roads, colour = 'grey30', size = 0.6) +
  geom_sf(data = roman_69_provinces, colour = 'black', size = 0.8) +
  geom_sf(data = roman_settlements, colour = 'black', alpha=0.6, size = 0.8) +
  geom_sf(data = LIRE_all_Dal_clean_ll, aes(size = n), alpha=0.8, colour = 'brown') +
  geom_sf(data = key_sites_ll, colour = 'black', size = 1) +
  labs(size = 'Monument density') +
  labs(caption = "Based on data from LIRE v.3.0 (Clean provinces = Dalmatia)") +
  ggtitle("Distribution of funerary and sacral monuments",
          subtitle = "Dalmatia") +
  coord_sf(default_crs = st_crs(4326), xlim = c(13, 21), ylim = c(41.5, 46)) +
  theme_void()

doubletroubler <- grid.arrange(plot3, plot4, ncol = 2)

ggsave("output_images/LIRE_clean_corpus_and_dal_scatter.pdf", doubletroubler, width = 11.7, height = 8.3)
ggsave("output_images/LIRE_clean_corpus_and_dal_scatter.jpeg", doubletroubler, dpi = 600)

## now to combine dated
LIRE_all_Dal_dated
LIRE_all_Dal_clean_dated

LIRE_all_Dal_place <- na.omit(LIRE_all_Dal %>%
                                   select(findspot_ancient_clean,Longitude,Latitude) %>%
                                   group_by(findspot_ancient_clean) %>%
                                   count(findspot_ancient_clean,Longitude,Latitude) %>%
                                   arrange(desc(n)))

(LIRE_all_Dal_ll <- st_as_sf(LIRE_all_Dal_place, coords = c('Longitude', 'Latitude'), remove = FALSE,
                                crs = 4326, agr = "constant"))

ggplot() + 
  geom_sf(data = world, color = "grey", fill = "lightgrey") + 
  geom_sf(data = roman_roads, colour = 'grey30', size = 0.6) +
  geom_sf(data = roman_69_provinces, colour = 'black', size = 0.8) +
  geom_sf(data = roman_settlements, colour = 'black', alpha=0.6, size = 0.8) +
  geom_sf(data = LIRE_all_Dal_ll, aes(size = n), alpha=0.8, colour = 'brown') +
  geom_sf(data = key_sites_ll, colour = 'black', size = 1) +
  geom_text_repel(data = key_sites_ll, aes(x = Longitude, y = Latitude,label = findspot_ancient_clean), 
                  nudge_x = c(-2, -1.5, -1), 
                  nudge_y = c(-0.25,-0.25,-0.25)) +
  labs(size = 'Monument density') +
  labs(caption = "Based on data from LIRE v.3.0") +
  ggtitle("Distribution of funerary and sacral monuments in Dalmatia",
          subtitle = "Dalmatia") +
  coord_sf(default_crs = st_crs(4326), xlim = c(13, 21), ylim = c(41.5, 46)) +
  theme_void()

ggsave("output_images/LIRE_dal_scatter.jpeg", dpi = 300)

LIRE_all_Dal_clean_place <- na.omit(LIRE_all_Dal_clean %>%
                                select(findspot_ancient_clean,Longitude,Latitude) %>%
                                group_by(findspot_ancient_clean) %>%
                                count(findspot_ancient_clean,Longitude,Latitude) %>%
                                arrange(desc(n)))

(LIRE_all_Dal_clean_ll <- st_as_sf(LIRE_all_Dal_clean_place, coords = c('Longitude', 'Latitude'), remove = FALSE,
                             crs = 4326, agr = "constant"))

ggplot() + 
  geom_sf(data = world, color = "grey", fill = "lightgrey") + 
  geom_sf(data = roman_roads, colour = 'grey30', size = 0.6) +
  geom_sf(data = roman_69_provinces, colour = 'black', size = 0.8) +
  geom_sf(data = roman_settlements, colour = 'black', alpha=0.6, size = 0.8) +
  geom_sf(data = LIRE_all_Dal_clean_ll, aes(size = n), alpha=0.8, colour = 'brown') +
  geom_sf(data = key_sites_ll, colour = 'black', size = 1) +
  geom_text_repel(data = key_sites_ll, aes(x = Longitude, y = Latitude,label = findspot_ancient_clean), 
                  nudge_x = c(-2, -1.5, -1), 
                  nudge_y = c(-0.25,-0.25,-0.25)) +
  labs(size = 'Monument density') +
  labs(caption = "Based on data from LIRE v.3.0 (Clean province = Dalmatia)") +
  ggtitle("Distribution of funerary and sacral monuments in Dalmatia") +
  coord_sf(default_crs = st_crs(4326), xlim = c(13, 21), ylim = c(41.5, 46)) +
  theme_void()

ggsave("output_images/LIRE_clean_dal_scatter.jpeg", dpi = 300)

#now to combine https://www.geeksforgeeks.org/draw-multiple-ggplot2-plots-side-by-side/

plot5 <- 
  ggplot() + 
  geom_sf(data = world, color = "grey", fill = "lightgrey") + 
  geom_sf(data = roman_roads, colour = 'grey30', size = 0.6) +
  geom_sf(data = roman_69_provinces, colour = 'black', size = 0.8) +
  geom_sf(data = roman_settlements, colour = 'black', alpha=0.6, size = 0.8) +
  geom_sf(data = LIRE_Dal_corpus_ll, aes(size = n), alpha=0.8, colour = 'sienna1') + 
  geom_sf(data = key_sites_ll, colour = 'black', size = 1) +
  labs(size = 'Monument density') +
  labs(caption = "Based on data from LIRE v.3.0") +
  ggtitle("Distribution of military funerary and sacral monuments",
          subtitle = "Dalmatia") +
  coord_sf(default_crs = st_crs(4326), xlim = c(13, 21), ylim = c(41.5, 46)) +
  theme_void()

plot6 <- 
  ggplot() + 
  geom_sf(data = world, color = "grey", fill = "lightgrey") + 
  geom_sf(data = roman_roads, colour = 'grey30', size = 0.6) +
  geom_sf(data = roman_69_provinces, colour = 'black', size = 0.8) +
  geom_sf(data = roman_settlements, colour = 'black', alpha=0.6, size = 0.8) +
  geom_sf(data = LIRE_all_Dal_ll, aes(size = n), alpha=0.8, colour = 'brown') +
  geom_sf(data = key_sites_ll, colour = 'black', size = 1) +
  labs(size = 'Monument density') +
  labs(caption = "Based on data from LIRE v.3.0") +
  ggtitle("Distribution of funerary and sacral monuments",
          subtitle = "Dalmatia") +
  coord_sf(default_crs = st_crs(4326), xlim = c(13, 21), ylim = c(41.5, 46)) +
  theme_void()

doubletroublest <- grid.arrange(plot5, plot2, ncol = 6)

ggsave("output_images/LIRE_corpus_and_dal_scatter.pdf", doubletrouble, width = 11.7, height = 8.3)
ggsave("output_images/LIRE_corpus_and_dal_scatter.jpeg", doubletrouble, dpi = 600)

plot7 <- 
  ggplot() + 
  geom_sf(data = world, color = "grey", fill = "lightgrey") + 
  geom_sf(data = roman_roads, colour = 'grey30', size = 0.6) +
  geom_sf(data = roman_69_provinces, colour = 'black', size = 0.8) +
  geom_sf(data = roman_settlements, colour = 'black', alpha=0.6, size = 0.8) +
  geom_sf(data = LIRE_Dal_corpus_clean_ll, aes(size = n), alpha=0.8, colour = 'sienna1') + 
  geom_sf(data = key_sites_ll, colour = 'black', size = 1) +
  labs(size = 'Monument density') +
  labs(caption = "Based on data from LIRE v.3.0 (Clean provinces = Dalmatia)") +
  ggtitle("Distribution of military funerary and sacral monuments",
          subtitle = "Dalmatia") +
  coord_sf(default_crs = st_crs(4326), xlim = c(13, 21), ylim = c(41.5, 46)) +
  theme_void()

plot8 <- 
  ggplot() + 
  geom_sf(data = world, color = "grey", fill = "lightgrey") + 
  geom_sf(data = roman_roads, colour = 'grey30', size = 0.6) +
  geom_sf(data = roman_69_provinces, colour = 'black', size = 0.8) +
  geom_sf(data = roman_settlements, colour = 'black', alpha=0.6, size = 0.8) +
  geom_sf(data = LIRE_all_Dal_clean_ll, aes(size = n), alpha=0.8, colour = 'brown') +
  geom_sf(data = key_sites_ll, colour = 'black', size = 1) +
  labs(size = 'Monument density') +
  labs(caption = "Based on data from LIRE v.3.0 (Clean provinces = Dalmatia)") +
  ggtitle("Distribution of funerary and sacral monuments",
          subtitle = "Dalmatia") +
  coord_sf(default_crs = st_crs(4326), xlim = c(13, 21), ylim = c(41.5, 46)) +
  theme_void()

doubletroublester <- grid.arrange(plot7, plot8, ncol = 2)

ggsave("output_images/LIRE_clean_corpus_and_dal_scatter.pdf", doubletroubler, width = 11.7, height = 8.3)
ggsave("output_images/LIRE_clean_corpus_and_dal_scatter.jpeg", doubletroubler, dpi = 600)