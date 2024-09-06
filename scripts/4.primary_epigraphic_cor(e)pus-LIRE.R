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

#get only some columns
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
clean_LIRE_Dal <- sqldf("Select Distinct 
                        place, findspot_ancient_clean, findspot_modern_clean, Longitude, Latitude 
                        from LIRE_Dal")
clean_LIRE_Dal_clean <- sqldf("Select Distinct 
                              place, findspot_ancient_clean, findspot_modern_clean, Longitude, Latitude 
                              from LIRE_Dal")

#now military
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

# remove building/dedicatory inscription; honorific inscription; boundary inscription; mile-/leaguestone;
# military diploma(type_of_inscription_clean); mile-/leaguestone; tile (type_of_monument_clean)
# filter types

LIRE_Dal_corpus <- LIRE_Dal_mil %>%
  filter(!type_of_monument_clean %in% c('architectural member','mile-/leaguestone', 'tile') 
        & !type_of_inscription_clean %in% c('remove building/dedicatory inscription', 
                                            'honorific inscription', 'boundary inscription', 
                                            'mile-/leaguestone', 'military diploma','building/dedicatory inscription'))

LIRE_Dal_corpus_clean <- LIRE_Dal_mil_clean %>%
  filter(!type_of_monument_clean %in% c('mile-/leaguestone', 'tile') 
         & !type_of_inscription_clean %in% c('architectural member','remove building/dedicatory inscription', 
                                             'honorific inscription', 'boundary inscription', 
                                             'mile-/leaguestone', 'military diploma','building/dedicatory inscription'))
#check data
LIRE_Dal_corpus %>% count(type_of_monument_clean, type_of_inscription_clean)
LIRE_Dal_corpus_clean %>% count(type_of_monument_clean, type_of_inscription_clean)

#add value for count
LIRE_Dal_corpus$count<- 1
LIRE_Dal_corpus_clean$count<- 1

#now to plot on scatter map
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
  geom_sf(data = roman_settlements, colour = 'black', alpha=0.6, size = 0.8) +
  geom_sf(data = LIRE_Dal_corpus_ll, aes(size = n), alpha=0.8, colour = 'sienna1') +
  labs(size = 'Monument density') +
  labs(caption = "Based on data from LIRE v.3.0") +
  ggtitle("Distribution of Military Monuments in Dalmatia", subtitle = "Filtered by site and key terms") +
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
  labs(size = 'Monument density') +
  labs(caption = "Based on data from LIRE v.3.0") +
  ggtitle("Distribution of Military Monuments in Dalmatia", 
          subtitle = "Filtered by site, key terms and 'clean' provinces") +
  coord_sf(default_crs = st_crs(4326), xlim = c(13, 21), ylim = c(41.5, 46)) +
  theme_void()

ggsave("output_images/LIRE_clean_corpus_scatter.jpeg", dpi = 300)

## Now compare to all dalmatia

LIRE_all_Dal <- LIRE_Dal %>%
  filter(!type_of_monument_clean %in% c('architectural member','mile-/leaguestone', 'tile') 
         & !type_of_inscription_clean %in% c('remove building/dedicatory inscription', 
                                             'honorific inscription', 'boundary inscription', 
                                             'mile-/leaguestone', 'military diploma','building/dedicatory inscription'))

LIRE_all_Dal_clean <- LIRE_Dal_clean %>%
  filter(!type_of_monument_clean %in% c('architectural member','mile-/leaguestone', 'tile') 
         & !type_of_inscription_clean %in% c('remove building/dedicatory inscription', 
                                             'honorific inscription', 'boundary inscription', 
                                             'mile-/leaguestone', 'military diploma','building/dedicatory inscription'))

LIRE_all_Dal$count<- 1
LIRE_all_Dal_clean$count<- 1

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
  geom_sf(data = LIRE_all_Dal_ll, aes(size = n), alpha=0.8, colour = 'sienna1') +
  labs(size = 'Monument density') +
  labs(caption = "Based on data from LIRE v.3.0") +
  ggtitle("Distribution of all Monuments in Dalmatia") +
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
  geom_sf(data = LIRE_all_Dal_clean_ll, aes(size = n), alpha=0.8, colour = 'sienna1') +
  labs(size = 'Monument density') +
  labs(caption = "Based on data from LIRE v.3.0") +
  ggtitle("Distribution of all Monuments in Dalmatia") +
  coord_sf(default_crs = st_crs(4326), xlim = c(13, 21), ylim = c(41.5, 46)) +
  theme_void()

ggsave("output_images/LIRE_clean_dal_scatter.jpeg", dpi = 300)

#now to combine https://www.geeksforgeeks.org/draw-multiple-ggplot2-plots-side-by-side/

plot1 <- 
  ggplot() + 
  geom_sf(data = world, color = "grey", fill = "lightgrey") + 
  geom_sf(data = roman_roads, colour = 'grey30', size = 0.6) +
  geom_sf(data = roman_69_provinces, colour = 'black', size = 0.8) +
  geom_sf(data = roman_settlements, colour = 'black', alpha=0.6, size = 0.8) +
  geom_sf(data = LIRE_Dal_corpus_ll, aes(size = n), alpha=0.8, colour = 'sienna1') +
  labs(size = 'Monument density') +
  labs(caption = "Filtered by site and key terms. Data from LIRE v.3.0") +
  ggtitle("Distribution of Military Monuments in Dalmatia", subtitle = "Funerary, Personal, and Sacral") +
  coord_sf(default_crs = st_crs(4326), xlim = c(13, 21), ylim = c(41.5, 46)) +
  theme_void()

plot2 <- 
  ggplot() + 
  geom_sf(data = world, color = "grey", fill = "lightgrey") + 
  geom_sf(data = roman_roads, colour = 'grey30', size = 0.6) +
  geom_sf(data = roman_69_provinces, colour = 'black', size = 0.8) +
  geom_sf(data = roman_settlements, colour = 'black', alpha=0.6, size = 0.8) +
  geom_sf(data = LIRE_all_Dal_ll, aes(size = n), alpha=0.8, colour = 'brown') +
  labs(size = 'Monument density') +
  labs(caption = "Data from LIRE v.3.0") +
  ggtitle("Distribution of all Monuments in Dalmatia", subtitle = "Funerary, Personal, and Sacral") +
  coord_sf(default_crs = st_crs(4326), xlim = c(13, 21), ylim = c(41.5, 46)) +
  theme_void()

doubletrouble <- grid.arrange(plot1, plot2, ncol = 2)

ggsave("output_images/LIRE_corpus_and_dal_scatter.pdf", doubletrouble, width = 11.7, height = 8.3)
ggsave("output_images/LIRE_corpus_and_dal_scatter.jpeg", doubletrouble, dpi = 600)

