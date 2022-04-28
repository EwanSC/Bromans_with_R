## trying out sql packages
## ESC 27/04/2020
## aim to try out and clean with SQL packages for R using previously developed skills

## SQL packages
library(sqldf)
library(dplyr)

## Get cleaned df
clean_data <- load_clean_epig_data("data/2022-04-26-EDCS_via_Lat_Epig-prov_Dalmatia-10140.json")

## testing out using https://dept.stat.lsa.umich.edu/~jerrick/courses/stat701/notes/sql.html
all_clean_data <- sqldf("SELECT * from clean_data")

# now analyse:
## 'all military'

all_military <- sqldf("Select * from clean_data
                  WHERE inscription_interpretive_cleaning 
                    LIKE '%legio%' 
                  OR inscription_interpretive_cleaning 
                    LIKE '%cohor%'
                  OR inscription_interpretive_cleaning 
                    LIKE '% ala %'
                  OR inscription_interpretive_cleaning 
                    LIKE '% alae %'
                  OR inscription_interpretive_cleaning 
                    LIKE '%milit%'
                  OR inscription_interpretive_cleaning 
                    LIKE '% equ%'
                  OR inscription_interpretive_cleaning 
                    LIKE '%duplicari%'
                  OR inscription_interpretive_cleaning 
                    LIKE '%veteran%'
                  OR cleaned_place = 'Tilurium'
                  OR cleaned_place = 'Burnum'
                  OR cleaned_place = 'Andetrium'
                  OR cleaned_place = 'Bigeste'
                  ")

write.csv(all_military,"data/all_military_first_cent.csv", row.names = FALSE)

all_military_place <- na.omit(all_military %>%
                                 select(cleaned_place,longitude,latitude) %>%
                                 group_by(cleaned_place) %>%
                                 count(cleaned_place,longitude,latitude) %>%
                                 arrange(desc(n))) 

(all_military_place_ll <- st_as_sf(all_military_place, coords = c('longitude', 'latitude'), 
                                   crs = 4326, agr = "constant"))

ggplot() + 
  geom_sf(data = world, color = "black", fill = "lightgrey") + 
  geom_sf(data = roman_69_provinces, colour = 'black', size = 0.8) +
  geom_sf(data = all_military_place_ll, aes(size = n), alpha=0.6, colour = '#cd2026') +
  labs(size = 'Monuments') +
  ggtitle("Epigraphic Distribution of the Military in Dalmatia 30 BCE - 150 CE", subtitle = "Filtered Using Key Words and Places") +
  coord_sf(default_crs = st_crs(4326), xlim = c(13, 21), ylim = c(41.5, 46))

## 'military' by inscription terms

military_by_term <- sqldf("Select * from clean_data
                          WHERE inscription_interpretive_cleaning 
                          LIKE '%legio%' 
                          OR inscription_interpretive_cleaning 
                          LIKE '%cohor%'
                          OR inscription_interpretive_cleaning 
                          LIKE '% ala %'
                          OR inscription_interpretive_cleaning 
                          LIKE '% alae %'
                          OR inscription_interpretive_cleaning 
                          LIKE '%milit%'
                          OR inscription_interpretive_cleaning 
                          LIKE '% equ%'
                          OR inscription_interpretive_cleaning 
                          LIKE '%duplicari%'
                          OR inscription_interpretive_cleaning 
                          LIKE '%veteran%'")

write.csv(military_by_term,"data/military_by_term_first_cent.csv", row.names = FALSE)

military_by_term_place <- na.omit(military_by_term %>%
                                select(cleaned_place,longitude,latitude) %>%
                                group_by(cleaned_place) %>%
                                count(cleaned_place,longitude,latitude) %>%
                                arrange(desc(n))) 

(military_by_term_place_ll <- st_as_sf(military_by_term_place, coords = c('longitude', 'latitude'), 
                                   crs = 4326, agr = "constant"))

ggplot() + 
  geom_sf(data = world, color = "black", fill = "lightgrey") + 
  geom_sf(data = roman_69_provinces, colour = 'black', size = 0.8) +
  geom_sf(data = military_by_term_place_ll, aes(size = n), alpha=0.6, colour = '#cd2026') +
  labs(size = 'Monuments') +
  ggtitle("Epigraphic Distribution of the Military in Dalmatia 30 BCE - 150 CE", subtitle = "Filtered Using Key Words") +
  coord_sf(default_crs = st_crs(4326), xlim = c(13, 21), ylim = c(41.5, 46))

## 'military' by site
military_by_place <- sqldf("Select * from clean_data
                  WHERE cleaned_place = 'Tilurium'
                  OR cleaned_place = 'Burnum'
                  OR cleaned_place = 'Andetrium'
                  OR cleaned_place = 'Bigeste'
                  ")

write.csv(military_by_place,"data/military_by_place_first_cent.csv", row.names = FALSE)

military_by_place_place <- na.omit(military_by_place %>%
                                    select(cleaned_place,place,longitude,latitude) %>%
                                    group_by(cleaned_place) %>%
                                    count(cleaned_place,place,longitude,latitude) %>%
                                    arrange(desc(n)))

(military_by_place_place_ll <- st_as_sf(military_by_place_place, coords = c('longitude', 'latitude'), 
                                       crs = 4326, agr = "constant"))
ggplot() + 
  geom_sf(data = world, color = "black", fill = "lightgrey") + 
  geom_sf(data = roman_69_provinces, colour = 'black', size = 0.8) +
  geom_sf(data = military_by_place_place_ll, aes(size = n), alpha=0.6, colour = '#cd2026') +
  labs(size = 'Monuments') +
  ggtitle("Epigraphic Distribution of the Military in Dalmatia 30 BCE - 150 CE", subtitle = "Filtered Using Places") +
  coord_sf(default_crs = st_crs(4326), xlim = c(13, 25), ylim = c(41.5, 48))
