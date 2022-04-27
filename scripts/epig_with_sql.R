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

## now adding 'where' clause
Salona <- sqldf("SELECT * from clean_data WHERE cleaned_place = 'Salona'")

military <- sqldf("Select * from clean_data
                  WHERE inscription_interpretive_cleaning 
                    LIKE '%legio%' 
                  OR inscription_interpretive_cleaning 
                    LIKE '%cohor%'
                  OR inscription_interpretive_cleaning 
                    LIKE '%  ala  %'
                  OR cleaned_place = 'Tilurium'
                  OR cleaned_place = 'Burnum'
                  OR cleaned_place = 'Andetrium'
                  ")

write.csv(military,"data/military_first_cent.csv", row.names = FALSE)

## now plot it

military_place <- na.omit(military %>%
                                 select(cleaned_place,longitude,latitude) %>%
                                 group_by(cleaned_place) %>%
                                 count(cleaned_place,longitude,latitude) %>%
                                 arrange(desc(n))) 

(military_place_ll <- st_as_sf(military_place, coords = c('longitude', 'latitude'), 
                                   crs = 4326, agr = "constant"))

ggplot() + 
  geom_sf(data = world, color = "darkgrey", fill = "lightgrey") + 
  geom_sf(data = roman_69_provinces, colour = 'red') +
  geom_sf(data = military_place_ll, aes(size = n), alpha=0.6) + 
  labs(size = 'Monuments') +
  ggtitle("The Military in Dalmatia", subtitle = "Epigraphic Distribution 30 BCE - 150 CE") +
  coord_sf(default_crs = st_crs(4326), xlim = c(13, 21), ylim = c(41.5, 46))
