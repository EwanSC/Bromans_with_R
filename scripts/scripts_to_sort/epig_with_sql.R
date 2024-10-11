## Using sql packages to create PhD corpus
## Author: Ewan Coopey
## Created: 27/04/2020
## Last edit: 29/08/2022
## Clean with SQL packages for R using previously developed 
## skills and come to final corpus for moving to database

## packages
library(sqldf)
library(dplyr)
library(ggplot2)
library(ggrepel)

## first, run scripts/primary_epigraphic_cor(e)pus and scripts/primary_map
## Get df and cleaned df 
data <- load_epig_data("data/2022-08-29-EDCS_via_Lat_Epig-prov_Dalmatia-10140.json")
dated_data <- load_first_cent_epig_data("data/2022-08-29-EDCS_via_Lat_Epig-prov_Dalmatia-10140.json")
cleaned_place <- load_clean_place("data/2022-08-29-EDCS_via_Lat_Epig-prov_Dalmatia-10140.json")
clean_data <- load_clean_epig_data_30_150("data/2022-08-29-EDCS_via_Lat_Epig-prov_Dalmatia-10140.json")



## testing out using https://dept.stat.lsa.umich.edu/~jerrick/courses/stat701/notes/sql.html
all_clean_data <- sqldf("SELECT * from clean_data")

str(all_clean_data)

# now analyse:
## 'all military' by using 'where like' clause and wildcard
# see here for more: https://www.w3schools.com/sql/sql_wildcards.asp#:~:text=SQL%20Wildcards,-%E2%9D%AE%20Previous%20Next&text=A%20wildcard%20character%20is%20used,specified%20pattern%20in%20a%20column.
## filtering to 'military' by inscription terms and place

all_military_1 <- sqldf("Select * from clean_data
                  WHERE inscription_interpretive_cleaning 
                    LIKE '%legio%'
                  OR inscription_interpretive_cleaning 
                    LIKE '%cohor%'
                  OR inscription_interpretive_cleaning 
                    LIKE '%ala%'
                  OR inscription_interpretive_cleaning 
                    LIKE '%alae%'
                  OR inscription_interpretive_cleaning 
                    LIKE '%milit%'
                  OR inscription_interpretive_cleaning 
                    LIKE '%eques%'
                  OR inscription_interpretive_cleaning 
                    LIKE '%equit%'
                  OR inscription_interpretive_cleaning 
                    LIKE '%duplicari%'
                  OR inscription_interpretive_cleaning 
                    LIKE '%veteran%'
                  or inscription_interpretive_cleaning
                    LIKE '%centuri%'
                  or inscription_interpretive_cleaning
                    LIKE '%immun%'
                  or inscription_interpretive_cleaning
                    LIKE '%miles%'
                  or inscription_interpretive_cleaning
                    LIKE '%beneficiar%'
                  or inscription_interpretive_cleaning
                    LIKE '%tesserari%'
                  or inscription_interpretive_cleaning
                    LIKE '%signifer%'
                  or inscription_interpretive_cleaning
                    LIKE '%aquilifer%'
                  or inscription_interpretive_cleaning
                    LIKE '%imaginifer%'
                  or inscription_interpretive_cleaning
                    LIKE '%corniculari%'
                  or inscription_interpretive_cleaning
                    LIKE '%principalis%'
                  or inscription_interpretive_cleaning
                    LIKE '%primus pilus%'
                  or inscription_interpretive_cleaning
                    LIKE '%primo pilo%'
                  or inscription_interpretive_cleaning
                    LIKE '%primi pili%'
                  or inscription_interpretive_cleaning
                    LIKE '%praefectus castrorum%'
                  or inscription_interpretive_cleaning
                    LIKE '%optio %'
                  or inscription_interpretive_cleaning
                    LIKE '%option%'
                  OR cleaned_place = 'Tilurium'
                  OR cleaned_place = 'Burnum'
                  OR cleaned_place = 'Andetrium'
                  OR cleaned_place = 'Bigeste'
                  ")

write.csv(all_military_1,"output_tables/all_military_first_cent1.csv", row.names = FALSE)

# using brackets to find either/or: this one doesn't capture as many, why??
all_military_2 <- sqldf("Select * from clean_data
                  WHERE inscription_interpretive_cleaning 
                    LIKE '%legio[ n]%'
                  OR inscription_interpretive_cleaning 
                    LIKE '%cohor[st]%'
                  OR inscription_interpretive_cleaning 
                    LIKE '%ala%'
                  OR inscription_interpretive_cleaning 
                    LIKE '%alae%'
                  OR inscription_interpretive_cleaning 
                    LIKE '%milit%'
                  OR inscription_interpretive_cleaning 
                    LIKE '%eques%'
                  OR inscription_interpretive_cleaning 
                    LIKE '%equit[ei]%'
                  OR inscription_interpretive_cleaning 
                    LIKE '%duplicari[ouis]%'
                  OR inscription_interpretive_cleaning 
                     LIKE '%veteran%'
                  OR inscription_interpretive_cleaning
                    LIKE '%centuri[ao]%'
                  OR inscription_interpretive_cleaning
                    LIKE '%immun[ie]%'
                  OR inscription_interpretive_cleaning
                    LIKE '%miles%'
                   OR inscription_interpretive_cleaning
                     LIKE '%beneficiari[uoie]%'
                   OR inscription_interpretive_cleaning
                     LIKE '%tesserari[uio]%'
                   OR inscription_interpretive_cleaning
                     LIKE '%signifer%'
                   OR inscription_interpretive_cleaning
                     LIKE '%aquilifer%'
                   OR inscription_interpretive_cleaning
                     LIKE '%imaginifer%'
                   OR inscription_interpretive_cleaning
                     LIKE '%corniculari[uio]%'
                   OR inscription_interpretive_cleaning
                     LIKE '%principalis%'
                   OR inscription_interpretive_cleaning
                     LIKE '%primus pilus%'
                   OR inscription_interpretive_cleaning
                     LIKE '%prim[oi] pil[oi]%'
                   OR inscription_interpretive_cleaning
                     LIKE '%praefectus [ck]astrorum%'
                   OR inscription_interpretive_cleaning
                     LIKE '%optio[ n]%'
                   OR cleaned_place = 'Tilurium'
                   OR cleaned_place = 'Burnum'
                   OR cleaned_place = 'Andetrium'
                   OR cleaned_place = 'Bigeste'
                ")

write.csv(all_military_2,"output_tables/all_military_first_cent2.csv", row.names = FALSE)

## now to plot #1
all_military_place <- na.omit(all_military_1 %>%
                                 select(cleaned_place,longitude,latitude) %>%
                                 group_by(cleaned_place) %>%
                                 count(cleaned_place,longitude,latitude) %>%
                                 arrange(desc(n))) 

(all_military_place_ll <- st_as_sf(all_military_place, coords = c('longitude', 'latitude'), remove = FALSE,
                                   crs = 4326, agr = "constant"))

ggplot() + 
  geom_sf(data = world, color = "black", fill = "lightgrey") + 
  geom_sf(data = roman_69_provinces, colour = 'black', size = 0.8) +
  geom_sf(data = all_military_place_ll, aes(size = n), alpha=0.6, colour = '#cd2026') +
  labs(size = 'Monuments') +
  ggtitle("Epigraphic Distribution of the Military in Dalmatia 30 BCE - 150 CE", subtitle = "Filtered Using Key Words and Places") +
  coord_sf(default_crs = st_crs(4326), xlim = c(13, 21), ylim = c(41.5, 46))

ggsave("output_images/dated_military_scatter.png", dpi = 300)

## filtering to 'military' via 'milites' status column
## Turns out table 1 above captures the same monuments and more, huzzah!
all_military_by_status <- sqldf("SELECT * 
                              FROM clean_data 
                              WHERE status like '%milites%'")

write.csv(military_by_status,"output_tables/military_by_edcs-status_first_cent.csv", row.names = FALSE)

## filtering to 'military' by only inscription terms
military_by_term <- sqldf("Select * from clean_data
                          WHERE inscription_interpretive_cleaning 
                    LIKE '%legio%'
                  OR inscription_interpretive_cleaning 
                    LIKE '%cohor%'
                  OR inscription_interpretive_cleaning 
                    LIKE '%ala%'
                  OR inscription_interpretive_cleaning 
                    LIKE '%alae%'
                  OR inscription_interpretive_cleaning 
                    LIKE '%milit%'
                  OR inscription_interpretive_cleaning 
                    LIKE '%eques%'
                  OR inscription_interpretive_cleaning 
                    LIKE '%equit%'
                  OR inscription_interpretive_cleaning 
                    LIKE '%duplicari%'
                  OR inscription_interpretive_cleaning 
                    LIKE '%veteran%'
                  or inscription_interpretive_cleaning
                    LIKE '%centuri%'
                  or inscription_interpretive_cleaning
                    LIKE '%immun%'
                  or inscription_interpretive_cleaning
                    LIKE '%miles%'
                  or inscription_interpretive_cleaning
                    LIKE '%beneficiar%'
                  or inscription_interpretive_cleaning
                    LIKE '%tesserari%'
                  or inscription_interpretive_cleaning
                    LIKE '%signifer%'
                  or inscription_interpretive_cleaning
                    LIKE '%aquilifer%'
                  or inscription_interpretive_cleaning
                    LIKE '%imaginifer%'
                  or inscription_interpretive_cleaning
                    LIKE '%corniculari%'
                  or inscription_interpretive_cleaning
                    LIKE '%principalis%'
                  or inscription_interpretive_cleaning
                    LIKE '%primus pilus%'
                  or inscription_interpretive_cleaning
                    LIKE '%primo pilo%'
                  or inscription_interpretive_cleaning
                    LIKE '%primi pili%'
                  or inscription_interpretive_cleaning
                    LIKE '%praefectus castrorum%'
                  or inscription_interpretive_cleaning
                    LIKE '%optio %'
                  or inscription_interpretive_cleaning
                    LIKE '%option%'")
  
write.csv(military_by_term,"output_tables/military_by_term_first_cent.csv", row.names = FALSE)

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

write.csv(military_by_place,"output_tables/military_by_place_first_cent.csv", row.names = FALSE)

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
  coord_sf(default_crs = st_crs(4326), xlim = c(13, 21), ylim = c(41.5, 46))

## all undated Dalmatia
undated_dal <- sqldf("Select * from cleaned_place
                        WHERE dating_to IS NULL
                        OR dating_from IS NULL
                        OR dating_from = ''
                        OR dating_to = ''
                  ")

all_military_undated <- sqldf("Select * from undated_dal
                  WHERE inscription_interpretive_cleaning 
                    LIKE '%legio%'
                  OR inscription_interpretive_cleaning 
                    LIKE '%cohor%'
                  OR inscription_interpretive_cleaning 
                    LIKE '%ala%'
                  OR inscription_interpretive_cleaning 
                    LIKE '%alae%'
                  OR inscription_interpretive_cleaning 
                    LIKE '%milit%'
                  OR inscription_interpretive_cleaning 
                    LIKE '%eques%'
                  OR inscription_interpretive_cleaning 
                    LIKE '%equit%'
                  OR inscription_interpretive_cleaning 
                    LIKE '%duplicari%'
                  OR inscription_interpretive_cleaning 
                    LIKE '%veteran%'
                  or inscription_interpretive_cleaning
                    LIKE '%centuri%'
                  or inscription_interpretive_cleaning
                    LIKE '%immun%'
                  or inscription_interpretive_cleaning
                    LIKE '%miles%'
                  or inscription_interpretive_cleaning
                    LIKE '%beneficiar%'
                  or inscription_interpretive_cleaning
                    LIKE '%tesserari%'
                  or inscription_interpretive_cleaning
                    LIKE '%signifer%'
                  or inscription_interpretive_cleaning
                    LIKE '%aquilifer%'
                  or inscription_interpretive_cleaning
                    LIKE '%imaginifer%'
                  or inscription_interpretive_cleaning
                    LIKE '%corniculari%'
                  or inscription_interpretive_cleaning
                    LIKE '%principalis%'
                  or inscription_interpretive_cleaning
                    LIKE '%primus pilus%'
                  or inscription_interpretive_cleaning
                    LIKE '%primo pilo%'
                  or inscription_interpretive_cleaning
                    LIKE '%primi pili%'
                  or inscription_interpretive_cleaning
                    LIKE '%praefectus castrorum%'
                  or inscription_interpretive_cleaning
                    LIKE '%optio %'
                  or inscription_interpretive_cleaning
                    LIKE '%option%'
                  OR cleaned_place = 'Tilurium'
                  OR cleaned_place = 'Burnum'
                  OR cleaned_place = 'Andetrium'
                  OR cleaned_place = 'Bigeste'
                  ")

write.csv(all_military_undated,"output_tables/all_military_undated.csv", row.names = FALSE)

all_military_undated_place <- na.omit(all_military_undated %>%
                                     select(cleaned_place,place,longitude,latitude) %>%
                                     group_by(cleaned_place) %>%
                                     count(cleaned_place,place,longitude,latitude) %>%
                                     arrange(desc(n)))

(all_military_undated_place_ll <- st_as_sf(all_military_undated_place, coords = c('longitude', 'latitude'), 
                                        crs = 4326, agr = "constant"))

ggplot() + 
  geom_sf(data = world, color = "black", fill = "lightgrey") + 
  geom_sf(data = roman_69_provinces, colour = 'black', size = 0.8) +
  geom_sf(data = all_military_undated_place_ll, aes(size = n), alpha=0.6, colour = '#cd2026') +
  labs(size = 'Monuments') +
  ggtitle("Epigraphic Distribution of the Military in Dalmatia", subtitle = "Undated Monuments in the EDCS") +
  coord_sf(default_crs = st_crs(4326), xlim = c(13, 21), ylim = c(41.5, 46))

## now to combine undated all military with combined all military
## from https://r-lang.com/how-to-append-data-frames-in-r/
military_first_cent_and_undated <- rbind(all_military_1, all_military_undated)

write.csv(military_first_cent_and_undated,"output_tables/military_first_cent_and_undated.csv", row.names = FALSE)

military_first_cent_and_undated_place <- na.omit(military_first_cent_and_undated %>%
                                        select(cleaned_place,place,longitude,latitude) %>%
                                        group_by(cleaned_place) %>%
                                        count(cleaned_place,place,longitude,latitude) %>%
                                        arrange(desc(n)))

(military_first_cent_and_undated_place_ll <- st_as_sf(military_first_cent_and_undated_place, coords = c('longitude', 'latitude'), 
                                           crs = 4326, agr = "constant"))

ggplot() + 
  geom_sf(data = world, color = "black", fill = "lightgrey") + 
  geom_sf(data = roman_69_provinces, colour = 'black', size = 0.8) +
  geom_sf(data = military_first_cent_and_undated_place_ll, aes(size = n), alpha=0.6, colour = '#cd2026') +
  labs(size = 'Monuments') +
  ggtitle("Epigraphic Distribution of the Military in Dalmatia", subtitle = "Undated Monuments and Monuments Dated 30 BCE–150 CE") +
  coord_sf(default_crs = st_crs(4326), xlim = c(13, 21), ylim = c(41.5, 46))

ggsave("output_images/dated_undated_military_scatter.png", dpi = 300)
