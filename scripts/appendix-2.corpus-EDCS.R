# Primary epigraph Cor(e)pus EDCS
# Ewan Coopey
# created 29/08/2022
# last edit: 12/10/2022
# Create primary datasets for PhD research by combining 1st cent. CE and undated military inscriptions
# and then plot both (separately and together)
# sql: ## testing out using https://dept.stat.lsa.umich.edu/~jerrick/courses/stat701/notes/sql.html

## get packages
library(sqldf)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(sf)

## run scripts/1.primary_epigraphic_cor(e)pus_functions and 2.scripts/primary_map
## then, deploy function to get and clean df:

cleaned_place <- load_clean_place("data/2022-08-29-EDCS_via_Lat_Epig-prov_Dalmatia-10140.json")
clean_dated_data <- load_clean_epig_data_30_150("data/2022-08-29-EDCS_via_Lat_Epig-prov_Dalmatia-10140.json")

# now analyse:
## 'all military' by using 'where like' clause and wildcard
# see here for more: https://www.w3schools.com/sql/sql_wildcards.asp#:~:text=SQL%20Wildcards,-%E2%9D%AE%20Previous%20Next&text=A%20wildcard%20character%20is%20used,specified%20pattern%20in%20a%20column.
## filtering to 'military' by inscription terms and place and then plotting using ggplot

#military from all periods
military <- sqldf("Select * from cleaned_place
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
                  or status
                    LIKE '%milites%'
                  OR cleaned_place = 'Tilurium'
                  OR cleaned_place = 'Burnum'
                  OR cleaned_place = 'Andetrium'
                  OR cleaned_place = 'Bigeste'
                  ")


#military from 1st cent.
dated_military <- sqldf("Select * from clean_dated_data
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
                  or status
                    LIKE '%milites%'
                  OR cleaned_place = 'Tilurium'
                  OR cleaned_place = 'Burnum'
                  OR cleaned_place = 'Andetrium'
                  OR cleaned_place = 'Bigeste'
                  ")

write.csv(dated_military,"output_tables/corpus/dated_military.csv", row.names = FALSE)

## count monument types
dated_military$count<- 1

military_tituli_sep <- sqldf("Select * from dated_military
                              WHERE status 
                                LIKE '%tituli Sepulcrales%'")

military_tituli_sac <- sqldf ("Select * from dated_military
                                Where status
                                  LIKE '%tituli sacri%'")

military_tituli_ss <- sqldf ("Select * from dated_military
                                Where status
                                  LIKE '%tituli sacri%'
                                OR status
                                  LIKE '%tituli sepulcrales%'")


## plot on map
dated_military_place <- na.omit(dated_military %>%
                                 select(cleaned_place,longitude,latitude) %>%
                                 group_by(cleaned_place) %>%
                                 count(cleaned_place,longitude,latitude) %>%
                                 arrange(desc(n))) 

(dated_military_place_ll <- st_as_sf(dated_military_place, coords = c('longitude', 'latitude'), remove = FALSE,
                                   crs = 4326, agr = "constant"))

ggplot() + 
  geom_sf(data = world, color = "grey", fill = "lightgrey") + 
  geom_sf(data = roman_roads, colour = 'grey30', size = 0.6) +
  geom_sf(data = roman_69_provinces, colour = 'black', size = 0.8) +
  geom_sf(data = roman_settlements, colour = 'black', alpha=0.6, size = 0.8) +
  geom_sf(data = dated_military_place_ll, aes(size = n), alpha=0.8, colour = 'darkorange') +
  labs(size = 'Monuments') +
  ggtitle("Epigraphic Distribution of the Military in Dalmatia 30 BCE - 150 CE", subtitle = "Filtered Using Key Words and Places") +
  coord_sf(default_crs = st_crs(4326), xlim = c(13, 21), ylim = c(41.5, 46)) +
  theme_void()

ggsave("output_images/dated_military_scatter.jpeg", dpi = 300)

## now to get all undated Dalmatian inscriptions of military flavour and plot
undated_dal <- sqldf("Select * from cleaned_place
                        WHERE dating_to IS NULL
                        OR dating_from IS NULL
                        OR dating_from = ''
                        OR dating_to = ''
                  ")

undated_military <- sqldf("Select * from undated_dal
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
                  or status
                    LIKE '%milites%'
                  OR cleaned_place = 'Tilurium'
                  OR cleaned_place = 'Burnum'
                  OR cleaned_place = 'Andetrium'
                  OR cleaned_place = 'Bigeste'
                  ")

write.csv(undated_military,"output_tables/corpus/undated_military.csv", row.names = FALSE)

undated_military$count<- 1

undated_tituli_sep <- sqldf("Select * from undated_military
                              WHERE status 
                                LIKE '%tituli Sepulcrales%'")

undated_tituli_sac <- sqldf ("Select * from undated_military
                                Where status
                                  LIKE '%tituli sacri%'")

undated_tituli_ss <- sqldf ("Select * from undated_military
                                Where status
                                  LIKE '%tituli sacri%'
                                OR status
                                  LIKE '%tituli sepulcrales%'")

undated_military_place <- na.omit(undated_military %>%
                            		select(cleaned_place,place,longitude,latitude) %>%
                            		group_by(cleaned_place) %>%
                            		count(cleaned_place,place,longitude,latitude) %>%
                            		arrange(desc(n)))

(undated_military_place_ll <- st_as_sf(undated_military_place, coords = c('longitude', 'latitude'), 
                                        crs = 4326, agr = "constant"))

ggplot() + 
  geom_sf(data = world, color = "grey", fill = "lightgrey") + 
  geom_sf(data = roman_roads, colour = 'grey30', size = 0.6) +
  geom_sf(data = roman_69_provinces, colour = 'black', size = 0.8) +
  geom_sf(data = roman_settlements, colour = 'black', alpha=0.6, size = 0.8) +
  geom_sf(data = undated_military_place_ll, aes(size = n), alpha=0.6, colour = 'darkorange4') +
  labs(size = 'Monuments') +
  ggtitle("Epigraphic Distribution of the Military in Dalmatia", subtitle = "Undated Monuments in the EDCS") +
  coord_sf(default_crs = st_crs(4326), xlim = c(13, 21), ylim = c(41.5, 46)) +
  theme_void()

ggsave("output_images/undated_military_scatter.jpeg", dpi = 300)

## now to combine undated dated_military with undated_military and plot
## combined using https://r-lang.com/how-to-append-data-frames-in-r/
dated_military$group <- 'Dated'
undated_military$group <- 'Undated'

military_dated_and_undated <- rbind(dated_military, undated_military)

write.csv(military_dated_and_undated,"output_tables/corpus/military_dated_and_undated.csv", row.names = FALSE)

military_dated_and_undated_place <- na.omit(military_dated_and_undated %>%
                                        select(cleaned_place,place,longitude,latitude,group) %>%
                                        group_by(cleaned_place) %>%
                                        count(cleaned_place,place,longitude,latitude,group) %>%
                                        arrange(desc(n)))

write.csv(military_dated_and_undated_place,"output_tables/corpus/military_dated_and_undated_place.csv", row.names = FALSE)

(military_dated_and_undated_place_ll <- st_as_sf(military_dated_and_undated_place, coords = c('longitude', 'latitude'), 
                                           crs = 4326, agr = "constant"))

ggplot() + 
  geom_sf(data = world, color = "grey", fill = "lightgrey") + 
  geom_sf(data = roman_roads, colour = 'grey30', size = 0.6) +
  geom_sf(data = roman_69_provinces, colour = 'black', size = 0.8) +
  geom_sf(data = roman_settlements, colour = 'black', alpha=0.6, size = 0.8) +
  geom_sf(data = military_dated_and_undated_place_ll, aes(size = n, group=group, color=group), alpha=0.6) +
  scale_color_manual(values = c("red", "pink")) +
  labs(size = 'Monuments', group= 'Date') +
  ggtitle("Epigraphic Distribution of the Military in Dalmatia", subtitle = "Undated Monuments and Monuments Dated 30 BCE–150 CE") +
  coord_sf(default_crs = st_crs(4326), xlim = c(13, 21), ylim = c(41.5, 46)) +
  theme_void()

ggsave("output_images/dated_undated_military_scatter.jpeg", dpi = 300)

# now to clean and select only distinct place at latitude data