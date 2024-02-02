#count monument types
# run C:/Users/ewans/Documents/GitHub/Bromans_with_R/scripts/1.primary_epigraphic_cor(e)pus_functions.R first
library(sqldf)
library(dplyr)
library(ggplot2)

cleaned_place <- load_clean_place("data/2022-08-29-EDCS_via_Lat_Epig-prov_Dalmatia-10140.json")

cleaned_place$count<- 1

dal_tituli_sep <- sqldf("Select * from cleaned_place
                  WHERE status 
                    LIKE '%tituli Sepulcrales%'")

dal_tituli_sac <- sqldf ("Select * from cleaned_place
                                Where status
                                  LIKE '%tituli sacri%'")

dal_tituli_ss <- sqldf ("Select * from cleaned_place
                                Where status
                                  LIKE '%tituli sacri%'
                                OR status
                                  LIKE '%tituli sepulcrales%'")

cleaned_military <- sqldf("Select * from cleaned_place
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
                  or status
                    LIKE '%milites%'
                  OR cleaned_place = 'Tilurium'
                  OR cleaned_place = 'Burnum'
                  OR cleaned_place = 'Andetrium'
                  OR cleaned_place = 'Bigeste'
                  ")

military_tituli_sep <- sqldf("Select * from cleaned_military
                  WHERE status 
                    LIKE '%tituli Sepulcrales%'")

military_tituli_sac <- sqldf ("Select * from cleaned_military
                                Where status
                                  LIKE '%tituli sacri%'")

military_tituli_ss <- sqldf ("Select * from cleaned_military
                                Where status
                                  LIKE '%tituli sacri%'
                                OR status
                                  LIKE '%tituli sepulcrales%'")

military_tituli_count <-  military_tituli_ss %>% 
  group_by(status) %>% 
  summarise(Count = sum(count))
