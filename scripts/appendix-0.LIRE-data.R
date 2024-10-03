# appendix-LIRE-data
#started 19/9/2024
# last ran 19/09/2024
library(dplyr)
library(sqldf)
library(arrow)
library(datplot)
library(ggplot2)

LIRE_all <-
  read_parquet("https://zenodo.org/records/8431452/files/LIRE_v3-0.parquet?download=1")

LIRE_dictionary <-
  read.csv("https://zenodo.org/records/8431452/files/LI_metadata.csv?download=1")

write.csv(LIRE_dictionary, file = "data/LIRE/LIRE_dictionary.csv")

#filter columns
LIRE_filtered <- select(LIRE_all,
                        "LIST-ID",
                        "EDCS-ID",
                        "EDH-ID",
                        "not_before",
                        "not_after",
                        "inscription",
                        "transcription",
                        "clean_text_interpretive_word",
                        "type_of_inscription_clean",
                        "type_of_inscription_auto",
                        "type_of_monument_clean",
                        "province",
                        "province_label_clean",
                        "place",
                        "findspot_ancient_clean",
                        "findspot_modern_clean",
                        "status_notation",
                        "Latitude",
                        "Longitude")
# fix zadar lat long
LIRE_filtered$Longitude[LIRE_filtered$findspot_ancient_clean == 'Iader'
                        & LIRE_filtered$findspot_modern_clean == 'Zadar'] <- "15.223778"

LIRE_filtered$Latitude[LIRE_filtered$findspot_ancient_clean == 'Iader'
                       & LIRE_filtered$findspot_modern_clean == 'Zadar'] <- "44.115501"

#get only Dalmatia
LIRE_Dal <- filter(LIRE_filtered, province == "Dalmatia")
LIRE_Dal_clean <- filter(LIRE_filtered, province_label_clean == "Dalmatia")

write.csv(LIRE_Dal, file = "data/LIRE/LIRE_Dal.csv")
write.csv(LIRE_Dal_clean, file = "data/LIRE/LIRE_Dal_clean.csv")
