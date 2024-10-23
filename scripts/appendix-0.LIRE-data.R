# appendix-LIRE-data
# started 19/9/2024
# last edited 10/10/2024
# last ran 23/10/2024
library(dplyr)
library(sqldf)
library(arrow)
library(datplot)
library(ggplot2)

LIRE <-
  read_parquet("https://zenodo.org/records/8431452/files/LIRE_v3-0.parquet?download=1")

#filter columns
LIRE_filtered <- select(LIRE,
                        "LIST-ID",
                        "EDCS-ID",
                        "EDH-ID",
                        "not_before",
                        "not_after",
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
LIRE_Dal_EDCS <- filter(LIRE_filtered, province == "Dalmatia")
LIRE_Dal_EDH <- filter(LIRE_filtered, province_label_clean == "Dalmatia")
LIRE_Dalmatia <- filter(LIRE_filtered, province_label_clean == "Dalmatia" | province == "Dalmatia")

write.csv(LIRE_Dalmatia, file = "data/LIRE/LIRE_Dalmatia.csv")
write.csv(LIRE_Dal_EDCS, file = "data/LIRE/LIRE_Dal_EDCS.csv")
write.csv(LIRE_Dal_EDH, file = "data/LIRE/LIRE_Dal_EDH.csv")
