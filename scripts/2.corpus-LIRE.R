#Getting and cleaning data from LIRE
#started 5/9/2024
# edited 17/09/2024

library(dplyr)
library(sqldf)
library(arrow)
library(ggplot2)
library(sf)
library(ggrepel)
library("gridExtra")

#get all data + vocab
LIRE <-
  read_parquet("https://zenodo.org/records/8431452/files/LIRE_v3-0.parquet?download=1")

LIRE_dictionary <-
  read.csv("https://zenodo.org/records/8431452/files/LI_metadata.csv?download=1")

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
                        place, findspot_ancient_clean, findspot_modern_clean,
                        Longitude, Latitude 
                        from LIRE_Dal
                        order by 
                            Latitude desc, 
                            Longitude asc")
clean_LIRE_Dal_clean_places <- sqldf("Select Distinct 
                              place, 
                              findspot_ancient_clean, 
                              findspot_modern_clean,
                              Longitude, Latitude 
                              from LIRE_Dal_clean
                              order by 
                                Latitude desc, 
                                Longitude asc")

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

write.csv(LIRE_Dal_mil,
          file = "output_tables/corpus/LIRE_Dalmatia_all_monuments_military.csv")

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

write.csv(LIRE_Dal_mil_clean,
          file = "output_tables/corpus/LIRE_clean_Dalmatia_all_monuments_military.csv")

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

write.csv(LIRE_dated_mil,
          file = "output_tables/corpus/LIRE_Dalmatia_all_monuments_military_dated.csv")

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

write.csv(LIRE_clean_dated_mil,
          file = "output_tables/corpus/LIRE_clean_Dalmatia_all_monuments_military_dated.csv")

# filter types
##show types and count
LIRE_Dal_mil %>%
  count(type_of_monument_clean, type_of_inscription_clean)

LIRE_Dal_mil_clean %>%
  count(type_of_monument_clean, type_of_inscription_clean)

LIRE_dated_mil %>%
  count(type_of_monument_clean, type_of_inscription_clean)

LIRE_clean_dated_mil %>%
  count(type_of_monument_clean, type_of_inscription_clean)

## remove unwanted monument/inscription types

LIRE_Dal_corpus <- LIRE_Dal_mil %>%
  filter(!type_of_monument_clean %in% c("architectural member",
                                        "mile-/leaguestone",
                                        "tile",
                                        "instrumentum domesticum",
                                        "table",
                                        "instrumentum militare")
        & !type_of_inscription_clean %in% c("building/dedicatory inscription",
                                            "honorific inscription",
                                            "boundary inscription",
                                            "mile-/leaguestone",
                                            "military diploma",
                                            "building/dedicatory inscription",
                                            "list"))

LIRE_Dal_corpus_clean <- LIRE_Dal_mil_clean %>%
  filter(!type_of_monument_clean %in% c("architectural member",
                                        "mile-/leaguestone",
                                        "tile",
                                        "instrumentum domesticum",
                                        "table",
                                        "instrumentum militare")
         & !type_of_inscription_clean %in% c("architectural member",
                                             "remove building/dedicatory inscription", 
                                             "honorific inscription",
                                             "boundary inscription",
                                             "mile-/leaguestone",
                                             "military diploma",
                                             "building/dedicatory inscription",
                                             "list"))

LIRE_dated_corpus <- LIRE_dated_mil %>%
  filter(!type_of_monument_clean %in% c("architectural member",
                                        "mile-/leaguestone",
                                        "tile",
                                        "instrumentum domesticum",
                                        "table",
                                        "instrumentum militare")
        & !type_of_inscription_clean %in% c("building/dedicatory inscription",
                                            "honorific inscription",
                                            "boundary inscription",
                                            "mile-/leaguestone",
                                            "military diploma",
                                            "building/dedicatory inscription",
                                            "list"))

LIRE_clean_dated_corpus <- LIRE_clean_dated_mil %>%
  filter(!type_of_monument_clean %in% c("architectural member",
                                        "mile-/leaguestone",
                                        "tile",
                                        "instrumentum domesticum",
                                        "table",
                                        "instrumentum militare")
         & !type_of_inscription_clean %in% c("architectural member",
                                             "remove building/dedicatory inscription", 
                                             "honorific inscription",
                                             "boundary inscription",
                                             "mile-/leaguestone",
                                             "military diploma",
                                             "building/dedicatory inscription",
                                             "list"))

#check data
LIRE_Dal_corpus %>%
  count(type_of_monument_clean, type_of_inscription_clean)

LIRE_Dal_corpus_clean %>%
  count(type_of_monument_clean, type_of_inscription_clean)

LIRE_dated_corpus %>%
  count(type_of_monument_clean, type_of_inscription_clean)

LIRE_clean_dated_corpus %>%
  count(type_of_monument_clean, type_of_inscription_clean)

#save data
write.csv(LIRE_Dal_corpus,
          file = "output_tables/corpus/LIRE_thesis_corpus.csv")
write.csv(LIRE_Dal_corpus_clean,
          file = "output_tables/corpus/LIRE_clean_thesis_corpus.csv")
write.csv(LIRE_dated_corpus,
          file = "output_tables/corpus/LIRE_thesis_corpus_dated.csv")
write.csv(LIRE_clean_dated_corpus,
          file = "output_tables/corpus/LIRE_clean_thesis_corpus_dated.csv")

#add value for count
LIRE_Dal_corpus$count<- 1
LIRE_Dal_corpus_clean$count<- 1
LIRE_dated_corpus$count<- 1
LIRE_clean_dated_corpus$count<- 1

# do same for all Dal
LIRE_all_Dal <- LIRE_Dal %>%
  filter(!type_of_monument_clean %in% c("architectural member",
                                        "mile-/leaguestone", 
                                        "tile", 
                                        "instrumentum domesticum",
                                        "table",
                                        "instrumentum militare") 
         & !type_of_inscription_clean %in% c("architectural member",
                                             "remove building/dedicatory inscription", 
                                             "honorific inscription", 
                                             "boundary inscription", 
                                             "mile-/leaguestone", 
                                             "military diploma",
                                             "building/dedicatory inscription",
                                             "list"))

LIRE_all_Dal_clean <- LIRE_Dal_clean %>%
  filter(!type_of_monument_clean %in% c("architectural member",
                                        "mile-/leaguestone", 
                                        "tile", 
                                        "instrumentum domesticum",
                                        "table",
                                        "instrumentum militare") 
         & !type_of_inscription_clean %in% c("architectural member",
                                             "remove building/dedicatory inscription", 
                                             "honorific inscription", 
                                             "boundary inscription", 
                                             "mile-/leaguestone", 
                                             "military diploma",
                                             "building/dedicatory inscription",
                                             "list"))

LIRE_all_Dal_dated <- LIRE_Dal %>%
  filter(!type_of_monument_clean %in% c("architectural member",
                                        "mile-/leaguestone", 
                                        "tile", 
                                        "instrumentum domesticum",
                                        "table",
                                        "instrumentum militare") 
         & !type_of_inscription_clean %in% c("architectural member",
                                             "remove building/dedicatory inscription", 
                                             "honorific inscription", 
                                             "boundary inscription", 
                                             "mile-/leaguestone", 
                                             "military diploma",
                                             "building/dedicatory inscription",
                                             "list") 
         & not_before %in% (-30:191) & not_after %in% (1:200)) %>%
  arrange(not_after, not_before)

LIRE_all_Dal_clean_dated <- LIRE_Dal_clean %>%
  filter(!type_of_monument_clean %in% c("architectural member",
                                        "mile-/leaguestone", 
                                        "tile", 
                                        "instrumentum domesticum",
                                        "table",
                                        "instrumentum militare") 
         & !type_of_inscription_clean %in% c("architectural member",
                                             "remove building/dedicatory inscription", 
                                             "honorific inscription", 
                                             "boundary inscription", 
                                             "mile-/leaguestone", 
                                             "military diploma",
                                             "building/dedicatory inscription",
                                             "list") 
         & not_before %in% (-30:191) & not_after %in% (1:200)) %>%
  arrange(not_after, not_before)

#save data
write.csv(LIRE_all_Dal,
          file = "output_tables/corpus/LIRE_thesis_Dalmatia.csv")
write.csv(LIRE_all_Dal_clean,
          file = "output_tables/corpus/LIRE_clean_thesis_Dalmatia.csv")
write.csv(LIRE_all_Dal_dated,
          file = "output_tables/corpus/LIRE_thesis_Dalmatia_dated.csv")
write.csv(LIRE_all_Dal_clean_dated,
          file = "output_tables/corpus/LIRE_clean_thesis_Dalmatia_dated.csv")

LIRE_all_Dal$count<- 1
LIRE_all_Dal_clean$count<- 1
LIRE_all_Dal_dated$count<- 1
LIRE_all_Dal_clean_dated$count<- 1