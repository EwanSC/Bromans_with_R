# getting corpus from cleaned LIRE data
#started 5/9/2024
# edited 3/10/2024
library(dplyr)
library(sqldf)
library(arrow)

# get and read LIRE data (cleaned in /scripts/appendix.0-LIRE-data.R)
# and make NA values consistent
# data as of 3/10/2024
LIRE_Dal <-
  read.csv("data/LIRE/LIRE_Dal.csv", na = c("","NA","NULL",NULL))

LIRE_Dal_clean <-
  read.csv("data/LIRE/LIRE_Dal_clean.csv", na = c("","NA","NULL",NULL))

# function to remove unwanted monument/inscription types
clean_monument_types <- function(dataframe) {
  library(dplyr)
  clean_monument_types <- dataframe %>%
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
                                               "list",
                                               "seat inscription",
                                               "public legal inscription",
                                               "prayer",
                                               "seat inscription",
                                               "owner/artist inscription",
                                               "identification inscription",
                                               "assignation inscription",
                                               "acclamation",
                                               "calendar"))  
  return(clean_monument_types)
}

#function to put within date parameter (Claudians - Antonines)
within_date_range <- function(dataframe) {
  library(dplyr)
  within_date_range <- dataframe %>%
    filter(not_before %in% (-30:192), not_after %in% (1:200)) %>%
    arrange(not_after, not_before)
  return(within_date_range)
}

# run them on the data to make dated/undated base and cleaned base data
LIRE_Dal <- clean_monument_types(LIRE_Dal) # old LIRE_all_Dal
LIRE_Dal_clean <- clean_monument_types(LIRE_Dal_clean) #old LIRE_all_Dal_clean
LIRE_Dal_dated <- within_date_range(LIRE_Dal_clean) # old LIRE_all_Dal_dated
LIRE_Dal_dated_clean <- within_date_range(LIRE_Dal_clean) # old LIRE_all_Dal_clean_dated

write.csv(LIRE_Dal,
          file = "output_tables/corpus/LIRE_thesis_Dalmatia.csv")
write.csv(LIRE_Dal_clean,
          file = "output_tables/corpus/LIRE_clean_thesis_Dalmatia.csv")
write.csv(LIRE_Dal_dated,
          file = "output_tables/corpus/LIRE_thesis_Dalmatia_dated.csv")
write.csv(LIRE_Dal_dated_clean,
          file = "output_tables/corpus/LIRE_clean_thesis_Dalmatia_dated.csv")

LIRE_Dal$count<- 1
LIRE_Dal_clean$count<- 1
LIRE_Dal_dated$count<- 1
LIRE_Dal_dated_clean$count<- 1

## now count monument types
LIRE_Dal_types <- LIRE_Dal %>% 
  count(type_of_monument_clean, type_of_inscription_clean)
LIRE_Dal_clean_types <- LIRE_Dal_clean %>% 
  count(type_of_monument_clean, type_of_inscription_clean)
LIRE_Dal_dated_types <- LIRE_Dal_dated %>% 
  count(type_of_monument_clean, type_of_inscription_clean)
LIRE_Dal_dated_clean_types <- LIRE_Dal_dated_clean %>% 
  count(type_of_monument_clean, type_of_inscription_clean)

#make function for military (some based on place, others just key words and tags)
## with places and key words
load_military_terms_and_sites <- function(dataframe) {
  library(sqldf)
  library(dplyr)
  loaded_military_terms_and_sites <- sqldf("Select * from dataframe
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
  return(loaded_military_terms_and_sites)
}

##with just key words
load_military_terms <- function(dataframe) {
  library(sqldf)
  library(dplyr)
  load_military_terms <- sqldf("Select * from dataframe
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
                    ")
  return(load_military_terms)
}

#now run functions to make military corpus (unclean, clean, dated, key words and place, no places)
LIRE_Dal_corpus <- load_military_terms_and_sites(LIRE_Dal)

LIRE_Dal_corpus_no_place_filtering <- load_military_terms(LIRE_Dal)

LIRE_Dal_corpus_clean <- load_military_terms_and_sites(LIRE_Dal_clean)

LIRE_Dal_corpus_no_place_filtering_clean <- load_military_terms(LIRE_Dal_clean)

##now dated
LIRE_dated_corpus <- load_military_terms_and_sites(LIRE_Dal_dated)

LIRE_dated_corpus_no_place_filtering <- load_military_terms(LIRE_Dal_dated)

LIRE_dated_corpus_clean <- load_military_terms_and_sites(LIRE_Dal_dated_clean)

LIRE_dated_corpus_no_place_filtering_clean <- load_military_terms(LIRE_Dal_dated_clean)

## now count monument types
LIRE_Dal_corpus_types <- LIRE_Dal_corpus %>% 
  count(type_of_monument_clean, type_of_inscription_clean)
IRE_Dal_corpus_no_place_filtering_types <- LIRE_Dal_corpus_no_place_filtering %>% 
  count(type_of_monument_clean, type_of_inscription_clean)
LIRE_Dal_corpus_clean_types <- LIRE_Dal_corpus_clean %>% 
  count(type_of_monument_clean, type_of_inscription_clean)
LIRE_Dal_corpus_no_place_filtering_clean_types <- LIRE_Dal_corpus_no_place_filtering_clean %>% 
  count(type_of_monument_clean, type_of_inscription_clean)
LIRE_dated_corpus_types <- LIRE_dated_corpus %>% 
  count(type_of_monument_clean, type_of_inscription_clean)
LIRE_dated_corpus_no_place_filtering_types <- LIRE_dated_corpus_no_place_filtering %>% 
  count(type_of_monument_clean, type_of_inscription_clean)
LIRE_dated_corpus_clean_types <- LIRE_dated_corpus_clean %>% 
  count(type_of_monument_clean, type_of_inscription_clean)
LIRE_dated_corpus_no_place_filtering_clean_types <- LIRE_dated_corpus_no_place_filtering_clean %>% 
  count(type_of_monument_clean, type_of_inscription_clean)

# saving data
write.csv(LIRE_Dal_corpus,
          file = "output_tables/corpus/LIRE_thesis_corpus.csv")
write.csv(LIRE_Dal_corpus_no_place_filtering,
          file = "output_tables/corpus/LIRE_thesis_corpus_no_place_filter.csv")
write.csv(LIRE_Dal_corpus_clean,
          file = "output_tables/corpus/LIRE_clean_thesis_corpus.csv")
write.csv(LIRE_Dal_corpus_no_place_filtering_clean,
          file = "output_tables/corpus/LIRE_clean_thesis_corpus_no_place_filter.csv")
write.csv(LIRE_dated_corpus,
          file = "output_tables/corpus/LIRE_thesis_corpus_dated.csv")
write.csv(LIRE_dated_corpus_no_place_filtering,
          file = "output_tables/corpus/LIRE_thesis_corpus_dated_no_place_filter.csv")
write.csv(LIRE_dated_corpus_clean,
          file = "output_tables/corpus/LIRE_clean_thesis_corpus_dated.csv")
write.csv(LIRE_dated_corpus_no_place_filtering_clean,
          file = "output_tables/corpus/LIRE_clean_thesis_corpus_dated_no_place_filter.csv")

#add value for count
LIRE_Dal_corpus$count<- 1
LIRE_Dal_corpus_no_place_filtering$count<- 1
LIRE_Dal_corpus_clean$count<- 1
LIRE_Dal_corpus_no_place_filtering_clean$count<- 1
LIRE_dated_corpus$count<- 1
LIRE_dated_corpus_no_place_filtering$count<- 1
LIRE_dated_corpus_clean$count<- 1
LIRE_dated_corpus_no_place_filtering_clean$count<- 1