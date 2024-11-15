library(dplyr)
library(sqldf)

LIRE_Dalmatia <-
  read.csv("output_tables/corpus/undated/LIRE_Dalmatia_all_types.csv")

LIRE_Dal_all_dated <-
  read.csv("output_tables/corpus/dated/LIRE_Dalmatia_all_types_dated.csv")

LIRE_Dal <-
  read.csv("output_tables/corpus/undated/LIRE_Dalmatia.csv")

LIRE_Dal_dated <-
  read.csv("output_tables/corpus/dated/LIRE_Dalmatia_dated.csv")

LIRE_Dal_stela <-
  read.csv("output_tables/corpus/undated/LIRE_Dalmatia_stela.csv")

LIRE_Dal_Stela_dated <-
  read.csv("output_tables/corpus/dated/LIRE_Dalmatia_stela_dated.csv")

LIRE_Dal_epitaph <-
  read.csv("output_tables/corpus/undated/LIRE_Dalmatia_epitaph.csv")

LIRE_Dal_epitaph_dated <-
  read.csv("output_tables/corpus/dated/LIRE_Dalmatia_epitaph_dated.csv")

LIRE_Dal_votive <-
  read.csv("output_tables/corpus/undated/LIRE_Dalmatia_votive.csv")

LIRE_Dal_votive_dated <-
  read.csv("output_tables/corpus/dated/LIRE_Dalmatia_votive_dated.csv")

LIRE_Dal_altar <-
  read.csv("output_tables/corpus/undated/LIRE_Dalmatia_altar.csv")

LIRE_Dal_altar_dated <-
  read.csv("output_tables/corpus/dated/LIRE_Dalmatia_altar_dated.csv")

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