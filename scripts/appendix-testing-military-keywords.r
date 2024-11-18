library(dplyr)
library(sqldf)
library(arsenal)

LIRE_Dal_all <-
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

# test old 
OLD_LIRE_Dal_corpus_all <- load_military_terms(LIRE_Dal_all)
OLD_LIRE_Dal_corpus_all_places <- load_military_terms_and_sites(LIRE_Dal_all)
OLD_LIRE_Dal_corpus <- load_military_terms(LIRE_Dal)
OLD_LIRE_Dal_corpus_places <- load_military_terms_and_sites(LIRE_Dal)

write.csv(OLD_LIRE_Dal_corpus,
          file = "output_tables/appendices/OLD_LIRE_Dal_corpus.csv")

# new
new_military_terms <- function(dataframe) {
  library(sqldf)
  library(dplyr)
  new_military_terms <- sqldf("Select * from dataframe
  WHERE clean_text_interpretive_word 
                    LIKE '%actari%'
                  or clean_text_interpretive_word
                    LIKE '%adiutor%'
                  or clean_text_interpretive_word
                    LIKE '%aerarii%'
                  or clean_text_interpretive_word
                    LIKE '%ala%'
                  or clean_text_interpretive_word
                    LIKE '%aquilices%'
                  or clean_text_interpretive_word
                    LIKE '%aquilifer%'
                  or clean_text_interpretive_word
                    LIKE '%arcuari%'
                  or clean_text_interpretive_word
                    LIKE '%armatura%'
                  or clean_text_interpretive_word
                    LIKE '%armorum%'
                  or clean_text_interpretive_word
                    LIKE '%artifices%'
                  or clean_text_interpretive_word
                    LIKE '%artifix%'
                  or clean_text_interpretive_word
                    LIKE '%baiulus%'
                  or clean_text_interpretive_word
                    LIKE '%ballistrari%'
                  or clean_text_interpretive_word
                    LIKE '%beneficiari%'
                  or clean_text_interpretive_word
                    LIKE '%bucinator%'
                  or clean_text_interpretive_word
                    LIKE '%bucularum%'
                  or clean_text_interpretive_word
                    LIKE '%capsari%'
                  or clean_text_interpretive_word
                    LIKE '%carpentari%'
                  or clean_text_interpretive_word
                    LIKE '%centurio%'
                  or clean_text_interpretive_word
                    LIKE '%centurio%'
                  or clean_text_interpretive_word
                    LIKE '%claviculari%'
                  or clean_text_interpretive_word
                    LIKE '%cohor%'
                  or clean_text_interpretive_word
                    LIKE '%commentariensis%'
                  or clean_text_interpretive_word
                    LIKE '%cornicular%'
                  or clean_text_interpretive_word
                    LIKE '%cornicen%'
                  or clean_text_interpretive_word
                    LIKE '%armorum%'
                  or clean_text_interpretive_word
                    LIKE '%custos basilicae%'
                  or clean_text_interpretive_word
                    LIKE '%decurio%'
                  or clean_text_interpretive_word
                    LIKE '%discens%'
                  or clean_text_interpretive_word
                    LIKE '%discentes%'
                  or clean_text_interpretive_word
                    LIKE '%doctorfabrum%'
                  or clean_text_interpretive_word
                    LIKE '%duplicari%'
                  or clean_text_interpretive_word
                    LIKE '%duplari%'
                  or clean_text_interpretive_word
                    LIKE '%eques%'
                  or clean_text_interpretive_word
                    LIKE '%equit%'
                  or clean_text_interpretive_word
                    LIKE '%evocat%'
                  or clean_text_interpretive_word
                    LIKE '%exactus%'
                  or clean_text_interpretive_word
                    LIKE '%exceptor%'
                  or clean_text_interpretive_word
                    LIKE '%fabri%'
                  or clean_text_interpretive_word
                    LIKE '%ferrari%'
                  or clean_text_interpretive_word
                    LIKE '%frumentari%'
                  or clean_text_interpretive_word
                    LIKE '%gubernator%'
                  or clean_text_interpretive_word
                    LIKE '%hastiliari%'
                  or clean_text_interpretive_word
                    LIKE '%horologiarius%'
                  or clean_text_interpretive_word
                    LIKE '%imaginifer%'
                  or clean_text_interpretive_word
                    LIKE '%immunis%'
                  or clean_text_interpretive_word
                    LIKE '%immun% cerari%'
                  or clean_text_interpretive_word
                    LIKE '%interpres%'
                  or clean_text_interpretive_word
                    LIKE '%lapidarii%'
                  or clean_text_interpretive_word
                    LIKE '%legio%'
                  or clean_text_interpretive_word
                    LIKE '%librari%'
                  or clean_text_interpretive_word
                    LIKE '%librator%'
                  or clean_text_interpretive_word
                    LIKE '%marsus%'
                  or clean_text_interpretive_word
                    LIKE '%medic%'
                  or clean_text_interpretive_word
                    LIKE '%mensor%'
                  or clean_text_interpretive_word
                    LIKE '%mensor%'
                  or clean_text_interpretive_word
                    LIKE '%miles%'
                  or clean_text_interpretive_word
                    LIKE '%milit%'
                  or clean_text_interpretive_word
                    LIKE '%munifex%'
                  or clean_text_interpretive_word
                    LIKE '%munific%'
                  or clean_text_interpretive_word
                    LIKE '%optio%'
                  or clean_text_interpretive_word
                    LIKE '%plumbari%'
                  or clean_text_interpretive_word
                    LIKE '%pollio%'
                  or clean_text_interpretive_word
                    LIKE '%praeco%'
                  or clean_text_interpretive_word
                    LIKE '%praefect%'
                  or clean_text_interpretive_word
                    LIKE '%primi pili%'
                  or clean_text_interpretive_word
                    LIKE '%primo pilo%'
                  or clean_text_interpretive_word
                    LIKE '%primus pilus%'
                  or clean_text_interpretive_word
                    LIKE '%primipilus%'
                  or clean_text_interpretive_word
                    LIKE '%principalis%'
                  or clean_text_interpretive_word
                    LIKE '%principales%'
                  or clean_text_interpretive_word
                    LIKE '%quaestionari%'
                  or clean_text_interpretive_word
                    LIKE '%sagittari%'
                  or clean_text_interpretive_word
                    LIKE '%scandulari%'
                  or clean_text_interpretive_word
                    LIKE '%secutor%'
                  or clean_text_interpretive_word
                    LIKE '%sesquiplari%'
                  or clean_text_interpretive_word
                    LIKE '%signifer%'
                  or clean_text_interpretive_word
                    LIKE '%singulari%'
                  or clean_text_interpretive_word
                    LIKE '%speculari%'
                  or clean_text_interpretive_word
                    LIKE '%speculator%'
                  or clean_text_interpretive_word
                    LIKE '%stator%'
                  or clean_text_interpretive_word
                    LIKE '%strator%'
                  or clean_text_interpretive_word
                    LIKE '%tablifer%'
                  or clean_text_interpretive_word
                    LIKE '%tesserari%'
                  or clean_text_interpretive_word
                    LIKE '%tubari%'
                  or clean_text_interpretive_word
                    LIKE '%tubican%'
                  or clean_text_interpretive_word
                    LIKE '%turarium%'
                  or clean_text_interpretive_word
                    LIKE '%turma%'
                  or clean_text_interpretive_word
                    LIKE '%venator%'
                  or clean_text_interpretive_word
                    LIKE '%veteran%'
                  or clean_text_interpretive_word
                    LIKE '%veterinari%'
                  or clean_text_interpretive_word
                    LIKE '%vexillari%'
                  or clean_text_interpretive_word
                    LIKE '%victimari%'
                  or status_notation
                    LIKE '%milites%'
                  ")
  return(new_military_terms)
}

new_military_terms_and_sites <- function(dataframe) {
  library(sqldf)
  library(dplyr)
  new_military_terms_and_sites <- sqldf("Select * from dataframe
  WHERE clean_text_interpretive_word 
                    LIKE '%actari%'
                  or clean_text_interpretive_word
                    LIKE '%adiutor%'
                  or clean_text_interpretive_word
                    LIKE '%aerarii%'
                  or clean_text_interpretive_word
                    LIKE '%ala%'
                  or clean_text_interpretive_word
                    LIKE '%aquilices%'
                  or clean_text_interpretive_word
                    LIKE '%aquilifer%'
                  or clean_text_interpretive_word
                    LIKE '%arcuari%'
                  or clean_text_interpretive_word
                    LIKE '%armatura%'
                  or clean_text_interpretive_word
                    LIKE '%armorum%'
                  or clean_text_interpretive_word
                    LIKE '%artifices%'
                  or clean_text_interpretive_word
                    LIKE '%artifix%'
                  or clean_text_interpretive_word
                    LIKE '%baiulus%'
                  or clean_text_interpretive_word
                    LIKE '%ballistrari%'
                  or clean_text_interpretive_word
                    LIKE '%beneficiari%'
                  or clean_text_interpretive_word
                    LIKE '%bucinator%'
                  or clean_text_interpretive_word
                    LIKE '%bucularum%'
                  or clean_text_interpretive_word
                    LIKE '%capsari%'
                  or clean_text_interpretive_word
                    LIKE '%carpentari%'
                  or clean_text_interpretive_word
                    LIKE '%centurio%'
                  or clean_text_interpretive_word
                    LIKE '%centurio%'
                  or clean_text_interpretive_word
                    LIKE '%claviculari%'
                  or clean_text_interpretive_word
                    LIKE '%cohor%'
                  or clean_text_interpretive_word
                    LIKE '%commentariensis%'
                  or clean_text_interpretive_word
                    LIKE '%cornicular%'
                  or clean_text_interpretive_word
                    LIKE '%cornicen%'
                  or clean_text_interpretive_word
                    LIKE '%armorum%'
                  or clean_text_interpretive_word
                    LIKE '%custos basilicae%'
                  or clean_text_interpretive_word
                    LIKE '%decurio%'
                  or clean_text_interpretive_word
                    LIKE '%discens%'
                  or clean_text_interpretive_word
                    LIKE '%discentes%'
                  or clean_text_interpretive_word
                    LIKE '%doctorfabrum%'
                  or clean_text_interpretive_word
                    LIKE '%duplicari%'
                  or clean_text_interpretive_word
                    LIKE '%duplari%'
                  or clean_text_interpretive_word
                    LIKE '%eques%'
                  or clean_text_interpretive_word
                    LIKE '%equit%'
                  or clean_text_interpretive_word
                    LIKE '%evocat%'
                  or clean_text_interpretive_word
                    LIKE '%exactus%'
                  or clean_text_interpretive_word
                    LIKE '%exceptor%'
                  or clean_text_interpretive_word
                    LIKE '%fabri%'
                  or clean_text_interpretive_word
                    LIKE '%ferrari%'
                  or clean_text_interpretive_word
                    LIKE '%frumentari%'
                  or clean_text_interpretive_word
                    LIKE '%gubernator%'
                  or clean_text_interpretive_word
                    LIKE '%hastiliari%'
                  or clean_text_interpretive_word
                    LIKE '%horologiarius%'
                  or clean_text_interpretive_word
                    LIKE '%imaginifer%'
                  or clean_text_interpretive_word
                    LIKE '%immunis%'
                  or clean_text_interpretive_word
                    LIKE '%immun% cerari% '
                  or clean_text_interpretive_word
                    LIKE '%interpres%'
                  or clean_text_interpretive_word
                    LIKE '%lapidarii%'
                  or clean_text_interpretive_word
                    LIKE '%legio%'
                  or clean_text_interpretive_word
                    LIKE '%librari%'
                  or clean_text_interpretive_word
                    LIKE '%librator%'
                  or clean_text_interpretive_word
                    LIKE '%marsus%'
                  or clean_text_interpretive_word
                    LIKE '%medic%'
                  or clean_text_interpretive_word
                    LIKE '%mensor%'
                  or clean_text_interpretive_word
                    LIKE '%mensor%'
                  or clean_text_interpretive_word
                    LIKE '%miles%'
                  or clean_text_interpretive_word
                    LIKE '%milit%'
                  or clean_text_interpretive_word
                    LIKE '%munifex%'
                  or clean_text_interpretive_word
                    LIKE '%munific%'
                  or clean_text_interpretive_word
                    LIKE '%optio%'
                  or clean_text_interpretive_word
                    LIKE '%plumbari%'
                  or clean_text_interpretive_word
                    LIKE '%pollio%'
                  or clean_text_interpretive_word
                    LIKE '%praeco%'
                  or clean_text_interpretive_word
                    LIKE '%praefect%'
                  or clean_text_interpretive_word
                    LIKE '%primi pili%'
                  or clean_text_interpretive_word
                    LIKE '%primo pilo%'
                  or clean_text_interpretive_word
                    LIKE '%primus pilus%'
                  or clean_text_interpretive_word
                    LIKE '%primipilus%'
                  or clean_text_interpretive_word
                    LIKE '%principalis%'
                  or clean_text_interpretive_word
                    LIKE '%principales%'
                  or clean_text_interpretive_word
                    LIKE '%quaestionari%'
                  or clean_text_interpretive_word
                    LIKE '%sagittari%'
                  or clean_text_interpretive_word
                    LIKE '%scandulari%'
                  or clean_text_interpretive_word
                    LIKE '%secutor%'
                  or clean_text_interpretive_word
                    LIKE '%sesquiplari%'
                  or clean_text_interpretive_word
                    LIKE '%signifer%'
                  or clean_text_interpretive_word
                    LIKE '%singulari%'
                  or clean_text_interpretive_word
                    LIKE '%speculari%'
                  or clean_text_interpretive_word
                    LIKE '%speculator%'
                  or clean_text_interpretive_word
                    LIKE '%stator%'
                  or clean_text_interpretive_word
                    LIKE '%strator%'
                  or clean_text_interpretive_word
                    LIKE '%tablifer%'
                  or clean_text_interpretive_word
                    LIKE '%tesserari%'
                  or clean_text_interpretive_word
                    LIKE '%tubari%'
                  or clean_text_interpretive_word
                    LIKE '%tubican%'
                  or clean_text_interpretive_word
                    LIKE '%turarium%'
                  or clean_text_interpretive_word
                    LIKE '%turma%'
                  or clean_text_interpretive_word
                    LIKE '%venator%'
                  or clean_text_interpretive_word
                    LIKE '%veteran%'
                  or clean_text_interpretive_word
                    LIKE '%veterinari%'
                  or clean_text_interpretive_word
                    LIKE '%vexillari%'
                  or clean_text_interpretive_word
                    LIKE '%victimari%'
                  or status_notation
                    LIKE '%milites%'
                  or findspot_ancient_clean = 'Tilurium'
                  or findspot_ancient_clean = 'Burnum'
                  or findspot_ancient_clean = 'Andetrium'
                  or findspot_ancient_clean = 'Bigeste'
                  or findspot_modern_clean = 'Ljubuški'
                  ")
return(new_military_terms_and_sites)
}

# test new
NEW_LIRE_Dal_corpus_all <- new_military_terms(LIRE_Dal_all)
NEW_LIRE_Dal_corpus_all_places <- new_military_terms_and_sites(LIRE_Dal_all)
NEW_LIRE_Dal_corpus <- new_military_terms(LIRE_Dal)
NEW_LIRE_Dal_corpus_places <- new_military_terms_and_sites(LIRE_Dal)

comparedf(NEW_LIRE_Dal_corpus,OLD_LIRE_Dal_corpus)

write.csv(NEW_LIRE_Dal_corpus,
          file = "output_tables/appendices/NEW_LIRE_Dal_corpus.csv")

# manually after comparing, none of the new fields captured in 
# NEW_LIRE_CORPUS.csv are relevant see suppl/testing_military_data_1.xlsx
# now to remove irrelevant inscriptions

##with just key words
load_military <- function(dataframe) {
  library(sqldf)
  library(dplyr)
  load_military <- sqldf("Select * from dataframe
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
load_military <- load_military %>%
  filter(!LIST.ID %in% c("468295",
                    "468613",
                    "468717",
                    "468832",
                    "469155",
                    "469186",
                    "469226",
                    "469341",
                    "469464",
                    "469506",
                    "469916",
                    "470136",
                    "470434",
                    "470543",
                    "470704",
                    "470864",
                    "471007",
                    "471036",
                    "471038",
                    "471230",
                    "471402",
                    "471414",
                    "471496",
                    "471645",
                    "471678",
                    "471682",
                    "471759",
                    "471777",
                    "471800",
                    "471809",
                    "471996",
                    "472452",
                    "472453",
                    "472586",
                    "472928",
                    "472984",
                    "473108",
                    "473129",
                    "473194",
                    "473217",
                    "473422",
                    "473564",
                    "473959",
                    "474003",
                    "474473",
                    "474478",
                    "474519",
                    "474682",
                    "474858",
                    "474992",
                    "475226",
                    "475259",
                    "475320",
                    "475377",
                    "475378",
                    "500263"))
return(load_military)
}

load_military_with_sites <- function(dataframe) {
  library(sqldf)
  library(dplyr)
  load_military_with_sites <- sqldf("Select * from dataframe
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
load_military_with_sites <- load_military_with_sites %>%
  filter(!LIST.ID %in% c("468295",
                    "468613",
                    "468717",
                    "468832",
                    "469155",
                    "469186",
                    "469226",
                    "469341",
                    "469464",
                    "469506",
                    "469916",
                    "470136",
                    "470434",
                    "470543",
                    "470704",
                    "470864",
                    "471007",
                    "471036",
                    "471038",
                    "471230",
                    "471402",
                    "471414",
                    "471496",
                    "471645",
                    "471678",
                    "471682",
                    "471759",
                    "471777",
                    "471800",
                    "471809",
                    "471996",
                    "472452",
                    "472453",
                    "472586",
                    "472928",
                    "472984",
                    "473108",
                    "473129",
                    "473194",
                    "473217",
                    "473422",
                    "473564",
                    "473959",
                    "474003",
                    "474473",
                    "474478",
                    "474519",
                    "474682",
                    "474858",
                    "474992",
                    "475226",
                    "475259",
                    "475320",
                    "475377",
                    "475378",
                    "500263"))
return(load_military_with_sites)
}

LIRE_Dal_corpus_all <- load_military(LIRE_Dal_all)
LIRE_Dal_corpus_all_places <- load_military_with_sites(LIRE_Dal_all)
LIRE_Dal_corpus <- load_military(LIRE_Dal)
LIRE_Dal_corpus_places <- load_military_with_sites(LIRE_Dal)

write.csv(LIRE_Dal_corpus,
          file = "output_tables/appendices/LIRE_Dal_corpus.csv")